module internal Vardusia.GenericPicklers

open System
open System.Collections.Generic

open TypeShape
open TypeShape_Utils

type IsomorphismPickler<'T, 'S>(convert : 'T -> 'S, recover : 'S -> 'T, pickler : JsonPickler<'S>) =
    interface JsonPickler<'T> with
        member __.Pickle writer t = pickler.Pickle writer (convert t)
        member __.UnPickle reader = pickler.UnPickle reader |> recover

type FSharpOptionPickler<'T>(pickler : JsonPickler<'T>) =
    interface JsonPickler<'T option> with
        member __.Pickle writer tOpt =
            match tOpt with
            | None -> writer.Null()
            | Some t -> pickler.Pickle writer t

        member __.UnPickle reader =
            let tok = reader.PeekToken()
            match tok.Tag with
            | JsonTag.Null -> let _ = reader.NextToken() in None
            | _ -> pickler.UnPickle reader |> Some

type EnumIntPickler<'Enum, 'U when 'Enum : enum<'U>>(upickler : JsonPickler<'U>) =
    interface JsonPickler<'Enum> with
        member __.Pickle writer enum = 
            let u = LanguagePrimitives.EnumToValue enum
            upickler.Pickle writer u

        member __.UnPickle reader =
            let u = upickler.UnPickle reader
            LanguagePrimitives.EnumOfValue u

type EnumStringPickler<'Enum, 'U when 'Enum : struct
                                 and 'Enum :> ValueType
                                 and 'Enum : enum<'U>
                                 and 'Enum : (new : unit -> 'Enum)>() =

    interface JsonPickler<'Enum> with
        member __.Pickle writer enum =
            let str = enum.ToString()
            writer.WriteValue str

        member __.UnPickle reader =
            let tok = reader.NextToken()
            let str = tok.AsString()
            let mutable result = Unchecked.defaultof<'Enum>
            if Enum.TryParse(str, true, &result) then result
            else unexpectedToken tok

type NullablePickler<'T when 'T : struct 
                         and 'T :> ValueType
                         and 'T : (new : unit -> 'T)> (pickler : JsonPickler<'T>) =

    interface JsonPickler<Nullable<'T>> with
        member __.Pickle writer t =
            if t.HasValue then pickler.Pickle writer t.Value
            else writer.Null()

        member __.UnPickle reader =
            let tok = reader.PeekToken()
            match tok.Tag with
            | JsonTag.Null -> let _ = reader.NextToken() in Nullable()
            | _ -> pickler.UnPickle reader |> Nullable


type CollectionPickler<'Collection, 'T when 'Collection :> seq<'T>> (ctor : ResizeArray<'T> -> 'Collection, tpickler : JsonPickler<'T>) =
    interface JsonPickler<'Collection> with
        member __.Pickle writer ts =
            writer.StartArray()
            for t in ts do tpickler.Pickle writer t
            writer.EndArray()

        member __.UnPickle reader =
            let _ = reader.EnsureToken JsonTag.StartArray
            let ra = ResizeArray<'T>()
            let mutable tok = reader.PeekToken()
            while tok.Tag <> JsonTag.EndArray do
                let t = tpickler.UnPickle reader
                ra.Add t
                tok <- reader.PeekToken() 

            let _ = reader.NextToken()
            ctor ra

let mkResizeArrayPickler t = CollectionPickler<ResizeArray<'T>, 'T>(id, t)
let mkArrayPickler t = CollectionPickler<'T [], 'T>((fun ra -> ra.ToArray()), t)
let mkListPickler t = CollectionPickler<'T list, 'T>(List.ofSeq, t)
let mkSetPickler t = CollectionPickler<Set<'T>, 'T>(Set.ofSeq, t)

type JsonField<'Value> = KeyValuePair<string, 'Value>
type DictionaryPickler<'Dict, 'Value when 'Dict :> seq<JsonField<'Value>>> (ctor : ResizeArray<JsonField<'Value>> -> 'Dict, vpickler : JsonPickler<'Value>) =
    interface JsonPickler<'Dict> with
        member __.Pickle writer map =
            writer.StartObject()
            for kv in map do
                writer.Key kv.Key
                vpickler.Pickle writer kv.Value

            writer.EndObject()

        member __.UnPickle reader =
            let _ = reader.EnsureToken JsonTag.StartObject
            let ra = ResizeArray<JsonField<'Value>>()
            let mutable tok = reader.NextToken()
            while tok.Tag <> JsonTag.EndObject do
                let key = tok.AsKey()
                let value = vpickler.UnPickle reader
                ra.Add(KeyValuePair(key, value))
                tok <- reader.NextToken() 
                
            ctor ra

let mkMapPickler vpickler =
    let mkMap (kvs : ResizeArray<KeyValuePair<_,_>>) =
        kvs |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq
    DictionaryPickler<Map<string,'v>, 'v>(mkMap, vpickler)

let mkDictPickler vpickler =
    let mkDict (kvs : ResizeArray<KeyValuePair<_,_>>) =
        let d = new Dictionary<string, 'v>()
        for kv in kvs do d.[kv.Key] <- kv.Value
        d

    DictionaryPickler<Dictionary<string, 'v>, 'v>(mkDict, vpickler)

type private IFieldPickler<'T> =
    abstract Pickle : JsonWriter -> 'T -> unit
    abstract UnPickle : JsonReader -> 'T -> 'T

let private mkFieldPickler (resolver : IPicklerResolver) (shapeField : IShapeWriteMember<'T>) =
    shapeField.Accept { new IWriteMemberVisitor<'T, IFieldPickler<'T>> with
        member __.Visit(shape : ShapeWriteMember<'T, 'Field>) =
            let fp = resolver.Resolve<'Field>()
            let label = shape.Label
            { new IFieldPickler<'T> with
                member __.Pickle writer t = 
                    let field = shape.Project t
                    writer.Key label
                    fp.Pickle writer field

                member __.UnPickle reader t =
                    let field = fp.UnPickle reader
                    shape.Inject t field }
    }

let private mkFieldPicklers resolver (fields : seq<IShapeWriteMember<'T>>) =
    let fields = Seq.toArray fields
    fields |> Array.map (fun f -> f.Label, mkFieldPickler resolver f)


type RecordPickler<'TRecord> (resolver : IPicklerResolver, ctor : unit -> 'TRecord, fields : IShapeWriteMember<'TRecord>[]) =
    let labels, picklers = mkFieldPicklers resolver fields |> Array.unzip
    let index = BinSearch labels

    interface JsonPickler<'TRecord> with
        member __.Pickle writer record =
            writer.StartObject()
            for p in picklers do p.Pickle writer record
            writer.EndObject()

        member __.UnPickle reader =
            let _ = reader.EnsureToken JsonTag.StartObject
            let mutable record = ctor()
            let mutable tok = reader.NextToken()
            while tok.Tag <> JsonTag.EndObject do
                let label = tok.AsKey()
                match index.TryFindIndex label with
                | i when i >= 0 -> record <- picklers.[i].UnPickle reader record
                | _ -> reader.ConsumeValue()

                tok <- reader.NextToken()

            record

let [<Literal>] private unionCaseKey = "case"
let [<Literal>] private unionFieldsKey = "fields"

type FSharpUnionPickler<'TUnion> (resolver : IPicklerResolver, shape : ShapeFSharpUnion<'TUnion>) =
    let labelss, picklerss, tags = 
        shape.UnionCases 
        |> Array.map (fun c -> 
            let labels = c.Fields |> Array.map (fun c -> c.Label) |> BinSearch
            let picklers = c.Fields |> Array.map (mkFieldPickler resolver)
            labels, picklers, c.CaseInfo.Name)
        |> Array.unzip3

    let caseIndex = BinSearch tags

    interface JsonPickler<'TUnion> with
        member __.Pickle writer union =
            writer.StartObject()
            let tag = shape.GetTag union
            let picklers = picklerss.[tag]
            writer.Key unionCaseKey
            writer.String tags.[tag]

            if picklers.Length > 0 then
                writer.Key unionFieldsKey
                writer.StartObject()
                for p in picklers do p.Pickle writer union
                writer.EndObject()

            writer.EndObject()

        member __.UnPickle reader =
            let _ = reader.EnsureToken JsonTag.StartObject
            let mutable tok = reader.EnsureToken JsonTag.Key
            if tok.Value <> unionCaseKey then
                sprintf "JSON Union objects must begin with a '%s' field" unionCaseKey
                |> VardusiaException
                |> raise

            tok <- reader.EnsureToken JsonTag.String

            match caseIndex.TryFindIndex tok.Value with
            | tag when tag < 0 -> 
                sprintf "Unrecognized union case '%s' at position %d" tok.Value tok.Position
                |> VardusiaException
                |> raise

            | tag ->

            let labels = labelss.[tag]
            let picklers = picklerss.[tag]
            let mutable record = shape.UnionCases.[tag].CreateUninitialized()
            
            if picklers.Length = 0 then record else

            tok <- reader.EnsureToken JsonTag.Key
            if tok.Value <> unionFieldsKey then
                sprintf "Nontrivial JSON Union objects must specify a '%s' object" unionFieldsKey
                |> VardusiaException
                |> raise

            let _ = reader.EnsureToken JsonTag.StartObject
            tok <- reader.NextToken()
            while tok.Tag <> JsonTag.EndObject do
                let label = tok.AsKey()
                match labels.TryFindIndex label with
                | i when i >= 0 -> record <- picklers.[i].UnPickle reader record
                | _ -> reader.ConsumeValue()

                tok <- reader.NextToken()

            record