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

type EnumStringPickler<'Enum when 'Enum : struct
                              and 'Enum :> ValueType
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
            | JsonTag.Null -> Nullable()
            | _ -> pickler.UnPickle reader |> Nullable


let inline mkCollectionPickler<'Collection, 'T when 'Collection :> seq<'T>> (tpickler : JsonPickler<'T>) (ctor : ResizeArray<'T> -> 'Collection) =
    { new JsonPickler<'Collection> with
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
            ctor ra }

type JsonField<'Value> = KeyValuePair<string, 'Value>
let inline mkDictionaryPickler<'Dict, 'Value when 'Dict :> seq<JsonField<'Value>>> 
    (vpickler : JsonPickler<'Value>) (ctor : ResizeArray<JsonField<'Value>> -> 'Dict) =

    { new JsonPickler<'Dict> with
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
                
            ctor ra }

type IFieldPickler<'T> =
    abstract Pickle : JsonWriter -> 'T -> unit
    abstract UnPickle : JsonReader -> 'T -> 'T

let mkFieldPickler (resolver : IPicklerResolver) (shapeField : IShapeWriteMember<'T>) =
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

let mkFieldPicklers resolver (fields : seq<IShapeWriteMember<'T>>) =
    let fields = Seq.toArray fields
    fields |> Array.map (fun f -> f.Label, mkFieldPickler resolver f)

let inline mkRecordShapePickler<'TRecord> (resolver : IPicklerResolver) (ctor : unit -> 'TRecord) (fields : IShapeWriteMember<'TRecord>[]) =
    let labels, picklers = mkFieldPicklers resolver fields |> Array.unzip
    let index = BinSearch labels

    { new JsonPickler<'TRecord> with
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

            record }

// TODO : check what strings make valid json field names
let mkFSharpUnionPickler<'TUnion> (resolver : IPicklerResolver) (tagId : string option) (shape : ShapeFSharpUnion<'TUnion>) =
    let lookups, picklerss, tags = 
        shape.UnionCases 
        |> Array.map (fun c -> c.Fields |> Array.map (fun c -> c.Label) |> BinSearch, c.Fields |> Array.map (mkFieldPickler resolver), c.CaseInfo.Name)
        |> Array.unzip3

    let lookup = BinSearch tags
    let tagId = defaultArg tagId "__case"

    { new JsonPickler<'TUnion> with
        member __.Pickle writer union =
            writer.StartObject()
            let tag = shape.GetTag union
            writer.Key tagId
            writer.String tags.[tag]

            let picklers = picklerss.[tag]
            for p in picklers do p.Pickle writer union
            writer.EndObject()

        member __.UnPickle reader =
            let _ = reader.EnsureToken JsonTag.StartObject
            let mutable tok = reader.EnsureToken JsonTag.Key
            if tok.Value <> tagId then
                sprintf "JSON Union objects must begin with a '%s' field" tagId
                |> VardusiaException
                |> raise

            tok <- reader.EnsureToken JsonTag.String

            match lookup.TryFindIndex tok.Value with
            | tag when tag < 0 -> raise <| VardusiaException (sprintf "Unrecognized union case '%s'" tok.Value)
            | tag ->

            let lookup = lookups.[tag]
            let picklers = picklerss.[tag]
            let mutable record = shape.UnionCases.[tag].CreateUninitialized()

            tok <- reader.NextToken()
            while tok.Tag <> JsonTag.EndObject do
                let label = tok.AsKey()
                match lookup.TryFindIndex label with
                | i when i >= 0 -> record <- picklers.[i].UnPickle reader record
                | _ -> reader.ConsumeValue()

                tok <- reader.NextToken()

            record }