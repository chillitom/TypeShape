module internal Vardusia.GenericPicklers

open System
open System.Collections.Generic
open System.Reflection

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
            | None -> writer.WriteNull()
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
            else writer.WriteNull()

        member __.UnPickle reader =
            let tok = reader.PeekToken()
            match tok.Tag with
            | JsonTag.Null -> let _ = reader.NextToken() in Nullable()
            | _ -> pickler.UnPickle reader |> Nullable


type CollectionPickler<'Collection, 'T when 'Collection :> seq<'T>> (ctor : ResizeArray<'T> -> 'Collection, tpickler : JsonPickler<'T>) =
    interface JsonPickler<'Collection> with
        member __.Pickle writer ts =
            writer.WriteStartArray()
            for t in ts do tpickler.Pickle writer t
            writer.WriteEndArray()

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
            writer.WriteStartObject()
            for kv in map do
                writer.WriteKey kv.Key
                vpickler.Pickle writer kv.Value

            writer.WriteEndObject()

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

let extractPicklerFromType<'Target> (resolver : IPicklerResolver) (source : Type) =
    match getParameterlessCtor source with
    | null ->
        sprintf "Pickler attribute type '%O' lacking a parameterless constructor." source
        |> VardusiaException
        |> raise

    | ctor ->
        match ctor.Invoke [||] with
        | :? JsonPickler<'Target> as p -> p
        | :? IPicklerFactory<'Target> as f -> f.Create resolver
        | _ ->
            sprintf "Pickler attribute type '%O' defines neither a pickler nor a pickler factory for type '%O'"
                                            source                                                  typeof<'Target>
            |> VardusiaException
            |> raise

let tryExtractPicklerFromToJsonMethods<'T> (resolver : IPicklerResolver) : JsonPickler<'T> option =
    let methodBindings = BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic
    let toMethodName = "ToJson"
    let fromMethodName = "FromJson"

    match typeof<'T>.GetMethod(toMethodName, methodBindings, null, [|typeof<'T>|], [||]) with
    | null -> None
    | fromMethodInfo ->
        let targetType = fromMethodInfo.ReturnType
        match typeof<'T>.GetMethod(fromMethodName, methodBindings, null, [|targetType|], [||]) with
        | null -> None
        | toMethodInfo when toMethodInfo.ReturnType <> typeof<'T> -> None
        | toMethodInfo ->
            TypeShape.Create(targetType).Accept {
                new ITypeShapeVisitor<JsonPickler<'T>> with
                    member __.Visit<'Target>() =
                        let targetPickler = resolver.Resolve<'Target>()
                        let fromMethod = wrapDelegate<Func<'T,'Target>> fromMethodInfo
                        let toMethod = wrapDelegate<Func<'Target, 'T>> toMethodInfo
                        { new JsonPickler<'T> with
                            member __.Pickle writer t = 
                                let target = fromMethod.Invoke t
                                targetPickler.Pickle writer target

                            member __.UnPickle reader =
                                let target = targetPickler.UnPickle reader
                                toMethod.Invoke target } }
            |> Some

type private IFieldPickler<'T> =
    abstract IsRequired : bool
    abstract Label : string
    abstract Pickle : JsonWriter -> 'T -> unit
    abstract UnPickle : JsonReader -> 'T -> 'T

let private mkFieldPickler (resolver : IPicklerResolver) isRequired (shapeField : IShapeWriteMember<'T>) =
    shapeField.Accept { new IWriteMemberVisitor<'T, IFieldPickler<'T>> with
        member __.Visit(shape : ShapeWriteMember<'T, 'Field>) =
            let propAttr = shape.MemberInfo.GetCustomAttributes<JsonPropertyAttribute>() |> Seq.tryPick Some
            let label = 
                match propAttr with
                | Some attr when attr.Label <> null -> attr.Label
                | _ -> shape.Label

            let pickler = 
                match propAttr with
                | Some attr when attr.Pickler <> null -> extractPicklerFromType<'Field> resolver attr.Pickler
                | _ -> resolver.Resolve<'Field>()

            let required = isRequired || propAttr |> Option.exists (fun attr -> attr.Required)

            { new IFieldPickler<'T> with
                member __.Label = label
                member __.IsRequired = required
                member __.Pickle writer t = 
                    let field = shape.Project t
                    writer.WriteKey label
                    pickler.Pickle writer field

                member __.UnPickle reader t =
                    let field = pickler.UnPickle reader
                    shape.Inject t field } }

let private mkFieldPicklers resolver allFieldsRequired (fields : IShapeWriteMember<'T>[]) =
    fields |> Array.map (mkFieldPickler resolver allFieldsRequired)


type RecordPickler<'TRecord> (resolver : IPicklerResolver, allFieldsRequired, ctor : unit -> 'TRecord, fields : IShapeWriteMember<'TRecord>[]) =
    let picklers = mkFieldPicklers resolver allFieldsRequired fields
    let flags = picklers |> Array.map (fun p -> p.IsRequired)
    let index = BinSearch (picklers |> Array.map (fun p -> p.Label))

    interface JsonPickler<'TRecord> with
        member __.Pickle writer record =
            writer.WriteStartObject()
            for p in picklers do p.Pickle writer record
            writer.WriteEndObject()

        member __.UnPickle reader =
            let _ = reader.EnsureToken JsonTag.StartObject
            let mutable record = ctor()
            let mutable tok = reader.NextToken()
            let flags = Array.clone flags
            while tok.Tag <> JsonTag.EndObject do
                let label = tok.AsKey()
                match index.TryFindIndex label with
                | i when i >= 0 -> 
                    flags.[i] <- false
                    record <- picklers.[i].UnPickle reader record
                | _ -> reader.ConsumeValue()

                tok <- reader.NextToken()

            match Array.IndexOf(flags, true) with
            | i when i >= 0 ->
                sprintf "JSON missing required field '%s' for type '%O'." index.Values.[i] typeof<'TRecord>
                |> VardusiaException
                |> raise

            | _ -> record

let [<Literal>] private unionCaseKey = "case"
let [<Literal>] private unionFieldsKey = "fields"

type FSharpUnionPickler<'TUnion> (resolver : IPicklerResolver, allFieldsRequired : bool, shape : ShapeFSharpUnion<'TUnion>) =
    let picklerss = shape.UnionCases |> Array.map (fun uc -> uc.Fields |> mkFieldPicklers resolver allFieldsRequired)
    let labelss = picklerss |> Array.map (fun picklers -> picklers |> Array.map (fun p -> p.Label) |> BinSearch)
    let flagss = picklerss |> Array.map (fun picklers -> picklers |> Array.map (fun p -> p.IsRequired))
    let tags = shape.UnionCases |> Array.map (fun uc -> uc.CaseInfo.Name)
    let caseIndex = BinSearch tags

    interface JsonPickler<'TUnion> with
        member __.Pickle writer union =
            writer.WriteStartObject()
            let tag = shape.GetTag union
            let picklers = picklerss.[tag]
            writer.WriteKey unionCaseKey
            writer.WriteString tags.[tag]

            if picklers.Length > 0 then
                writer.WriteKey unionFieldsKey
                writer.WriteStartObject()
                for p in picklers do p.Pickle writer union
                writer.WriteEndObject()

            writer.WriteEndObject()

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
            let flags = Array.clone flagss.[tag]
            let mutable record = shape.UnionCases.[tag].CreateUninitialized()
            
            tok <- reader.NextToken()
            match tok.Tag with
            | JsonTag.EndObject -> ()
            | JsonTag.Key when tok.Value = unionFieldsKey ->
                let _ = reader.EnsureToken JsonTag.StartObject
                tok <- reader.NextToken()
                while tok.Tag <> JsonTag.EndObject do
                    let label = tok.AsKey()
                    match labels.TryFindIndex label with
                    | i when i >= 0 -> 
                        flags.[i] <- false
                        record <- picklers.[i].UnPickle reader record
                    | _ -> reader.ConsumeValue()

                    tok <- reader.NextToken()

                let _ = reader.EnsureToken JsonTag.EndObject
                ()

            | _ -> unexpectedToken tok

            match Array.IndexOf(flags, true) with
            | i when i >= 0 ->
                sprintf "JSON missing required field '%s' for type '%O.%s'." labels.Values.[i] typeof<'TUnion> tags.[tag]
                |> VardusiaException
                |> raise
            | _ -> record