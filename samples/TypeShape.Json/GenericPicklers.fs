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
            | JsonTag.Null -> reader.ClearPeeked(); None
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
            use e = ts.GetEnumerator()
            writer.StartArray()
            if e.MoveNext() then 
                tpickler.Pickle writer e.Current

            while e.MoveNext() do
                writer.NextValue()
                tpickler.Pickle writer e.Current

            writer.EndArray()

        member __.UnPickle reader =
            reader.EnsureToken JsonTag.StartArray
            let ra = ResizeArray<'T>()
            let mutable tok = reader.PeekToken()
            if tok.Tag <> JsonTag.EndArray then
                let t = tpickler.UnPickle reader
                ra.Add t
                tok <- reader.NextToken()

            while tok.Tag = JsonTag.Comma do
                let t = tpickler.UnPickle reader
                ra.Add t
                tok <- reader.NextToken()

            if tok.Tag <> JsonTag.EndArray then unexpectedToken tok
            else ctor ra }

type JsonField<'Value> = KeyValuePair<string, 'Value>
let inline mkDictionaryPickler<'Dict, 'Value when 'Dict :> seq<JsonField<'Value>>> 
    (vpickler : JsonPickler<'Value>) (ctor : ResizeArray<JsonField<'Value>> -> 'Dict) =

    { new JsonPickler<'Dict> with
        member __.Pickle writer map =
            writer.StartObject()
            let e = map.GetEnumerator()
            if e.MoveNext() then
                let kv = e.Current
                writer.FieldName kv.Key
                vpickler.Pickle writer kv.Value

            while e.MoveNext() do
                writer.NextValue()
                let kv = e.Current
                writer.FieldName kv.Key
                vpickler.Pickle writer kv.Value

            writer.EndObject()

        member __.UnPickle reader =
            reader.EnsureToken JsonTag.StartObject
            let ra = ResizeArray<KeyValuePair<string, 'Value>>()
            let mutable tok = reader.NextToken()
            if tok.Tag = JsonTag.String then
                let key = tok.Value
                reader.EnsureToken JsonTag.Colon
                let value = vpickler.UnPickle reader
                ra.Add(KeyValuePair(key, value))
                tok <- reader.NextToken()

            while tok.Tag = JsonTag.Comma do
                tok <- reader.NextToken()
                if tok.Tag <> JsonTag.String then unexpectedToken tok
                let key = tok.Value
                reader.EnsureToken JsonTag.Colon
                let value = vpickler.UnPickle reader
                ra.Add(KeyValuePair(key, value))
                tok <- reader.NextToken()

            if tok.Tag <> JsonTag.EndObject then unexpectedToken tok
            else ctor ra }


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
                    writer.FieldName label
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
            let n = picklers.Length
            if n > 0 then
                picklers.[0].Pickle writer record

            for i = 1 to n - 1 do
                writer.NextValue()
                picklers.[i].Pickle writer record

            writer.EndObject()

        member __.UnPickle reader =
            reader.EnsureToken JsonTag.StartObject
            let mutable record = ctor()
            let mutable tok = reader.NextToken()
            if tok.Tag = JsonTag.String then
                let label = tok.Value
                reader.EnsureToken JsonTag.Colon
                match index.TryFindIndex label with
                | i when i >= 0 -> record <- picklers.[i].UnPickle reader record
                | _ -> ()

                tok <- reader.NextToken()

            while tok.Tag = JsonTag.Comma do
                tok <- reader.NextToken()
                if tok.Tag <> JsonTag.String then unexpectedToken tok
                let label = tok.Value
                reader.EnsureToken JsonTag.Colon
                match index.TryFindIndex label with
                | i when i >= 0 -> record <- picklers.[i].UnPickle reader record
                | _ -> ()

                tok <- reader.NextToken()

            if tok.Tag = JsonTag.EndObject then record
            else unexpectedToken tok }