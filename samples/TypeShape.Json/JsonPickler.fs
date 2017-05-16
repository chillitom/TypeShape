namespace Vardusia

open System
open System.Collections.Generic

type JsonPickler<'T> =
    abstract Pickle : JsonWriter -> 'T -> unit
    abstract UnPickle : JsonReader -> 'T

type IJsonPicklerResolver =
    abstract Resolve<'T> : unit -> JsonPickler<'T>

type UnitPickler<'unit>(unit : 'unit) =
    interface JsonPickler<'unit> with
        member __.Pickle writer _ = writer.Null()
        member __.UnPickle reader =
            let tok = reader.NextToken()
            if tok.Tag = JsonTag.Null then unit
            else unexpectedToken tok

type BoolPickler() =
    interface JsonPickler<bool> with
        member __.Pickle writer bool = writer.Bool bool
        member __.UnPickle reader =
            let tok = reader.NextToken()
            tok.AsBoolean()

type StringPickler() =
    interface JsonPickler<string> with
        member __.Pickle writer str = writer.WriteValue str

        member __.UnPickle reader =
            let tok = reader.NextToken()
            tok.AsString()

type SBytePickler() =
    interface JsonPickler<sbyte> with
        member __.Pickle writer byte = writer.WriteValue byte
        member __.UnPickle reader =
            let tok = reader.NextToken()
            tok.AsSByte reader.Format

type Int16Pickler() =
    interface JsonPickler<int16> with
        member __.Pickle writer int16 = writer.WriteValue int16
        member __.UnPickle reader =
            let tok = reader.NextToken()
            tok.AsInt16 reader.Format

type Int32Pickler() =
    interface JsonPickler<int32> with
        member __.Pickle writer int = writer.WriteValue int
        member __.UnPickle reader = 
            let tok = reader.NextToken()
            tok.AsInt32 reader.Format

type Int64Pickler() =
    interface JsonPickler<int64> with
        member __.Pickle writer int = writer.WriteValue int
        member __.UnPickle reader = 
            let tok = reader.NextToken()
            tok.AsInt64 reader.Format

type BytePickler() =
    interface JsonPickler<byte> with
        member __.Pickle writer byte = writer.WriteValue byte
        member __.UnPickle reader =
            let tok = reader.NextToken()
            tok.AsByte reader.Format

type UInt16Pickler() =
    interface JsonPickler<uint16> with
        member __.Pickle writer int16 = writer.WriteValue int16
        member __.UnPickle reader =
            let tok = reader.NextToken()
            tok.AsUInt16 reader.Format

type UInt32Pickler() =
    interface JsonPickler<uint32> with
        member __.Pickle writer int = writer.WriteValue int
        member __.UnPickle reader = 
            let tok = reader.NextToken()
            tok.AsUInt32 reader.Format

type UInt64Pickler() =
    interface JsonPickler<uint64> with
        member __.Pickle writer int = writer.WriteValue int
        member __.UnPickle reader = 
            let tok = reader.NextToken()
            tok.AsUInt64 reader.Format

type TimeSpanPickler() =
    interface JsonPickler<TimeSpan> with
        member __.Pickle writer timespan = writer.WriteValue timespan
        member __.UnPickle reader =
            let tok = reader.NextToken()
            tok.AsTimeSpan reader.Format

type BigIntegerPickler() =
    interface JsonPickler<bigint> with
        member __.Pickle writer bigint = writer.WriteValue bigint
        member __.UnPickle reader =
            let tok = reader.NextToken()
            tok.AsBigInteger reader.Format

type DateTimePickler() =
    interface JsonPickler<DateTime> with
        member __.Pickle writer datetime = writer.WriteValue datetime
        member __.UnPickle reader =
            let tok = reader.NextToken()
            tok.AsDateTime reader.Format

type DateTimeOffsetPickler() =
    interface JsonPickler<DateTimeOffset> with
        member __.Pickle writer dto = writer.WriteValue dto
        member __.UnPickle reader =
            let tok = reader.NextToken()
            tok.AsDateTimeOffset reader.Format

type ByteArrayPickler() =
    interface JsonPickler<byte[]> with
        member __.Pickle writer bytes = writer.WriteValue bytes
        member __.UnPickle reader =
            let tok = reader.NextToken()
            tok.AsByteArray()


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

type EnumPickler<'Enum, 'U when 'Enum : enum<'U>>(upickler : JsonPickler<'U>) =
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

type CollectionPickler<'Collection, 'T when 'Collection :> seq<'T>>(ctor : seq<'T> -> 'Collection, epickler : JsonPickler<'T>) =
    interface JsonPickler<'Collection> with
        member __.Pickle writer ts =
            use e = ts.GetEnumerator()
            writer.StartArray()
            if e.MoveNext() then 
                epickler.Pickle writer e.Current

            while e.MoveNext() do
                writer.NextValue()
                epickler.Pickle writer e.Current

            writer.EndArray()

        member __.UnPickle reader =
            reader.EnsureToken JsonTag.StartArray
            let ra = ResizeArray<'T>()
            let mutable tok = reader.PeekToken()
            if tok.Tag <> JsonTag.EndArray then
                let t = epickler.UnPickle reader
                ra.Add t
                tok <- reader.NextToken()

            while tok.Tag = JsonTag.Comma do
                let t = epickler.UnPickle reader
                ra.Add t
                tok <- reader.NextToken()

            if tok.Tag <> JsonTag.EndArray then unexpectedToken tok
            else ctor ra

type DictionaryPickler<'Dict, 'Value when 'Dict :> seq<KeyValuePair<string, 'Value>>>(ctor : seq<KeyValuePair<string, 'Value>> -> 'Dict, valuePickler : JsonPickler<'Value>) =
    interface JsonPickler<'Dict> with
        member __.Pickle writer map =
            writer.StartObject()
            let e = map.GetEnumerator()
            if e.MoveNext() then
                let kv = e.Current
                writer.FieldName kv.Key
                valuePickler.Pickle writer kv.Value

            while e.MoveNext() do
                writer.NextValue()
                let kv = e.Current
                writer.FieldName kv.Key
                valuePickler.Pickle writer kv.Value

            writer.EndObject()

        member __.UnPickle reader =
            reader.EnsureToken JsonTag.StartObject
            let ra = ResizeArray<KeyValuePair<string, 'Value>>()
            let mutable tok = reader.NextToken()
            if tok.Tag = JsonTag.String then
                let key = tok.Value
                reader.EnsureToken JsonTag.Colon
                let value = valuePickler.UnPickle reader
                ra.Add(KeyValuePair(key, value))
                tok <- reader.NextToken()

            while tok.Tag = JsonTag.Comma do
                tok <- reader.NextToken()
                if tok.Tag <> JsonTag.String then unexpectedToken tok
                let key = tok.Value
                reader.EnsureToken JsonTag.Colon
                let value = valuePickler.UnPickle reader
                ra.Add(KeyValuePair(key, value))
                tok <- reader.NextToken()

            if tok.Tag <> JsonTag.EndObject then unexpectedToken tok
            else ctor ra
                

open TypeShape
open TypeShape_Utils

[<AutoOpen>]
module internal Foo =

    type IFieldPickler<'T> =
        abstract Pickle : JsonWriter -> 'T -> unit
        abstract UnPickle : JsonReader -> 'T -> 'T

    let mkFieldPickler (resolver : IJsonPicklerResolver) (shapeField : IShapeWriteMember<'T>) =
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

type FSharpRecordPickler<'TRecord> internal (resolver : IJsonPicklerResolver, shape : ShapeFSharpRecord<'TRecord>) =
    let labels, picklers = mkFieldPicklers resolver shape.Fields |> Array.unzip
    let index = BinSearch labels

    interface JsonPickler<'TRecord> with
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
            let mutable record = shape.CreateUninitialized()
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
            else unexpectedToken tok


module private Bar =

    let generatePickler<'T> (resolver : IJsonPicklerResolver) : JsonPickler<'T> =
        let EQ (jp : JsonPickler<'a>) = unbox<JsonPickler<'T>> jp
        match shapeof<'T> with
        | Shape.Unit -> UnitPickler<unit>(()) |> EQ
        | Shape.Byte -> BytePickler() |> EQ
        | Shape.SByte -> SBytePickler() |> EQ
        | Shape.Int16 -> Int16Pickler() |> EQ
        | Shape.Int32 -> Int32Pickler() |> EQ
        | Shape.Int64 -> Int64Pickler() |> EQ
        | Shape.UInt16 -> UInt16Pickler() |> EQ
        | Shape.UInt32 -> UInt32Pickler() |> EQ
        | Shape.UInt64 -> UInt64Pickler() |> EQ
        | Shape.TimeSpan -> TimeSpanPickler() |> EQ
        | Shape.DateTime -> DateTimePickler() |> EQ
        | Shape.DateTimeOffset -> DateTimeOffsetPickler() |> EQ
        | Shape.BigInt -> BigIntegerPickler() |> EQ
        | Shape.String -> StringPickler() |> EQ
        | Shape.ByteArray -> ByteArrayPickler() |> EQ
        | Shape.Enum s ->
            s.Accept { new IEnumVisitor<JsonPickler<'T>> with
                member __.Visit<'e, 'u when 'e : enum<'u>
                                        and 'e : struct
                                        and 'e :> ValueType
                                        and 'e : (new : unit -> 'e)> () =

                            EnumStringPickler<'e>() |> EQ }

        | Shape.Nullable s ->
            s.Accept { new INullableVisitor<JsonPickler<'T>> with
                member __.Visit<'t when 't : struct
                                    and 't :> ValueType
                                    and 't : (new : unit -> 't)> () =

                    let tpickler = resolver.Resolve<'t>()
                    NullablePickler<'t>(tpickler) |> EQ }

        | Shape.FSharpOption s ->
            s.Accept { new IFSharpOptionVisitor<JsonPickler<'T>> with
                member __.Visit<'t>() =
                    let tpickler = resolver.Resolve<'t>()
                    FSharpOptionPickler<'t>(tpickler) |> EQ }

        | Shape.Array s when s.Rank = 1 ->
            s.Accept { new IArrayVisitor<JsonPickler<'T>> with
                member __.Visit<'t> _ =
                    let tpickler = resolver.Resolve<'t>()
                    CollectionPickler<'t[], 't>(Seq.toArray, tpickler) |> EQ }

        | Shape.FSharpList s ->
            s.Accept { new IFSharpListVisitor<JsonPickler<'T>> with
                member __.Visit<'t> () =
                    let tpickler = resolver.Resolve<'t>()
                    CollectionPickler<'t list, 't>(Seq.toList, tpickler) |> EQ }

        | Shape.ResizeArray s ->
            s.Accept { new IResizeArrayVisitor<JsonPickler<'T>> with
                member __.Visit<'t> () =
                    let tpickler = resolver.Resolve<'t>()
                    CollectionPickler<ResizeArray<'t>, 't>(ResizeArray, tpickler) |> EQ }

        | Shape.FSharpSet s ->
            s.Accept { new IFSharpSetVisitor<JsonPickler<'T>> with
                member __.Visit<'t when 't : comparison> () =
                    let tpickler = resolver.Resolve<'t>()
                    CollectionPickler<Set<'t>, 't>(Set.ofSeq, tpickler) |> EQ }

        | Shape.Dictionary s when s.Key = shapeof<string> ->
            s.Accept { new IDictionaryVisitor<JsonPickler<'T>> with
                member __.Visit<'k,'v when 'k : equality> () =
                    let vpickler = resolver.Resolve<'v>()
                    let mkDict (items : seq<KeyValuePair<_,_>>) =
                        let d = Dictionary<string, 'v>()
                        for kv in items do d.[kv.Key] <- kv.Value
                        d

                    DictionaryPickler<Dictionary<string,'v>, 'v>(mkDict, vpickler) |> EQ }

        | Shape.FSharpMap s when s.Key = shapeof<string> ->
            s.Accept { new IFSharpMapVisitor<JsonPickler<'T>> with
                member __.Visit<'k,'v when 'k : comparison> () =
                    let vpickler = resolver.Resolve<'v>()
                    let mkMap (items : seq<KeyValuePair<_,_>>) =
                        items |> Seq.map (fun kv -> kv.Key,kv.Value) |> Map.ofSeq

                    DictionaryPickler<Map<string,'v>, 'v>(mkMap, vpickler) |> EQ }

        | Shape.FSharpRecord (:? ShapeFSharpRecord<'T> as shape) ->
            FSharpRecordPickler<'T>(resolver, shape) |> EQ

        | _ -> failwithf "Unsupported type '%O'" typeof<'T>


module Pickler =
    open System.Globalization

    let private cache = new TypeCache()

    let resolve<'T> () : JsonPickler<'T> =
        let mutable p = Unchecked.defaultof<JsonPickler<'T>>
        if cache.TryGetValue(&p) then p
        else
            let ctx = cache.CreateRecTypeManager()
            let rec self =
                { new IJsonPicklerResolver with 
                    member __.Resolve<'t>() =
                        match ctx.TryFind<JsonPickler<'t>>() with
                        | Some p -> p
                        | None -> 
                            ctx.CreateUninitialized(fun cell -> 
                                { new JsonPickler<'t> with 
                                    member __.Pickle writer t = cell.Value.Pickle writer t
                                    member __.UnPickle reader = cell.Value.UnPickle reader })
                            |> ignore

                            let p = Bar.generatePickler<'t> self
                            ctx.Complete p }

            self.Resolve<'T> ()


    let pickle (pickler : JsonPickler<'T>) (value : 'T) : string =
        let writer = new JsonWriter(2, CultureInfo.InvariantCulture)
        pickler.Pickle writer value
        writer.ToJson()

    let unpickle (pickler : JsonPickler<'T>) (json : string) : 'T =
        let reader = new JsonReader(json, CultureInfo.InvariantCulture)
        pickler.UnPickle reader
