namespace Vardusia

open System

type JsonPickler<'T> =
    abstract Pickle : JsonWriter -> 'T -> unit
    abstract UnPickle : JsonReader -> 'T

type IJsonPicklerResolver =
    abstract Resolve<'T> : unit -> JsonPickler<'T>

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

open TypeShape
open TypeShape_Utils
open System.Reflection.Emit

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
    let picklers = mkFieldPicklers resolver shape.Fields
    let labels = picklers |> Array.map fst |> BinSearch

    interface JsonPickler<'TRecord> with
        member __.Pickle writer record =
            writer.StartObject()
            for label,pickler in picklers do
                writer.FieldName label
                pickler.Pickle writer record
            writer.EndObject()

        member __.UnPickle reader =
            reader.EnsureToken JsonTag.StartObject
            let mutable record = shape.CreateUninitialized()
            let mutable tok = reader.NextToken()
            while tok.Tag = JsonTag.String do
                let label = tok.Value
                reader.EnsureToken JsonTag.Colon
                match labels.TryFindIndex label with
                | i when i >= 0 ->
                    let _,pickler = picklers.[i]
                    record <- pickler.UnPickle reader record
                | _ -> ()

                tok <- reader.NextToken()

            if tok.Tag = JsonTag.EndObject then record
            else unexpectedToken tok