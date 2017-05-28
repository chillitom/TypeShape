module internal Vardusia.PrimitivePicklers

open System

type UnitPickler<'unit>(unit : 'unit) =
    interface JsonPickler<'unit> with
        member __.Pickle writer _ = writer.WriteNull()
        member __.UnPickle reader =
            let tok = reader.NextToken()
            if tok.Tag = JsonTag.Null then unit
            else unexpectedToken tok

type BoolPickler() =
    interface JsonPickler<bool> with
        member __.Pickle writer bool = writer.WriteBool bool
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

type SinglePickler() =
    interface JsonPickler<single> with
        member __.Pickle writer s = writer.WriteValue s
        member __.UnPickle reader =
            let tok = reader.NextToken()
            tok.AsSingle reader.Format

type DoublePickler() =
    interface JsonPickler<double> with
        member __.Pickle writer s = writer.WriteValue s
        member __.UnPickle reader =
            let tok = reader.NextToken()
            tok.AsDouble reader.Format

type DecimalPickler() =
    interface JsonPickler<decimal> with
        member __.Pickle writer d = writer.WriteValue d
        member __.UnPickle reader =
            let tok = reader.NextToken()
            tok.AsDecimal reader.Format

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