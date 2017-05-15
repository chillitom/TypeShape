namespace Vardusia

open System
open System.Text

[<AutoOpen>]
module private JsonWriterImpl =

    let [<Literal>] spaceStrSize = 1024
    let spacesStr = String(Constants.Space, spaceStrSize)
    let spaceColonSpace = String [|Constants.Space; Constants.Colon; Constants.Space|]

    let inline newLine indent depth (sb : StringBuilder) =
        if indent > 0 then
            let _ = sb.AppendLine()
            let mutable spaces = indent * depth
            while spaces > 0 do
                let k = min spaces spaceStrSize
                let _ = sb.Append(spacesStr, 0, k)
                spaces <- spaces - k

type JsonWriter(indent : int, fmt : IFormatProvider) =
    let sb = new StringBuilder()
    let mutable depth = 0

    member __.Format = fmt

    member __.Null() = append sb Constants.Null
    member __.Bool b = if b then append sb Constants.True else append sb Constants.False
    member __.Number (n : string) = append sb n
    member __.String (s : string) = appendEscaped sb s
    member __.FieldName(name : string) =
        appendEscaped sb name
        if indent > 0 then
            append sb spaceColonSpace
        else
            append sb Constants.Colon

    member __.NextValue() =
        append sb Constants.Comma
        newLine indent depth sb

    member __.StartObject() = 
        append sb Constants.StartObject 
        depth <- depth + 1 
        newLine indent depth sb

    member __.EndObject() = 
        depth <- depth - 1 
        newLine indent depth sb
        append sb Constants.EndObject 

    member __.StartArray() = 
        append sb Constants.StartArray 
        depth <- depth + 1
        newLine indent depth sb

    member __.EndArray() = 
        depth <- depth - 1 
        newLine indent depth sb
        append sb Constants.EndArray

    member __.ToJson() = sb.ToString()


type JsonWriter with

    member inline jw.WriteValue(value : string) =
        match value with
        | null -> jw.Null()
        | _ -> jw.String value
    
    member inline jw.WriteValue(sbyte : sbyte) =
        let value = format jw.Format sbyte
        jw.Number value

    member inline jw.WriteValue(int16 : int16) =
        let value = format jw.Format int16
        jw.Number value

    member inline jw.WriteValue(int32 : int) =
        let value = format jw.Format int32
        jw.Number value

    member inline jw.WriteValue(int64 : int64) =
        let value = format jw.Format int64
        jw.Number value

    member inline jw.WriteValue(byte : byte) =
        let value = format jw.Format byte
        jw.Number value

    member inline jw.WriteValue(uint16 : uint16) =
        let value = format jw.Format uint16
        jw.Number value

    member inline jw.WriteValue(uint32 : uint32) =
        let value = format jw.Format uint32
        jw.Number value

    member inline jw.WriteValue(uint64 : uint64) =
        let value = format jw.Format uint64
        jw.Number value

    member inline jw.WriteValue(bigint : bigint) =
        let value = format jw.Format bigint
        jw.Number value

    member inline jw.WriteValue(single : single) =
        if Single.IsNaN single then jw.Null()
        elif Single.IsPositiveInfinity single then jw.String Constants.PositiveInfinity
        elif Single.IsNegativeInfinity single then jw.String Constants.NegativeInfinity
        else
            let value = format jw.Format single
            jw.Number value

    member inline jw.WriteValue(double : double) =
        if Double.IsNaN double then jw.Null()
        elif Double.IsPositiveInfinity double then jw.String Constants.PositiveInfinity
        elif Double.IsNegativeInfinity double then jw.String Constants.NegativeInfinity
        else
            let value = format jw.Format double
            jw.Number value

    member inline jw.WriteValue(timespan : TimeSpan) =
        let fmt = timespan.ToString("G", jw.Format)
        jw.String fmt

    member inline jw.WriteValue(dateTime: DateTime) =
        let dto = DateTimeOffset(dateTime)
        let fmt = dto.ToString("o", jw.Format)
        jw.String fmt

    member inline jw.WriteValue(dateTimeOffset : DateTimeOffset) =
        let fmt = dateTimeOffset.ToString("o", jw.Format)
        jw.String fmt

    member inline jw.WriteValue(bytes : byte[]) =
        match bytes with
        | null -> jw.Null()
        | bytes -> jw.String(Convert.ToBase64String bytes)