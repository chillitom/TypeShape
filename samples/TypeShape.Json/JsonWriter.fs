namespace Vardusia

open System
open System.Collections.Generic
open System.Text

type Indent =
    | Compact   = -1
    | None      =  0

[<RequireQualifiedAccess>]
module Indent =

    let Spaces (spaces : int) = enum<Indent> spaces

module private JsonWriterImpl =
    
    [<Flags>]
    type JCtx =
        | Root      = 1uy
        | Array     = 2uy
        | Object    = 4uy
        | Key       = 8uy
        | Sequence  = 16uy

    let [<Literal>] spaceStrSize = 1024
    let spacesStr = String(Constants.Space, spaceStrSize)

    let inline newLine indent (stack : Stack<JCtx>) (sb : StringBuilder) =
        if indent > 0 then
            let _ = sb.AppendLine()
            let mutable spaces = indent * (stack.Count - 1)
            while spaces > 0 do
                let k = min spaces spaceStrSize
                let _ = sb.Append(spacesStr, 0, k)
                spaces <- spaces - k

    // Encode characters that are not valid in JS string. The implementation is based
    // on https://github.com/mono/mono/blob/master/mcs/class/System.Web/System.Web/HttpUtility.cs
    let inline appendEscaped (sb : StringBuilder) (str : string) =
        let n = str.Length - 1
        append sb '"'
        for i = 0 to n do
            let c = str.[i]
            let ci = int c
            if ci >= 0 && ci <= 7 || ci = 11 || ci >= 14 && ci <= 31 then
                sb.AppendFormat("\\u{0:x4}", ci) |> ignore
            else
                match c with
                | '\b' -> append sb "\\b"
                | '\t' -> append sb "\\t"
                | '\n' -> append sb "\\n"
                | '\f' -> append sb "\\f"
                | '\r' -> append sb "\\r"
                | '"'  -> append sb "\\\""
                | '\\' -> append sb "\\\\"
                | s -> append sb s
        append sb '"'

    let inline appendComma (sb : StringBuilder) (indent : int) =
        append sb Constants.Comma
        if indent >= 0 then append sb Constants.Space

    let inline appendColon (sb : StringBuilder) (indent : int) =
        append sb Constants.Colon
        if indent >= 0 then append sb Constants.Space

    let inline markSequence (stack : Stack<JCtx>) =
        let c = stack.Pop()
        stack.Push(c ||| JCtx.Sequence)

    let inline checkForValue sb indent (stack : Stack<JCtx>) =
        let ctx = stack.Peek()
        if hasFlag JCtx.Object ctx then 
            raise <| VardusiaException("Cannot insert json values into objects.")

        elif hasFlag JCtx.Sequence ctx then 
            appendComma sb indent
            newLine indent stack sb

        elif hasFlag JCtx.Key ctx then
            let _ = stack.Pop()
            if stack.Peek() |> hasFlag JCtx.Sequence |> not then
                markSequence stack

        elif hasFlag JCtx.Array ctx then
            markSequence stack
            newLine indent stack sb
        else
            markSequence stack

open JsonWriterImpl

type JsonWriter(indent : Indent, fmt : IFormatProvider) =
    let intIndent = int indent
    let sb = new StringBuilder()
    let stack = new Stack<JCtx>(20)
    do stack.Push JCtx.Root

    member __.Format = fmt
    member __.Indent = indent

    member __.Null() =
        checkForValue sb intIndent stack
        append sb Constants.Null

    member __.Bool bool = 
        checkForValue sb intIndent stack
        if bool then append sb Constants.True 
        else append sb Constants.False

    member internal __.Number (number : string) = 
        checkForValue sb intIndent stack
        append sb number

    member internal __.String (string : string) =
        checkForValue sb intIndent stack
        appendEscaped sb string

    member __.Key (name : string) =
        if isNull name then raise <| new ArgumentNullException()
        let ctx = stack.Peek()
        if not <| hasFlag JCtx.Object ctx then
            raise <| VardusiaException("Json key identifiers can only be inserted in json objects")

        if hasFlag JCtx.Sequence ctx then appendComma sb intIndent
        newLine intIndent stack sb

        appendEscaped sb name
        appendColon sb intIndent
        stack.Push JCtx.Key

    member __.StartObject() =
        checkForValue sb intIndent stack
        append sb Constants.StartObject
        stack.Push JCtx.Object

    member __.StartArray() =
        checkForValue sb intIndent stack
        append sb Constants.StartArray
        stack.Push JCtx.Array

    member __.EndObject() =
        let ctx = stack.Peek()
        if not <| hasFlag JCtx.Object ctx then
            raise <| VardusiaException("Cannot insert EndObject token here.")

        let _ = stack.Pop()
        if hasFlag JCtx.Sequence ctx then newLine intIndent stack sb
        append sb Constants.EndObject

    member __.EndArray() =
        let ctx = stack.Peek()
        if not <| hasFlag JCtx.Array ctx then
            raise <| VardusiaException("Cannot insert EndArray token here.")

        let _ = stack.Pop()
        if hasFlag JCtx.Sequence ctx then newLine intIndent stack sb
        append sb Constants.EndArray 

    member __.ToJson() = sb.ToString()


type JsonWriter with

    member jw.WriteValue(value : string) =
        match value with
        | null -> jw.Null()
        | _ -> jw.String value
    
    member jw.WriteValue(sbyte : sbyte) =
        let value = format jw.Format sbyte
        jw.Number value

    member jw.WriteValue(int16 : int16) =
        let value = format jw.Format int16
        jw.Number value

    member jw.WriteValue(int32 : int) =
        let value = format jw.Format int32
        jw.Number value

    member jw.WriteValue(int64 : int64) =
        let value = format jw.Format int64
        jw.Number value

    member jw.WriteValue(byte : byte) =
        let value = format jw.Format byte
        jw.Number value

    member jw.WriteValue(uint16 : uint16) =
        let value = format jw.Format uint16
        jw.Number value

    member jw.WriteValue(uint32 : uint32) =
        let value = format jw.Format uint32
        jw.Number value

    member jw.WriteValue(uint64 : uint64) =
        let value = format jw.Format uint64
        jw.Number value

    member jw.WriteValue(bigint : bigint) =
        let value = format jw.Format bigint
        jw.Number value

    member jw.WriteValue(single : single) =
        if Single.IsNaN single then jw.Null()
        elif Single.IsPositiveInfinity single then jw.String Constants.PositiveInfinity
        elif Single.IsNegativeInfinity single then jw.String Constants.NegativeInfinity
        else
            let value = format jw.Format single
            jw.Number value

    member jw.WriteValue(double : double) =
        if Double.IsNaN double then jw.Null()
        elif Double.IsPositiveInfinity double then jw.String Constants.PositiveInfinity
        elif Double.IsNegativeInfinity double then jw.String Constants.NegativeInfinity
        else
            let value = format jw.Format double
            jw.Number value

    member jw.WriteValue(timespan : TimeSpan) =
        let fmt = timespan.ToString("G", jw.Format)
        jw.String fmt

    member jw.WriteValue(dateTime: DateTime) =
        let dto = DateTimeOffset(dateTime)
        let fmt = dto.ToString("o", jw.Format)
        jw.String fmt

    member jw.WriteValue(dateTimeOffset : DateTimeOffset) =
        let fmt = dateTimeOffset.ToString("o", jw.Format)
        jw.String fmt

    member jw.WriteValue(bytes : byte[]) =
        match bytes with
        | null -> jw.Null()
        | bytes -> jw.String(Convert.ToBase64String bytes)