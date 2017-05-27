namespace Vardusia

open System
open System.Collections.Generic
open System.Text
open System.Globalization

[<Struct>]
type JsonToken internal (tag : JsonTag, value : string, pos : int) =
    member __.Tag = tag
    member __.Position = pos
    member __.Value = value

and JsonTag =
    | Null          = 0uy
    | False         = 1uy
    | True          = 2uy
    | Number        = 3uy
    | String        = 4uy
    | Key           = 5uy
    | StartObject   = 6uy
    | EndObject     = 7uy
    | StartArray    = 8uy
    | EndArray      = 9uy
    | EOF           = 10uy

type JsonToken with
    member tok.Id =
        match tok.Tag with
        | JsonTag.Null -> Constants.Null
        | JsonTag.False -> Constants.False
        | JsonTag.True -> Constants.True
        | JsonTag.Number -> tok.Value
        | JsonTag.Key
        | JsonTag.String -> "\"" + tok.Value + "\""
        | JsonTag.StartObject -> string Constants.StartObject
        | JsonTag.EndObject -> string Constants.EndObject
        | JsonTag.StartArray -> string Constants.StartArray
        | JsonTag.EndArray -> string Constants.EndArray
        | JsonTag.EOF -> "EOF"
        | t -> invalidOp <| sprintf "internal error: unexpected JSON token '%O'" t

[<AutoOpen>]
module Helpers =

    let inline unexpectedToken (tok : JsonToken) =
        sprintf "Unexpected JSON token '%s' at position %d." tok.Id tok.Position
        |> VardusiaException
        |> raise

module private JsonReaderImpl =

    type JCtx =
        | Root          = 0uy
        | Object        = 1uy
        | Array         = 2uy
        | Key           = 3uy

    type JTag =
        | Null          = 0uy
        | False         = 1uy
        | True          = 2uy
        | Number        = 3uy
        | String        = 4uy
        | Colon         = 5uy
        | Comma         = 6uy
        | StartObject   = 7uy
        | EndObject     = 8uy
        | StartArray    = 9uy
        | EndArray      = 10uy
        | EOF           = 11uy

    type JsonToken with
        static member inline Null i = new JsonToken(JsonTag.Null, null, i)
        static member inline False i = new JsonToken(JsonTag.False, null, i)
        static member inline True i = new JsonToken(JsonTag.True, null, i)
        static member inline String i str = new JsonToken(JsonTag.String, str, i)
        static member inline Number i str = new JsonToken(JsonTag.Number, str, i)
        static member inline Key i str = new JsonToken(JsonTag.Key, str, i)
        static member inline StartObject i = new JsonToken(JsonTag.StartObject, null, i)
        static member inline EndObject i = new JsonToken(JsonTag.EndObject, null, i)
        static member inline StartArray i = new JsonToken(JsonTag.StartArray, null, i)
        static member inline EndArray i = new JsonToken(JsonTag.EndArray, null, i)
        static member inline EOF i = new JsonToken(JsonTag.EOF, null, i)

    let inline formatJTag (tag : JTag) (value : string) =
        match tag with
        | JTag.Null -> Constants.Null
        | JTag.False -> Constants.False
        | JTag.True -> Constants.True
        | JTag.Number -> value
        | JTag.String -> "\"" + value + "\""
        | JTag.Colon -> string Constants.Colon
        | JTag.Comma -> string Constants.Comma
        | JTag.StartObject -> string Constants.StartObject
        | JTag.EndObject -> string Constants.EndObject
        | JTag.StartArray -> string Constants.StartArray
        | JTag.EndArray -> string Constants.EndArray
        | JTag.EOF -> "EOF"
        | _ -> invalidOp "internal error: could not format json tag"

    let inline unexpectedTag (tag : JTag) (value : string) (pos : int) =
        sprintf "Unexpected JSON token '%s' at position %d" (formatJTag tag value) pos
        |> VardusiaException
        |> raise

    let inline failRead (input : string) (pos : int) =
        sprintf "Invalid JSON string '%s'." input.[max (pos - 3) 0 .. min (pos + 3) (input.Length - 1)]
        |> VardusiaException
        |> raise

    let inline skipWhiteSpace (input : string) (n : int) (i : byref<int>) =
        while i < n && Char.IsWhiteSpace(input.[i]) do i <- i + 1
        i = n

    let inline matchLiteral (literal : string) (input : string) (n : int) (i : byref<int>) =
        let n0 = literal.Length
        if n0 + i > n then false else
        let mutable j = 0
        let mutable isMatch = true
        while isMatch && j < n0 do
            isMatch <- input.[i + j] = literal.[j]
            j <- j + 1

        if isMatch then i <- i + n0 ; true
        else false

    let inline isNumber (value : string) =
        let mutable x = 0.
        Double.TryParse(value, NumberStyles.Any, CultureInfo.InvariantCulture, &x)

    let inline parseUnquotedString (input : string) (n : int) (sb : StringBuilder) (i : byref<int>) =
        let sb = sb.Clear()
        let mutable notCompleted = true
        while notCompleted && i < n do
            match input.[i] with
            | Constants.StartObject
            | Constants.EndObject
            | Constants.StartArray
            | Constants.EndArray
            | Constants.Comma
            | Constants.Quote
            | Constants.Colon -> notCompleted <- false
            | c when Char.IsWhiteSpace c -> notCompleted <- false
            | c -> append sb c ; i <- i + 1

        sb.ToString()

    let inline parseQuotedString (input : string) (n : int) (sb : StringBuilder) (i : byref<int>) =
        let sb = sb.Clear()
        let mutable notCompleted = true
        while notCompleted && i < n do
            match input.[i] with
            | '"' -> 
                notCompleted <- false
                i <- i + 1

            | '\\' ->
                if i = n + 1 then failRead input i

                match input.[i + 1] with
                | '"' -> append sb '"'
                | 'b' -> append sb '\b'
                | 'f' -> append sb '\f'
                | 'n' -> append sb '\n'
                | 't' -> append sb '\t'
                | 'r' -> append sb '\r'
                | '\\' -> append sb '\\'
                | '/' -> append sb '/'
                | 'u' ->
                    // Taken from FSharp.Data
                    if i + 5 >= n then failRead input i
                    let inline hex2int i d =
                        if d >= '0' && d <= '9' then int32 d - int32 '0'
                        elif d >= 'a' && d <= 'f' then int32 d - int32 'a' + 10
                        elif d >= 'A' && d <= 'F' then int32 d - int32 'A' + 10
                        else failRead input i

                    let c =
                        hex2int i input.[i + 2] * 4096 +
                        hex2int i input.[i + 3] * 256  +
                        hex2int i input.[i + 4] * 16   +
                        hex2int i input.[i + 5]

                    append sb (char c)
                    i <- i + 4

                | 'U' ->
                    if i + 9 >= n then failRead input i
                    let us = input.Substring(i + 2, 8)
                    if us.[0] <> '0' || us.[1] <> '0' then failRead input i
                    let pnum = System.UInt32.Parse(us, NumberStyles.HexNumber)
                    // for coversion to UTF16 surrogate pair: Taken from FSharp.Data
                    // used http://en.wikipedia.org/wiki/UTF-16#Code_points_U.2B010000_to_U.2B10FFFF as a guide below
                    // only code points U+010000 to U+10FFFF supported
                    let codePoint = pnum - 0x010000u
                    let HIGH_TEN_BIT_MASK = 0xFFC00u // 1111|1111|1100|0000|0000
                    let LOW_TEN_BIT_MASK = 0x003FFu  // 0000|0000|0011|1111|1111
                    let lead = char <| (codePoint &&& HIGH_TEN_BIT_MASK >>> 10) + 0xD800u
                    let trail = char <| (codePoint &&& LOW_TEN_BIT_MASK) + 0xDC00u
                    append sb lead ; append sb trail
                    i <- i + 8

                | _ -> failRead input i

                i <- i + 2

            | c -> 
                append sb c
                i <- i + 1

        if notCompleted then failRead input i
        sb.ToString()

    let inline parseNumeric fmt nullVal (tok : JsonToken) =
        match tok.Tag with
        | JsonTag.Null -> nullVal
        | JsonTag.False -> LanguagePrimitives.GenericZero
        | JsonTag.True -> LanguagePrimitives.GenericOne
        | JsonTag.Number -> 
            let mutable num = Unchecked.defaultof< ^t>
            if tryParseNumber fmt &num tok.Value then num
            else unexpectedToken tok
        | JsonTag.String -> 
            let mutable num = Unchecked.defaultof< ^t>
            if tryParseNumber fmt &num tok.Value then num
            else
                let mutable bool = false
                if Boolean.TryParse(tok.Value, &bool) then 
                    if bool then LanguagePrimitives.GenericOne 
                    else LanguagePrimitives.GenericZero
                else unexpectedToken tok
        | _ -> unexpectedToken tok
        
    let inline parseFloat fmt nullVal negInf posInf (tok : JsonToken) : ^t =
        match tok.Tag with
        | JsonTag.Null -> nullVal
        | JsonTag.False -> LanguagePrimitives.GenericZero
        | JsonTag.True -> LanguagePrimitives.GenericOne
        | JsonTag.Number -> 
            let mutable num = Unchecked.defaultof< ^t>
            if tryParseNumber fmt &num tok.Value then num
            else unexpectedToken tok
        | JsonTag.String ->
            match tok.Value with
            | Constants.NegativeInfinity -> negInf
            | Constants.PositiveInfinity -> posInf
            | _ ->
                let mutable num = Unchecked.defaultof< ^t>
                if tryParseNumber fmt &num tok.Value then num
                else
                    let mutable bool = false
                    if Boolean.TryParse(tok.Value, &bool) then 
                        if bool then LanguagePrimitives.GenericOne 
                        else LanguagePrimitives.GenericZero
                    else unexpectedToken tok

        | _ -> unexpectedToken tok

    let parseJsonToken (sb : StringBuilder) (input : string) (n : int) (i : byref<int>) (tokenPos : byref<int>) (token : byref<string>) =
        if skipWhiteSpace input n &i then
            tokenPos <- n
            JTag.EOF
        else
        
        tokenPos <- i

        match input.[i] with
        | 'n' when matchLiteral "null" input n &i -> JTag.Null
        | 'f' when matchLiteral "false" input n &i -> JTag.False
        | 't' when matchLiteral "true" input n &i -> JTag.True
        | Constants.StartObject -> i <- i + 1 ; JTag.StartObject
        | Constants.EndObject -> i <- i + 1 ; JTag.EndObject
        | Constants.StartArray -> i <- i + 1 ; JTag.StartArray
        | Constants.EndArray -> i <- i + 1 ; JTag.EndArray
        | Constants.Colon -> i <- i + 1 ; JTag.Colon
        | Constants.Comma -> i <- i + 1 ; JTag.Comma
        | Constants.Quote ->
            i <- i + 1
            token <- parseQuotedString input n sb &i
            JTag.String

        | _ -> 
            token <- parseUnquotedString input n sb &i
            if isNumber token then JTag.Number
            else JTag.String

    let inline getNextToken (ctxs : Stack<JCtx>) (sb : StringBuilder) (input : string) (pos : byref<int>) =
        let n = input.Length
        let ctx = ctxs.Peek()
        let mutable tokenPos = 0
        let mutable strToken = null
        let mutable tag = parseJsonToken sb input n &pos &tokenPos &strToken
        
        // handle comma tokens first
        if tag = JTag.Comma then
            match ctx with
            | JCtx.Root | JCtx.Object | JCtx.Array -> ()
            | _ -> unexpectedTag tag strToken tokenPos

            // tolerate invalid json strings such '[1,]', '[,1]', '[1 2]' and '[1,,,2]'
            while tag = JTag.Comma do
                tag <- parseJsonToken sb input n &pos &tokenPos &strToken

        let inline popKey () = if ctx = JCtx.Key then let _ = ctxs.Pop() in ()

        match tag, ctx with
        | JTag.Null, (JCtx.Root | JCtx.Key | JCtx.Array) -> popKey () ; JsonToken.Null tokenPos
        | JTag.False, (JCtx.Root | JCtx.Key | JCtx.Array) -> popKey () ; JsonToken.False tokenPos
        | JTag.True, (JCtx.Root | JCtx.Key | JCtx.Array) -> popKey () ; JsonToken.True tokenPos
        | JTag.Number, (JCtx.Root | JCtx.Key | JCtx.Array) -> popKey () ; JsonToken.Number tokenPos strToken
        | JTag.String, (JCtx.Root | JCtx.Key | JCtx.Array) -> popKey () ; JsonToken.String tokenPos strToken
        | (JTag.String | JTag.Number), JCtx.Object ->
            let token = JsonToken.Key tokenPos strToken
            match parseJsonToken sb input n &pos &tokenPos &strToken with
            | JTag.Colon -> ctxs.Push JCtx.Key ; token
            | tag -> unexpectedTag tag strToken tokenPos

        | JTag.StartArray, (JCtx.Root | JCtx.Key | JCtx.Array) -> 
            popKey () ; ctxs.Push JCtx.Array ; JsonToken.StartArray tokenPos

        | JTag.StartObject, (JCtx.Root | JCtx.Key | JCtx.Array) ->
            popKey () ; ctxs.Push JCtx.Object ; JsonToken.StartObject tokenPos

        | JTag.EndArray, JCtx.Array -> let _ = ctxs.Pop() in JsonToken.EndArray tokenPos
        | JTag.EndObject, JCtx.Object -> let _ = ctxs.Pop() in JsonToken.EndObject tokenPos
        | _ -> unexpectedTag tag strToken tokenPos


open JsonReaderImpl

type JsonReader(input : string, format : IFormatProvider) =
    let format = getDefaultFmt format
    let mutable pos = 0
    let mutable isPeeked = false
    let mutable peeked = Unchecked.defaultof<JsonToken>
    let context = new Stack<JCtx>(20)
    do context.Push JCtx.Root
    let sb = new StringBuilder()

    member __.Format = format
    member __.Depth = 
        match context.Peek() with
        | JCtx.Key -> context.Count - 2
        | _ -> context.Count - 1

    member __.PeekToken() : JsonToken =
        if isPeeked then peeked
        else
            let mutable i = pos
            peeked <- getNextToken context sb input &i
            pos <- i
            isPeeked <- true
            peeked

    member __.NextToken() : JsonToken =
        if isPeeked then
            isPeeked <- false
            peeked
        else
            let mutable i = pos
            let tok = getNextToken context sb input &i
            pos <- i
            tok



type JsonToken with
    member tok.AsBoolean() : bool =
        match tok.Tag with
        | JsonTag.Null 
        | JsonTag.False -> false
        | JsonTag.True -> true
        | JsonTag.Number -> 
            let mutable num = 0
            if Int32.TryParse(tok.Value, &num) then num <> 0
            else unexpectedToken tok

        | JsonTag.String ->
            let mutable bool = false
            if Boolean.TryParse(tok.Value, &bool) then bool
            else unexpectedToken tok

        | _ -> unexpectedToken tok

    member tok.AsString() : string =
        match tok.Tag with
        | JsonTag.Null -> null
        | JsonTag.False -> Constants.False
        | JsonTag.True -> Constants.True
        | JsonTag.Number -> tok.Value
        | JsonTag.String -> tok.Value
        | _ -> unexpectedToken tok


    member tok.AsByte fmt : byte = parseNumeric fmt 0uy tok
    member tok.AsInt16 fmt : int16 = parseNumeric fmt 0s tok
    member tok.AsInt32 fmt : int32 = parseNumeric fmt 0 tok
    member tok.AsInt64 fmt : int64 = parseNumeric fmt 0L tok

    member tok.AsSByte fmt : sbyte = parseNumeric fmt 0y tok
    member tok.AsUInt16 fmt : uint16 = parseNumeric fmt 0us tok
    member tok.AsUInt32 fmt : uint32 = parseNumeric fmt 0u tok
    member tok.AsUInt64 fmt : uint64 = parseNumeric fmt 0uL tok

    member tok.AsBigInteger fmt : bigint = parseNumeric fmt 0I tok

    member tok.AsSingle fmt : single = parseFloat fmt Single.NaN Single.NegativeInfinity Single.PositiveInfinity tok
    member tok.AsDouble fmt : double = parseFloat fmt Double.NaN Double.NegativeInfinity Double.PositiveInfinity tok
    member tok.AsDecimal fmt : decimal = parseNumeric fmt 0M tok

    member tok.AsTimeSpan fmt : TimeSpan =
        match tok.Tag with
        | JsonTag.String ->
            let mutable result = Unchecked.defaultof<_>
            if TimeSpan.TryParse(tok.Value, fmt, &result) then result
            else
                unexpectedToken tok

        | _ -> unexpectedToken tok

    member tok.AsDateTime fmt : DateTime =
        match tok.Tag with
        | JsonTag.String ->
            let mutable result = Unchecked.defaultof<_>
            if DateTime.TryParse(tok.Value, fmt, DateTimeStyles.None, &result) then result
            else
                unexpectedToken tok

        | _ -> unexpectedToken tok

    member tok.AsDateTimeOffset fmt : DateTimeOffset =
        match tok.Tag with
        | JsonTag.String -> 
            let mutable result = Unchecked.defaultof<_>
            if DateTimeOffset.TryParse(tok.Value, fmt, DateTimeStyles.None, &result) then result
            else
                unexpectedToken tok

        | _ -> unexpectedToken tok

    member tok.AsByteArray() : byte[] =
        match tok.Tag with
        | JsonTag.Null -> null
        | JsonTag.String -> Convert.FromBase64String tok.Value
        | _ -> unexpectedToken tok

    member inline tok.AsKey() : string =
        match tok.Tag with
        | JsonTag.Key -> tok.Value
        | _ -> unexpectedToken tok

type JsonReader with
    member inline reader.EnsureToken(tag) =
        let tok = reader.NextToken()
        if tag <> tok.Tag then unexpectedToken tok
        tok

    member reader.ConsumeValue() =
        let tok = reader.NextToken()
        match tok.Tag with
        | JsonTag.Null
        | JsonTag.False
        | JsonTag.True
        | JsonTag.Number
        | JsonTag.String -> ()

        | JsonTag.StartArray
        | JsonTag.StartObject ->
            let mutable depth = 1
            while depth > 0 do
                let tok = reader.NextToken()
                match tok.Tag with
                | JsonTag.StartArray
                | JsonTag.StartObject -> depth <- depth + 1
                | JsonTag.EndArray
                | JsonTag.EndObject -> depth <- depth - 1
                | _ -> ()

        | _ -> unexpectedToken tok