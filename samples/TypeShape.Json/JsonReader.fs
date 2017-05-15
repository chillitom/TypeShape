namespace Vardusia

open System
open System.Text
open System.Globalization

type JsonTag =
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

[<Struct>]
type JsonToken(tag : JsonTag, value : string, index : int) =
    member __.Tag = tag
    member __.Index = index
    member __.Value = value

    static member inline Null i = new JsonToken(JsonTag.Null, null, i)
    static member inline False i = new JsonToken(JsonTag.False, null, i)
    static member inline True i = new JsonToken(JsonTag.True, null, i)
    static member inline String i str = new JsonToken(JsonTag.String, str, i)
    static member inline Number i str = new JsonToken(JsonTag.Number, str, i)
    static member inline Colon i = new JsonToken(JsonTag.Colon, null, i)
    static member inline Comma i = new JsonToken(JsonTag.Comma, null, i)
    static member inline StartObject i = new JsonToken(JsonTag.StartObject, null, i)
    static member inline EndObject i = new JsonToken(JsonTag.EndObject, null, i)
    static member inline StartArray i = new JsonToken(JsonTag.StartArray, null, i)
    static member inline EndArray i = new JsonToken(JsonTag.EndArray, null, i)
    static member inline EOF i = new JsonToken(JsonTag.EOF, null, i)

[<AutoOpen>]
module private JsonReaderImpl =
    open Constants

    let inline unexpectedToken (tok : JsonToken) =
        failwithf "Unexpected JSON token '%O' at position %d" tok.Tag tok.Index

    let inline failRead (input : string) (pos : int) =
        failwithf "Invalid JSON string '%s'" input.[max (pos - 3) 0 .. min (pos + 3) (input.Length - 1)]

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

                //| 'U' ->
                //    if i + 9 >= n then failRead input i
                //    let unicodeChar (s:string) =
                //        if s.Length <> 8 then failwith "unicodeChar";
                //        if s.[0..1] <> "00" then failwith "unicodeChar";
                //        UnicodeHelper.getUnicodeSurrogatePair <| System.UInt32.Parse(s, NumberStyles.HexNumber) 
                //    let lead, trail = unicodeChar (s.Substring(i+2, 8))
                //    buf.Append(lead) |> ignore
                //    buf.Append(trail) |> ignore
                //    i <- i + 8  // the \ and u will also be skipped past further below
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

    let inline parseToken (sb : StringBuilder) (i : byref<int>) (input : string) =
        let input = input
        let n = input.Length

        if skipWhiteSpace input n &i then JsonToken.EOF n else
        
        let idx = i

        match input.[i] with
        | 'n' when matchLiteral "null" input n &i -> JsonToken.Null idx
        | 'f' when matchLiteral "false" input n &i -> JsonToken.False idx
        | 't' when matchLiteral "true" input n &i -> JsonToken.True idx
        | Constants.StartObject -> i <- i + 1 ; JsonToken.StartObject idx
        | Constants.EndObject -> i <- i + 1 ; JsonToken.EndObject idx
        | Constants.StartArray -> i <- i + 1 ; JsonToken.StartArray idx
        | Constants.EndArray -> i <- i + 1 ; JsonToken.EndArray idx
        | Constants.Colon -> i <- i + 1 ; JsonToken.Colon idx
        | Constants.Comma -> i <- i + 1 ; JsonToken.Comma idx
        | Constants.Quote ->
            i <- i + 1
            let str = parseQuotedString input n sb &i
            JsonToken.String idx str

        | _ -> 
            let str = parseUnquotedString input n sb &i
            if isNumber str then JsonToken.Number idx str
            else JsonToken.String idx str
        

type JsonReader(input : string, format : IFormatProvider) =
    let mutable pos = 0
    let mutable isPeeked = false
    let mutable peeked = Unchecked.defaultof<_>
    let sb = new StringBuilder()

    member __.Format = format

    member __.ClearPeeked() = isPeeked <- false

    member __.PeekToken() : JsonToken =
        if isPeeked then peeked
        else
            let mutable i = pos
            let tok = parseToken sb &i input
            pos <- i
            isPeeked <- true
            peeked <- tok
            tok

    member __.NextToken() : JsonToken =
        if isPeeked then
            isPeeked <- false
            peeked
        else
            let mutable i = pos
            let tok = parseToken sb &i input
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

    member tok.AsTimeSpan fmt : TimeSpan =
        match tok.Tag with
        | JsonTag.String ->
            let mutable result = Unchecked.defaultof<_>
            if TimeSpan.TryParse("G", fmt, &result) then result
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


type JsonReader with
    member inline reader.EnsureToken(tag) =
        let tok = reader.NextToken()
        if tag <> tok.Tag then
            unexpectedToken tok