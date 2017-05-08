namespace TypeShape.Json

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

    //member inline tok.AsBoolean(fmt : IFormatProvider) =
    //    match tok.Tag with

[<AutoOpen>]
module private JsonReaderImpl =

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
        

type JsonReader(input : string) =
    let mutable pos = 0
    let sb = new StringBuilder()

    member __.NextToken() : JsonToken =
        let input = input
        let n = input.Length
        let mutable i = pos

        if skipWhiteSpace input n &i then JsonToken.EOF n else
        
        let idx = i

        match input.[i] with
        | 'n' when matchLiteral "null" input n &i -> pos <- i ; JsonToken.Null idx
        | 'f' when matchLiteral "false" input n &i -> pos <- i ; JsonToken.False idx
        | 't' when matchLiteral "true" input n &i -> pos <- i ; JsonToken.True idx
        | Constants.StartObject -> pos <- i + 1 ; JsonToken.StartObject idx
        | Constants.EndObject -> pos <- i + 1 ; JsonToken.EndObject idx
        | Constants.StartArray -> pos <- i + 1 ; JsonToken.StartArray idx
        | Constants.EndArray -> pos <- i + 1 ; JsonToken.EndArray idx
        | Constants.Colon -> pos <- i + 1 ; JsonToken.Colon idx
        | Constants.Comma -> pos <- i + 1 ; JsonToken.Comma idx
        | Constants.Quote ->
            i <- i + 1
            let str = parseQuotedString input n sb &i
            pos <- i ; JsonToken.String idx str

        | c -> 
            let str = parseUnquotedString input n sb &i
            pos <- i ; 
            if isNumber str then JsonToken.Number idx str
            else JsonToken.String idx str