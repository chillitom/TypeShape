namespace Vardusia

open System
open System.Globalization
open System.Collections.Generic
open System.Numerics

type internal JsonNumberVal =
    | Decimal of decimal
    | Float of double
    | String of string

[<JsonNumberPicklerFactory>]
[<Struct; StructuredFormatDisplay("{SFD}"); NoEquality; NoComparison>]
type JsonNumber internal (value : JsonNumberVal) =
    member __.ToString(fmt : IFormatProvider) =
        match value with
        | Decimal d -> format fmt d
        | Float d -> format fmt d
        | String s -> s

    member private __.SFD = __.ToString CultureInfo.InvariantCulture
    member internal __.Value = value
    override __.ToString() = __.ToString CultureInfo.InvariantCulture

    new(value : byte) = JsonNumber(Decimal (decimal value))
    new(value : sbyte) = JsonNumber(Decimal (decimal value))
    new(value : int16) = JsonNumber(Decimal (decimal value))
    new(value : int32) = JsonNumber(Decimal (decimal value))
    new(value : int64) = JsonNumber(Decimal (decimal value))
    new(value : uint16) = JsonNumber(Decimal (decimal value))
    new(value : uint32) = JsonNumber(Decimal (decimal value))
    new(value : uint64) = JsonNumber(Decimal (decimal value))
    new(value : decimal) = JsonNumber(Decimal value)
    new(value : single) = JsonNumber(Float (double value))
    new(value : double) = JsonNumber(Float value)
    new(bigint : bigint) = JsonNumber(String (format CultureInfo.InvariantCulture bigint))
    internal new (value : string) = JsonNumber(String value)

    member __.AsByte ([<O;D(null : FP)>]fmt : IFormatProvider) =
        match value with Decimal d -> byte d | Float d -> byte d | String s -> parse fmt s

    member __.AsSByte ([<O;D(null : FP)>]fmt : IFormatProvider) =
        match value with Decimal d -> sbyte d | Float d -> sbyte d | String s -> parse fmt s

    member __.AsInt16 ([<O;D(null : FP)>]fmt : IFormatProvider) =
        match value with Decimal d -> int16 d | Float d -> int16 d | String s -> parse fmt s

    member __.AsInt32 ([<O;D(null : FP)>]fmt : IFormatProvider) =
        match value with Decimal d -> int32 d | Float d -> int32 d | String s -> parse fmt s

    member __.AsInt64 ([<O;D(null : FP)>]fmt : IFormatProvider) =
        match value with Decimal d -> int64 d | Float d -> int64 d | String s -> parse fmt s

    member __.AsUInt16 ([<O;D(null : FP)>]fmt : IFormatProvider) =
        match value with Decimal d -> uint16 d | Float d -> uint16 d | String s -> parse fmt s

    member __.AsUInt32 ([<O;D(null : FP)>]fmt : IFormatProvider) =
        match value with Decimal d -> uint32 d | Float d -> uint32 d | String s -> parse fmt s

    member __.AsUInt64 ([<O;D(null : FP)>]fmt : IFormatProvider) =
        match value with Decimal d -> uint64 d | Float d -> uint64 d | String s -> parse fmt s

    member __.AsDecimal ([<O;D(null : FP)>]fmt : IFormatProvider) =
        match value with Decimal d -> d | Float d -> decimal d | String s -> parse fmt s

    member __.AsSingle ([<O;D(null : FP)>]fmt : IFormatProvider) =
        match value with Decimal d -> single d | Float d -> single d | String s -> parse fmt s

    member __.AsDouble ([<O;D(null : FP)>]fmt : IFormatProvider) =
        match value with Decimal d -> double d | Float d -> d | String s -> parse fmt s

    member __.AsBigInteger ([<O;D(null : FP)>]fmt : IFormatProvider) =
        match value with Decimal d -> bigint d | Float d -> bigint d | String s -> parse fmt s

    static member op_Explicit(number : JsonNumber) = number.AsByte()
    static member op_Explicit(number : JsonNumber) = number.AsSByte()
    static member op_Explicit(number : JsonNumber) = number.AsInt16()
    static member op_Explicit(number : JsonNumber) = number.AsInt32()
    static member op_Explicit(number : JsonNumber) = number.AsInt64()
    static member op_Explicit(number : JsonNumber) = number.AsUInt16()
    static member op_Explicit(number : JsonNumber) = number.AsUInt32()
    static member op_Explicit(number : JsonNumber) = number.AsUInt64()
    static member op_Explicit(number : JsonNumber) = number.AsDecimal()
    static member op_Explicit(number : JsonNumber) = number.AsSingle()
    static member op_Explicit(number : JsonNumber) = number.AsDouble()
    static member op_Explicit(number : JsonNumber) = number.AsBigInteger()

and private JsonNumberPicklerFactory() =
    inherit PicklerFactoryAttribute<JsonNumber>()
    override __.Create _ =
        { new JsonPickler<JsonNumber> with
            member __.Pickle w n =
                match n.Value with
                | Decimal d -> w.WriteValue d
                | Float d -> w.WriteValue d
                | String s -> w.String s

            member __.UnPickle r =
                let tok = r.NextToken()
                match tok.Tag with
                | JsonTag.Null
                | JsonTag.False -> JsonNumber 0
                | JsonTag.True -> JsonNumber 1
                | JsonTag.Number -> JsonNumber tok.Value 
                | JsonTag.String when isNumber tok.Value -> JsonNumber tok.Value
                | _ -> unexpectedToken tok }

[<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
[<RequireQualifiedAccess; NoEquality; NoComparison>] 
type internal JsonExpr =
    | Null
    | Bool of bool
    | Number of JsonNumber
    | String of string
    | Array of JsonExpr []
    | Object of KeyValuePair<string, JsonExpr> []

[<AutoOpen>]
module private JsonValueImpl =

    let getNodeId (expr : JsonExpr) =
        match expr with
        | JsonExpr.Null -> "null"
        | JsonExpr.Bool _ -> "boolean"
        | JsonExpr.Number _ -> "number"
        | JsonExpr.String _ -> "string"
        | JsonExpr.Object _ -> "object"
        | JsonExpr.Array _ -> "array"

    let rec formatJson (writer : JsonWriter) (value : JsonExpr) =
        match value with
        | JsonExpr.Null -> writer.Null()
        | JsonExpr.Bool b -> writer.Bool b
        | JsonExpr.Number num -> 
            match num.Value with
            | Decimal d -> writer.WriteValue d
            | Float d -> writer.WriteValue d
            | String s -> writer.Number s

        | JsonExpr.String s -> writer.WriteValue s
        | JsonExpr.Array items ->
            writer.StartArray()
            for item in items do formatJson writer item
            writer.EndArray()

        | JsonExpr.Object items ->
            writer.StartObject()
            for item in items do
                writer.Key item.Key
                formatJson writer item.Value
            writer.EndObject()

    let parseJson (reader : JsonReader) =
        let rec parse (token : JsonToken) =
            match token.Tag with
            | JsonTag.Null -> JsonExpr.Null
            | JsonTag.False -> JsonExpr.Bool false
            | JsonTag.True -> JsonExpr.Bool true
            | JsonTag.Number -> JsonExpr.Number (JsonNumber token.Value)
            | JsonTag.String -> JsonExpr.String token.Value
            | JsonTag.StartArray ->
                let ra = ResizeArray()
                let mutable tok = reader.NextToken()
                while tok.Tag <> JsonTag.EndArray do
                    let value = parse tok
                    ra.Add value
                    tok <- reader.NextToken()

                JsonExpr.Array(ra.ToArray())

            | JsonTag.StartObject ->
                let ra = ResizeArray()
                let mutable tok = reader.NextToken()
                while tok.Tag <> JsonTag.EndObject do
                    let key = tok.AsKey()
                    tok <- reader.NextToken()
                    let value = parse tok
                    ra.Add(KeyValuePair(key, value))
                    tok <- reader.NextToken()

                JsonExpr.Object(ra.ToArray())

            | _ -> unexpectedToken token

        parse (reader.NextToken())

    let inline cannotCoerce<'T> (expr : JsonExpr) : 'T =
        let msg = sprintf "Cannot convert JSON %s into value of type '%O'." (getNodeId expr) typeof<'T>
        raise <| FormatException(msg)

    let inline parseNumeric fmt nullVal f expr =
        match expr with
        | JsonExpr.Null -> nullVal
        | JsonExpr.Bool b -> if b then LanguagePrimitives.GenericOne else LanguagePrimitives.GenericZero
        | JsonExpr.Number n -> f n
        | JsonExpr.String s -> parse fmt s
        | _ -> cannotCoerce expr

[<JsonValuePicklerFactory>]
[<Struct; NoEquality; NoComparison; StructuredFormatDisplay("{SFD}")>]
type JsonValue internal (expr : JsonExpr) =
    member internal __.Expr = expr
    member private __.SFD = sprintf "%+A" expr
    override __.ToString () = __.SFD

    member jval.ToJsonString([<O;D(Indent.None)>]indent : Indent, [<O;D(null : FP)>]format : IFormatProvider) =
        let writer = JsonWriter(indent, format)
        formatJson writer jval.Expr
        writer.ToJson()

    static member Parse(json : string, [<O;D(null : FP)>]format : IFormatProvider) =
        let reader = JsonReader(json, format) 
        let expr = parseJson reader
        JsonValue expr

and private JsonValuePicklerFactory() =
    inherit PicklerFactoryAttribute<JsonValue>()
    override __.Create _ =
        { new JsonPickler<JsonValue> with
            member __.Pickle writer jval = formatJson writer jval.Expr
            member __.UnPickle reader = let expr = parseJson reader in JsonValue expr }

type JsonValue with

    static member Null = JsonValue JsonExpr.Null
    static member Bool b = JsonValue(JsonExpr.Bool b)

    static member String(string : string) =
        match string with
        | null -> JsonExpr.Null
        | _ -> JsonExpr.String string
        |> JsonValue

    static member Array (values : seq<JsonValue>) =
        values |> Seq.mapFast (fun jv -> jv.Expr) |> JsonExpr.Array |> JsonValue

    static member Object (values : seq<string * JsonValue>) =
        values |> Seq.mapFast (fun (k,v) -> KeyValuePair(k,v.Expr)) |> JsonExpr.Object |> JsonValue

    static member Number(number : JsonNumber) = number |> JsonExpr.Number |> JsonValue
    static member Number(number : byte) = number |> JsonNumber |> JsonExpr.Number |> JsonValue
    static member Number(number : sbyte) = number |> JsonNumber |> JsonExpr.Number |> JsonValue

    static member Number(number : int16) = number |> JsonNumber |> JsonExpr.Number |> JsonValue
    static member Number(number : int32) = number |> JsonNumber |> JsonExpr.Number |> JsonValue
    static member Number(number : int64) = number |> JsonNumber |> JsonExpr.Number |> JsonValue

    static member Number(number : uint16) = number |> JsonNumber |> JsonExpr.Number |> JsonValue
    static member Number(number : uint32) = number |> JsonNumber |> JsonExpr.Number |> JsonValue
    static member Number(number : uint64) = number |> JsonNumber |> JsonExpr.Number |> JsonValue

    static member Number(number : decimal) = number |> JsonNumber |> JsonExpr.Number |> JsonValue
    static member Number(number : single) = number |> JsonNumber |> JsonExpr.Number |> JsonValue
    static member Number(number : double) = number |> JsonNumber |> JsonExpr.Number |> JsonValue


type JsonValue with
    member jv.AsString([<O;D(null : FP)>]fmt : IFormatProvider) =
        match jv.Expr with
        | JsonExpr.Null -> null
        | JsonExpr.Bool b -> format fmt b
        | JsonExpr.String s -> s
        | JsonExpr.Number n -> format fmt n
        | e -> cannotCoerce e

    member jv.AsBoolean([<O;D(null : FP)>]fmt : IFormatProvider) =
        match jv.Expr with
        | JsonExpr.Null -> false
        | JsonExpr.Bool b -> b
        | JsonExpr.Number n as e ->
            let mutable iv = 0
            if Int32.TryParse(format fmt n, &iv) then iv <> 0
            else cannotCoerce e

        | JsonExpr.String s as e ->
            let mutable bv = false
            let mutable iv = 0
            if Boolean.TryParse(s, &bv) then bv
            elif Int32.TryParse(s, &iv) then iv <> 0
            else cannotCoerce e

        | e -> cannotCoerce e

    member jv.AsInt16([<O;D(null : FP)>]fmt : IFormatProvider) = parseNumeric fmt 0s (fun n -> n.AsInt16 fmt) jv.Expr
    member jv.AsInt32([<O;D(null : FP)>]fmt : IFormatProvider) = parseNumeric fmt 0 (fun n -> n.AsInt32 fmt) jv.Expr
    member jv.AsInt64([<O;D(null : FP)>]fmt : IFormatProvider) = parseNumeric fmt 0L (fun n -> n.AsInt64 fmt) jv.Expr
    member jv.AsUInt16([<O;D(null : FP)>]fmt : IFormatProvider) = parseNumeric fmt 0us (fun n -> n.AsUInt16 fmt) jv.Expr
    member jv.AsUInt32([<O;D(null : FP)>]fmt : IFormatProvider) = parseNumeric fmt 0u (fun n -> n.AsUInt32 fmt) jv.Expr
    member jv.AsUInt64([<O;D(null : FP)>]fmt : IFormatProvider) = parseNumeric fmt 0uL (fun n -> n.AsUInt64 fmt) jv.Expr
    member jv.AsSingle([<O;D(null : FP)>]fmt : IFormatProvider) = parseNumeric fmt Single.NaN (fun n -> n.AsSingle fmt) jv.Expr
    member jv.AsDouble([<O;D(null : FP)>]fmt : IFormatProvider) = parseNumeric fmt Double.NaN (fun n -> n.AsDouble fmt) jv.Expr
    member jv.AsDecimal([<O;D(null : FP)>]fmt : IFormatProvider) = parseNumeric fmt 0M (fun n -> n.AsDecimal fmt) jv.Expr
    member jv.AsBigInteger([<O;D(null : FP)>]fmt : IFormatProvider) = parseNumeric fmt 0I (fun n -> n.AsBigInteger fmt) jv.Expr

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module JsonValue =

    let (|Null|Bool|Number|String|Array|Object|) (json : JsonValue) =
        match json.Expr with
        | JsonExpr.Null -> Null
        | JsonExpr.Bool b -> Bool b
        | JsonExpr.Number n -> Number n
        | JsonExpr.String s -> String s
        | JsonExpr.Array js -> Array(js |> Array.mapFast JsonValue)
        | JsonExpr.Object fs -> Object(fs |> Array.mapFast (fun f -> KeyValuePair(f.Key, JsonValue f.Value)))

    let (|Field|_|) (key : string) (fields : KeyValuePair<string, JsonValue> []) =
        fields |> Array.tryPickFast (function kv when kv.Key = key -> Some kv.Value | _ -> None)

    let (|Element|_|) (index : int) (elements : JsonValue []) =
        if index >= 0 && index < elements.Length then Some elements.[index]
        else None