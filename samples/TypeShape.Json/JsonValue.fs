namespace Vardusia

open System
open System.Globalization
open System.Collections.Generic
open System.Numerics

[<NoEquality; NoComparison>]
type internal JsonNumberVal =
    | Decimal of decimal
    | Float of double
    | Formatted of string * IFormatProvider

[<JsonNumberPicklerFactory>]
[<Struct; StructuredFormatDisplay("{SFD}"); NoEquality; NoComparison>]
type JsonNumber internal (value : JsonNumberVal) =
    member __.ToString(fmt : IFormatProvider) =
        match value with
        | Decimal d -> format fmt d
        | Float d -> format fmt d
        | Formatted (s,_) -> s

    member private __.SFD = __.ToString CultureInfo.InvariantCulture
    member internal __.Value = value
    override __.ToString() = __.ToString CultureInfo.InvariantCulture

    new (value : string, [<O;D(null : FP)>]fmt : IFormatProvider) = 
        let fmt = getDefaultFmt fmt
        if not <| isNumber fmt value then
            invalidArg "value" "JsonValue: input string was not a valid number representation."

        JsonNumber(Formatted (value, fmt))

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
    new(bigint : bigint) = 
        let fmt = CultureInfo.InvariantCulture
        JsonNumber(Formatted (format fmt bigint, fmt))

    static member op_Explicit(number : JsonNumber) =
        match number.Value with Decimal d -> byte d | Float d -> byte d | Formatted(s,fmt) -> parse fmt s

    static member op_Explicit(number : JsonNumber) =
        match number.Value with Decimal d -> sbyte d | Float d -> sbyte d | Formatted(s,fmt) -> parse fmt s

    static member op_Explicit(number : JsonNumber) =
        match number.Value with Decimal d -> int16 d | Float d -> int16 d | Formatted(s,fmt) -> parse fmt s

    static member op_Explicit(number : JsonNumber) =
        match number.Value with Decimal d -> int32 d | Float d -> int32 d | Formatted(s,fmt) -> parse fmt s

    static member op_Explicit(number : JsonNumber) =
        match number.Value with Decimal d -> int64 d | Float d -> int64 d | Formatted(s,fmt) -> parse fmt s

    static member op_Explicit(number : JsonNumber) =
        match number.Value with Decimal d -> uint16 d | Float d -> uint16 d | Formatted(s,fmt) -> parse fmt s

    static member op_Explicit(number : JsonNumber) =
        match number.Value with Decimal d -> uint32 d | Float d -> uint32 d | Formatted(s,fmt) -> parse fmt s

    static member op_Explicit(number : JsonNumber) =
        match number.Value with Decimal d -> uint64 d | Float d -> uint64 d | Formatted(s,fmt) -> parse fmt s

    static member op_Explicit(number : JsonNumber) =
        match number.Value with Decimal d -> d | Float d -> decimal d | Formatted(s,fmt) -> parse fmt s

    static member op_Explicit(number : JsonNumber) =
        match number.Value with Decimal d -> single d | Float d -> single d | Formatted(s,fmt) -> parse fmt s

    static member op_Explicit(number : JsonNumber) =
        match number.Value with Decimal d -> double d | Float d -> d | Formatted(s,fmt) -> parse fmt s

    static member op_Explicit(number : JsonNumber) =
        match number.Value with Decimal d -> bigint d | Float d -> bigint d | Formatted(s,fmt) -> parse fmt s

and private JsonNumberPicklerFactory() =
    inherit PicklerFactoryAttribute<JsonNumber>()
    override __.Create _ =
        { new JsonPickler<JsonNumber> with
            member __.Pickle w n =
                match n.Value with
                | Decimal d -> w.WriteValue d
                | Float d -> w.WriteValue d
                | Formatted (s,_) -> w.WriteString s

            member __.UnPickle r =
                let tok = r.NextToken()
                match tok.Tag with
                | JsonTag.Null
                | JsonTag.False -> JsonNumber 0
                | JsonTag.True -> JsonNumber 1
                | JsonTag.Number -> JsonNumber(Formatted (tok.Value, r.Format))
                | JsonTag.String when isNumber r.Format tok.Value -> JsonNumber(Formatted (tok.Value, r.Format))
                | _ -> unexpectedToken tok }

[<JsonValuePicklerFactory>]
[<RequireQualifiedAccess; NoEquality; NoComparison>]
type JsonValue =
    | Null
    | Bool of bool
    | Number of JsonNumber
    | String of string
    | Array of JsonValue []
    | Object of KeyValuePair<string, JsonValue> []

and JsonValuePickler() =
    let rec pickle (writer : JsonWriter) (value : JsonValue) =
        match value with
        | JsonValue.Null -> writer.WriteNull()
        | JsonValue.Bool b -> writer.WriteBool b
        | JsonValue.Number num -> 
            match num.Value with
            | Decimal d -> writer.WriteValue d
            | Float d -> writer.WriteValue d
            | Formatted (s,_) -> writer.WriteNumber s

        | JsonValue.String s -> writer.WriteValue s
        | JsonValue.Array items ->
            writer.WriteStartArray()
            for item in items do pickle writer item
            writer.WriteEndArray()

        | JsonValue.Object items ->
            writer.WriteStartObject()
            for item in items do
                writer.WriteKey item.Key
                pickle writer item.Value
            writer.WriteEndObject()

    let rec unpickle (reader : JsonReader) (token : JsonToken) =
        match token.Tag with
        | JsonTag.Null -> JsonValue.Null
        | JsonTag.False -> JsonValue.Bool false
        | JsonTag.True -> JsonValue.Bool true
        | JsonTag.Number -> JsonValue.Number (JsonNumber (Formatted (token.Value, reader.Format)))
        | JsonTag.String -> JsonValue.String token.Value
        | JsonTag.StartArray ->
            let ra = ResizeArray()
            let mutable tok = reader.NextToken()
            while tok.Tag <> JsonTag.EndArray do
                let value = unpickle reader tok
                ra.Add value
                tok <- reader.NextToken()

            JsonValue.Array(ra.ToArray())

        | JsonTag.StartObject ->
            let ra = ResizeArray()
            let mutable tok = reader.NextToken()
            while tok.Tag <> JsonTag.EndObject do
                let key = tok.AsKey()
                tok <- reader.NextToken()
                let value = unpickle reader tok
                ra.Add(KeyValuePair(key, value))
                tok <- reader.NextToken()

            JsonValue.Object(ra.ToArray())

        | _ -> unexpectedToken token

    interface JsonPickler<JsonValue> with
        member __.Pickle writer value = pickle writer value
        member __.UnPickle reader = let tok = reader.NextToken() in unpickle reader tok

and private JsonValuePicklerFactory() =
    inherit PicklerFactoryAttribute<JsonValue>()
    override __.Create _ = JsonValuePickler() :> _

[<AutoOpen>]
module private JsonValueImpl =

    let pickler = JsonValuePickler() :> JsonPickler<_>

    let getNodeId (expr : JsonValue) =
        match expr with
        | JsonValue.Null -> "null"
        | JsonValue.Bool _ -> "boolean"
        | JsonValue.Number _ -> "number"
        | JsonValue.String _ -> "string"
        | JsonValue.Object _ -> "object"
        | JsonValue.Array _ -> "array"

    let inline cannotCoerce<'T> (expr : JsonValue) : 'T =
        let msg = sprintf "Cannot convert JSON %s into value of type '%O'." (getNodeId expr) typeof<'T>
        raise <| FormatException(msg)

    let inline parseNumeric fmt nullVal f expr =
        match expr with
        | JsonValue.Null -> nullVal
        | JsonValue.Bool b -> if b then LanguagePrimitives.GenericOne else LanguagePrimitives.GenericZero
        | JsonValue.Number n -> f n
        | JsonValue.String s -> parse (getDefaultFmt fmt) s
        | _ -> cannotCoerce expr


type JsonValue with

    member jval.ToJsonString([<O;D(2)>]indent : int, [<O;D(null : FP)>]format : IFormatProvider) : string =
        let writer = JsonWriter(indent, format)
        pickler.Pickle writer jval
        writer.ToJsonString()

    static member Parse(json : string, [<O;D(null : FP)>]format : IFormatProvider) : JsonValue =
        let reader = JsonReader(json, format) 
        pickler.UnPickle reader


type JsonValue with
    static member inline jnull = JsonValue.Null
    static member inline bool (bool : bool) = JsonValue.Bool bool
    static member inline string (string : string) = JsonValue.String string

    static member inline num (number : byte) = JsonValue.Number(JsonNumber number)
    static member inline num (number : sbyte) = JsonValue.Number(JsonNumber number)
    static member inline num (number : int16) = JsonValue.Number(JsonNumber number)
    static member inline num (number : int32) = JsonValue.Number(JsonNumber number)
    static member inline num (number : int64) = JsonValue.Number(JsonNumber number)
    static member inline num (number : uint16) = JsonValue.Number(JsonNumber number)
    static member inline num (number : uint32) = JsonValue.Number(JsonNumber number)
    static member inline num (number : uint64) = JsonValue.Number(JsonNumber number)
    static member inline num (number : single) = JsonValue.Number(JsonNumber number)
    static member inline num (number : double) = JsonValue.Number(JsonNumber number)
    static member inline num (number : decimal) = JsonValue.Number(JsonNumber number)
    static member inline num (number : bigint) = JsonValue.Number(JsonNumber number)

    static member inline array (elements : seq<JsonValue>) = JsonValue.Array(Seq.toArray elements)
    static member inline obj (fields : seq<KeyValuePair<string, JsonValue>>) = JsonValue.Object(Seq.toArray fields)

type JsonValue with
    member jv.AsString([<O;D(null : FP)>]fmt : IFormatProvider) =
        match jv with
        | JsonValue.Null -> null
        | JsonValue.Bool b -> format fmt b
        | JsonValue.String s -> s
        | JsonValue.Number n -> format fmt n
        | e -> cannotCoerce e

    member jv.AsBoolean([<O;D(null : FP)>]fmt : IFormatProvider) =
        match jv with
        | JsonValue.Null -> false
        | JsonValue.Bool b -> b
        | JsonValue.Number n as e ->
            let mutable iv = 0
            if Int32.TryParse(format fmt n, &iv) then iv <> 0
            else cannotCoerce e

        | JsonValue.String s as e ->
            let mutable bv = false
            let mutable iv = 0
            if Boolean.TryParse(s, &bv) then bv
            elif Int32.TryParse(s, &iv) then iv <> 0
            else cannotCoerce e

        | e -> cannotCoerce e

    member jv.AsInt16([<O;D(null : FP)>]fmt : IFormatProvider) = parseNumeric fmt 0s int16 jv
    member jv.AsInt32([<O;D(null : FP)>]fmt : IFormatProvider) = parseNumeric fmt 0 int32 jv
    member jv.AsInt64([<O;D(null : FP)>]fmt : IFormatProvider) = parseNumeric fmt 0L int64 jv
    member jv.AsUInt16([<O;D(null : FP)>]fmt : IFormatProvider) = parseNumeric fmt 0us uint16 jv
    member jv.AsUInt32([<O;D(null : FP)>]fmt : IFormatProvider) = parseNumeric fmt 0u uint32 jv
    member jv.AsUInt64([<O;D(null : FP)>]fmt : IFormatProvider) = parseNumeric fmt 0uL uint64 jv
    member jv.AsSingle([<O;D(null : FP)>]fmt : IFormatProvider) = parseNumeric fmt Single.NaN single jv
    member jv.AsDouble([<O;D(null : FP)>]fmt : IFormatProvider) = parseNumeric fmt Double.NaN double jv
    member jv.AsDecimal([<O;D(null : FP)>]fmt : IFormatProvider) = parseNumeric fmt 0M decimal jv
    member jv.AsBigInteger([<O;D(null : FP)>]fmt : IFormatProvider) = parseNumeric fmt 0I JsonNumber.op_Explicit jv

[<AutoOpen>]
module JsonValueHelpers =

    type jnum = JsonNumber
    type jval = JsonValue

    let inline (=>) key value = KeyValuePair(key, value)

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module JsonValue =

        let (|Field|_|) (key : string) (fields : KeyValuePair<string, JsonValue> []) =
            fields |> Array.tryPickFast (function kv when kv.Key = key -> Some kv.Value | _ -> None)

        let (|Element|_|) (index : int) (elements : JsonValue []) =
            if index >= 0 && index < elements.Length then Some elements.[index]
            else None