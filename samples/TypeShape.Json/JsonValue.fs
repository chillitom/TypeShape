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

    internal new (value : string) = JsonNumber(String value)
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
                | JsonTag.String when isNumber r.Format tok.Value -> JsonNumber tok.Value
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
        | JsonValue.Null -> writer.Null()
        | JsonValue.Bool b -> writer.Bool b
        | JsonValue.Number num -> 
            match num.Value with
            | Decimal d -> writer.WriteValue d
            | Float d -> writer.WriteValue d
            | String s -> writer.Number s

        | JsonValue.String s -> writer.WriteValue s
        | JsonValue.Array items ->
            writer.StartArray()
            for item in items do pickle writer item
            writer.EndArray()

        | JsonValue.Object items ->
            writer.StartObject()
            for item in items do
                writer.Key item.Key
                pickle writer item.Value
            writer.EndObject()

    let rec unpickle (reader : JsonReader) (token : JsonToken) =
        match token.Tag with
        | JsonTag.Null -> JsonValue.Null
        | JsonTag.False -> JsonValue.Bool false
        | JsonTag.True -> JsonValue.Bool true
        | JsonTag.Number -> JsonValue.Number (JsonNumber token.Value)
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
        | JsonValue.String s -> parse fmt s
        | _ -> cannotCoerce expr


type JsonValue with

    member jval.ToJsonString([<O;D(Indent.None)>]indent : Indent, [<O;D(null : FP)>]format : IFormatProvider) : string =
        let writer = JsonWriter(indent, format)
        pickler.Pickle writer jval
        writer.ToJson()

    static member Parse(json : string, [<O;D(null : FP)>]format : IFormatProvider) : JsonValue =
        let reader = JsonReader(json, format) 
        pickler.UnPickle reader


type JsonValue with
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

    member jv.AsInt16([<O;D(null : FP)>]fmt : IFormatProvider) = parseNumeric fmt 0s (fun n -> n.AsInt16 fmt) jv
    member jv.AsInt32([<O;D(null : FP)>]fmt : IFormatProvider) = parseNumeric fmt 0 (fun n -> n.AsInt32 fmt) jv
    member jv.AsInt64([<O;D(null : FP)>]fmt : IFormatProvider) = parseNumeric fmt 0L (fun n -> n.AsInt64 fmt) jv
    member jv.AsUInt16([<O;D(null : FP)>]fmt : IFormatProvider) = parseNumeric fmt 0us (fun n -> n.AsUInt16 fmt) jv
    member jv.AsUInt32([<O;D(null : FP)>]fmt : IFormatProvider) = parseNumeric fmt 0u (fun n -> n.AsUInt32 fmt) jv
    member jv.AsUInt64([<O;D(null : FP)>]fmt : IFormatProvider) = parseNumeric fmt 0uL (fun n -> n.AsUInt64 fmt) jv
    member jv.AsSingle([<O;D(null : FP)>]fmt : IFormatProvider) = parseNumeric fmt Single.NaN (fun n -> n.AsSingle fmt) jv
    member jv.AsDouble([<O;D(null : FP)>]fmt : IFormatProvider) = parseNumeric fmt Double.NaN (fun n -> n.AsDouble fmt) jv
    member jv.AsDecimal([<O;D(null : FP)>]fmt : IFormatProvider) = parseNumeric fmt 0M (fun n -> n.AsDecimal fmt) jv
    member jv.AsBigInteger([<O;D(null : FP)>]fmt : IFormatProvider) = parseNumeric fmt 0I (fun n -> n.AsBigInteger fmt) jv

[<AutoOpen>]
module JsonValueHelpers =

    let inline (=>) key value = KeyValuePair(key, value)

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module JsonValue =

        let (|Field|_|) (key : string) (fields : KeyValuePair<string, JsonValue> []) =
            fields |> Array.tryPickFast (function kv when kv.Key = key -> Some kv.Value | _ -> None)

        let (|Element|_|) (index : int) (elements : JsonValue []) =
            if index >= 0 && index < elements.Length then Some elements.[index]
            else None