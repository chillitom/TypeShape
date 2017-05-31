#r "../../bin/TypeShape.dll"
#r "../../packages/Newtonsoft.Json/lib/net45/Newtonsoft.Json.dll"
open Newtonsoft.Json
open System
open System.Globalization
open System.Collections.Generic
open Newtonsoft.Json

let inline format fmt (input : ^t) =
    (^t : (member ToString : IFormatProvider -> string) (input, fmt))

let inline parse fmt (input : string) =
    (^t : (static member Parse : string * IFormatProvider -> ^t) (input, fmt))

[<AbstractClass>]
type JsonPickler<'T>() =
    inherit JsonConverter()

    abstract Write : writer:JsonWriter * serializer:JsonSerializer * source:'T  -> unit
    abstract Read : reader:JsonReader * serializer:JsonSerializer -> 'T

    override __.CanConvert t = t = typeof<'T>
    override __.CanRead = true
    override __.CanWrite = true

    override __.WriteJson(writer, source : obj, serialize : JsonSerializer) =
        __.Write(writer, serialize, source :?> 'T)

    override __.ReadJson(reader : JsonReader, _, _, serializer : JsonSerializer) =
        __.Read(reader, serializer) :> obj


[<AbstractClass>]
type JsonIsomorphism<'T, 'U>() =
    inherit JsonConverter()

    abstract ToJson   : 'T -> 'U
    abstract FromJson : 'U -> 'T

    override __.CanConvert t = t = typeof<'T>
    override __.CanRead = true
    override __.CanWrite = true
    override __.WriteJson(writer : JsonWriter, source : obj, serializer : JsonSerializer) =
        let target = __.ToJson(source :?> 'T)
        serializer.Serialize(writer, target, typeof<'U>)

    override __.ReadJson(reader : JsonReader, _, _, serializer : JsonSerializer) =
        let target = serializer.Deserialize<'U>(reader)
        __.FromJson target :> obj

[<JsonConverter(typeof<FooConverter>)>]
type Foo = { value : int }

and FooConverter() =
    inherit JsonIsomorphism<Foo, int>()
    override __.ToJson foo = foo.value
    override __.FromJson i = { value = i }

JsonConvert.SerializeObject { value = 42 }
|> JsonConvert.DeserializeObject<Foo>

type internal JsonNumberVal =
    | Decimal of decimal
    | Float of double
    | Formatted of string * CultureInfo

[<JsonConverter(typeof<JsonNumberConverter>)>]
[<Struct; StructuredFormatDisplay("{SFD}"); NoEquality; NoComparison>]
type JsonNumber internal (value : JsonNumberVal) =
    member __.ToString(fmt : IFormatProvider) =
        match value with
        | Decimal d -> format fmt d
        | Float d -> format fmt d
        | Formatted (s,_) -> s

    member internal __.Value = value
    member private __.SFD = __.ToString CultureInfo.InvariantCulture
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

and JsonNumberConverter() =
    inherit JsonPickler<JsonNumber>()
    override __.Write(writer : JsonWriter, _ : JsonSerializer, number : JsonNumber) =
        match number.Value with
        | Decimal decimal -> writer.WriteValue decimal
        | Float float -> writer.WriteValue float
        | Formatted (string,_) -> writer.WriteRawValue string

    override __.Read(reader : JsonReader, _ : JsonSerializer) =
        match reader.Value with
        | :? int as i -> Decimal(decimal i)
        | :? int64 as i -> Decimal(decimal i)
        | :? float as f -> Float f
        | :? bigint as i -> Formatted(format CultureInfo.InvariantCulture i, CultureInfo.InvariantCulture)
        | :? string as s -> Formatted(s, reader.Culture)
        | null -> Decimal 0M
        | o -> failwithf "unexpected %O : %O" o (o.GetType())
        |> JsonNumber


[<JsonConverter(typeof<JsonValuePickler>)>]
[<RequireQualifiedAccess; NoEquality; NoComparison>]
type JsonValue =
    | Null
    | Bool of bool
    | Number of JsonNumber
    | String of string
    | Array of JsonValue []
    | Object of KeyValuePair<string, JsonValue> []

and JsonValuePickler() =
    inherit JsonPickler<JsonValue>()
    let rec write (writer : JsonWriter) (value : JsonValue) =
        match value with
        | JsonValue.Null -> writer.WriteNull()
        | JsonValue.Bool b -> writer.WriteValue b
        | JsonValue.Number n ->
            match n.Value with
            | Decimal d -> writer.WriteValue d
            | Float d -> writer.WriteValue d
            | Formatted(s,_) -> writer.WriteRaw s
        | JsonValue.String s -> writer.WriteValue s
        | JsonValue.Array elems ->
            writer.WriteStartArray()
            for e in elems do write writer e
            writer.WriteEndArray()
        | JsonValue.Object fields ->
            writer.WriteStartObject()
            for f in fields do
                writer.WritePropertyName f.Key
                write writer f.Value
            writer.WriteEndObject()

    let rec read (reader : JsonReader) =
        match reader.TokenType with
        | JsonToken.Null -> JsonValue.Null
        | JsonToken.Boolean -> JsonValue.Bool(reader.Value :?> bool)
        | JsonToken.Float -> JsonValue.Number(JsonNumber(Float (reader.Value :?> float)))
        | JsonToken.Integer -> 
            match reader.Value with
            | :? int as i -> Decimal(decimal i)
            | :? int64 as i -> Decimal(decimal i)
            | :? bigint as i -> Decimal(decimal i)
            | o -> Decimal(decimal (Convert.ToInt64 o))
            |> JsonNumber
            |> JsonValue.Number
        | JsonToken.Date -> 
            match reader.Value with 
            | :? DateTime as d -> format reader.Culture d
            | :? DateTimeOffset as d -> format reader.Culture d
            | o -> format reader.Culture (Convert.ToDateTime o)
            |> JsonValue.String
        | JsonToken.String -> JsonValue.String(reader.Value :?> string)
        | JsonToken.Bytes -> JsonValue.String(reader.Value :?> byte[] |> Convert.ToBase64String)
        | JsonToken.StartArray ->
            let ra = ResizeArray()
            while reader.Read() && reader.TokenType <> JsonToken.EndArray do
                ra.Add(read reader)
            JsonValue.Array(ra.ToArray())

        | JsonToken.StartObject ->
            let ra = ResizeArray()
            while reader.Read() && reader.TokenType <> JsonToken.EndObject do
                let key = reader.Value :?> string
                if not <| reader.Read() then failwith "Unexpected EOF reading JSON"
                let value = read reader
                ra.Add(KeyValuePair(key,value))

            JsonValue.Object(ra.ToArray())

        | JsonToken.Comment ->
            if not <| reader.Read() then failwith "Unexpected EOF reading JSON"
            read reader

        | JsonToken.Raw -> JsonValue.String(reader.Value :?> string)
        | tok -> failwithf "unrecognized JSON token %O" tok

    override __.Write(writer,_,value) = write writer value
    override __.Read(reader,_) = read reader

open System
let inline (=>) k v = KeyValuePair(k,v)

let json =
    JsonValue.Array [|
        JsonValue.Null ; 
        JsonValue.String "asda" ; 
        JsonValue.String (string DateTimeOffset.Now) ;
        JsonValue.String (Convert.ToBase64String [|1uy .. 10uy|])
        JsonValue.Bool false ; 
        JsonValue.Object [|"foo" => JsonValue.Null|]
    |]

JsonConvert.SerializeObject [json;json]
|> JsonConvert.DeserializeObject<JsonValue list>

let r = new JsonTextReader(new System.IO.StringReader("{ \"foo\" : 12 }"))

r.Read()
r.TokenType, r.Value


JsonConvert.DeserializeObject<JsonValue> "{ \"foo\" : 12, \"bar\" : [1,2, \"poutsa\"] }"