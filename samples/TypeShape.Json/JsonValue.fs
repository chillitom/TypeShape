namespace Vardusia

open System
open System.Globalization
open System.Collections.Generic
open System.Numerics

[<Struct; StructuredFormatDisplay("{Value}")>]
type internal JsonNumber(value : string) =
    member __.Value = value

[<NoEquality; NoComparison; CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
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

    let formatJson (writer : JsonWriter) (value : JsonExpr) =
        let rec aux value =
            match value with
            | Null ->  writer.Null()
            | Bool b -> writer.Bool b
            | Number num -> writer.Number num.Value
            | String s -> writer.String s
            | Array items -> 
                writer.StartArray()
                let n = items.Length
                if n > 0 then
                    aux items.[0]

                    for i = 1 to n - 1 do
                        writer.NextValue()
                        aux items.[i]

                writer.EndArray()

            | Object items ->
                writer.StartObject()
                let n = items.Length
                if n > 0 then
                    let kv = items.[0]
                    writer.FieldName kv.Key
                    aux kv.Value

                    for i = 1 to n - 1 do
                        writer.NextValue()
                        let kv = items.[i]
                        writer.FieldName kv.Key
                        aux kv.Value

                writer.EndObject()

        do aux value

    let parseJson (reader : JsonReader) =
        let mutable arrBuff = Unchecked.defaultof<ResizeArray<JsonExpr>>
        let mutable recBuf = Unchecked.defaultof<ResizeArray<KeyValuePair<string, JsonExpr>>>
        let inline error (tag : JsonTag) = failwithf "Unexpected JSON tag %O" tag

        let rec parse (token : JsonToken) =
            match token.Tag with
            | JsonTag.Null -> Null
            | JsonTag.False -> Bool false
            | JsonTag.True -> Bool true
            | JsonTag.Number -> Number (JsonNumber token.Value)
            | JsonTag.String -> String token.Value
            | JsonTag.StartArray ->
                let token = reader.NextToken()
                if token.Tag = JsonTag.EndArray then Array [||] else
                let agg = getOrInit &arrBuff
                let value = parse token
                agg.Add value

                let mutable ongoing = true
                while ongoing do
                    let token = reader.NextToken()
                    match token.Tag with
                    | JsonTag.EndArray -> ongoing <- false
                    | JsonTag.Comma ->
                        let token = reader.NextToken()
                        let value = parse token
                        agg.Add value

                    | t -> error t

                Array(agg.ToArray())

            | JsonTag.StartObject ->
                let inline parseField (token : JsonToken) =
                    if token.Tag <> JsonTag.String then error token.Tag
                    let field = token.Value
                    let token = reader.NextToken()
                    if token.Tag <> JsonTag.Colon then error token.Tag
                    let token = reader.NextToken()
                    let value = parse token
                    KeyValuePair(field, value)

                let token = reader.NextToken()
                if token.Tag = JsonTag.EndObject then Object [||] else

                let agg = getOrInit &recBuf
                let field = parseField token
                agg.Add field
                let mutable ongoing = true
                while ongoing do
                    let token = reader.NextToken()
                    match token.Tag with
                    | JsonTag.EndObject -> ongoing <- false
                    | JsonTag.Comma ->
                        let token = reader.NextToken()
                        let field = parseField token
                        agg.Add field

                    | t -> error t

                Object(agg.ToArray())

            | t -> error t

        parse (reader.NextToken())

    let inline cannotCoerce<'T> (expr : JsonExpr) : 'T =
        let msg = sprintf "Cannot coerce JSON %s into value of type '%O'." (getNodeId expr) typeof<'T>
        raise <| FormatException(msg)


[<Struct; NoEquality; NoComparison; StructuredFormatDisplay("{SFD}")>]
type JsonValue internal (expr : JsonExpr) =
    member internal __.Expr = expr
    member private __.SFD = sprintf "%+A" expr
    override __.ToString () = __.SFD
    member __.ToJson(?indent : int) = 
        let writer = JsonWriter(defaultArg indent 0, CultureInfo.InvariantCulture)
        formatJson writer expr
        writer.ToJson()

    static member FromJson(json, ?format : IFormatProvider) =
        let format = defaultArg format (CultureInfo.InvariantCulture :> _)
        let reader = JsonReader(json, format)
        let expr = parseJson reader
        JsonValue expr

type JsonValuePickler() =
    interface JsonPickler<JsonValue> with
        member __.Pickle writer jval = formatJson writer jval.Expr
        member __.UnPickle reader = JsonValue(parseJson reader)

[<AutoOpen>]
module private JsonValueFmtUtils =

    let inline fmtNumeric value =
        let format = (^t : (member ToString : IFormatProvider -> string) (value, CultureInfo.InvariantCulture))
        JsonValue(Number (JsonNumber format))

    let inline fmtFloat value =
        if (^t : (static member IsNaN : ^t -> bool) value) then JsonValue(String Constants.NaN)
        elif (^t : (static member IsPositiveInfinity : ^t -> bool) value) then JsonValue(String Constants.PositiveInfinity)
        elif (^t : (static member IsNegativeInfinity : ^t -> bool) value) then JsonValue(String Constants.NegativeInfinity)
        else fmtNumeric value

    let inline parseNumeric fmt nullVal (expr : JsonExpr) =
        match expr with
        | Null -> nullVal
        | Bool b -> if b then LanguagePrimitives.GenericOne else LanguagePrimitives.GenericZero
        | Number n -> parse fmt n.Value
        | String s -> parse fmt s
        | _ -> cannotCoerce expr

type JsonValue with
    static member Null = JsonValue JsonExpr.Null
    static member Bool b = JsonValue(JsonExpr.Bool b)

    static member String(string : string) =
        match string with
        | null -> JsonExpr.Null
        | _ -> JsonExpr.String string
        |> JsonValue

    static member Array(values : seq<JsonValue>) =
        values |> Seq.mapFast (fun v -> v.Expr) |> JsonExpr.Array |> JsonValue

    static member Object(values : #seq<KeyValuePair<string, JsonValue>>) =
        values |> Seq.mapFast (fun v -> KeyValuePair(v.Key, let e = v.Value in e.Expr)) |> JsonExpr.Object |> JsonValue

    static member Object(values : seq<string * JsonValue>) =
        values |> Seq.mapFast (fun (k,v) -> KeyValuePair(k, v.Expr)) |> JsonExpr.Object |> JsonValue

    static member Number(int16 : int16) = fmtNumeric int16
    static member Number(int32 : int32) = fmtNumeric int32
    static member Number(int64 : int64) = fmtNumeric int64
    static member Number(uint16 : uint16) = fmtNumeric uint16
    static member Number(uint32 : uint32) = fmtNumeric uint32
    static member Number(uint64 : uint64) = fmtNumeric uint64
    static member Number(decimal : decimal) = fmtNumeric decimal
    static member Number(bigint : bigint) = fmtNumeric bigint
    static member Number(single : single) = fmtFloat single
    static member Number(double : double) = fmtFloat double


type JsonValue with
    member __.AsString() =
        match __.Expr with
        | Null -> null
        | Bool b -> b.ToString(CultureInfo.InvariantCulture)
        | String s -> s
        | Number n -> n.Value
        | e -> cannotCoerce e

    member __.AsBoolean() =
        match __.Expr with
        | Null -> false
        | Bool b -> b
        | Number n as e ->
            let mutable iv = 0
            if Int32.TryParse(n.Value, &iv) then iv <> 0
            else cannotCoerce e

        | String s as e ->
            let mutable bv = false
            let mutable iv = 0
            if Boolean.TryParse(s, &bv) then bv
            elif Int32.TryParse(s, &iv) then iv <> 0
            else cannotCoerce e

        | e -> cannotCoerce e

    member __.AsInt16(fmt : IFormatProvider) = parseNumeric fmt 0s __.Expr
    member __.AsInt32(fmt : IFormatProvider) = parseNumeric fmt 0 __.Expr
    member __.AsInt64(fmt : IFormatProvider) = parseNumeric fmt 0L __.Expr
    member __.AsUInt16(fmt : IFormatProvider) = parseNumeric fmt 0us __.Expr
    member __.AsUInt32(fmt : IFormatProvider) = parseNumeric fmt 0u __.Expr
    member __.AsUInt64(fmt : IFormatProvider) = parseNumeric fmt 0uL __.Expr
    member __.AsBigInteger(fmt : IFormatProvider) = parseNumeric fmt 0I __.Expr
    member __.AsDecimal(fmt : IFormatProvider) = parseNumeric fmt 0M __.Expr
    member __.AsDouble(fmt : IFormatProvider) = parseNumeric fmt Double.NaN __.Expr
    member __.AsSingle(fmt : IFormatProvider) = parseNumeric fmt Single.NaN __.Expr

    member __.AsInt16() = parseNumeric CultureInfo.InvariantCulture 0s __.Expr
    member __.AsInt32() = parseNumeric CultureInfo.InvariantCulture 0 __.Expr
    member __.AsInt64() = parseNumeric CultureInfo.InvariantCulture 0L __.Expr
    member __.AsUInt16() = parseNumeric CultureInfo.InvariantCulture 0us __.Expr
    member __.AsUInt32() = parseNumeric CultureInfo.InvariantCulture 0u __.Expr
    member __.AsUInt64() = parseNumeric CultureInfo.InvariantCulture 0uL __.Expr
    member __.AsBigInteger() = parseNumeric CultureInfo.InvariantCulture 0I __.Expr
    member __.AsDecimal() = parseNumeric CultureInfo.InvariantCulture 0M __.Expr
    member __.AsDouble() = parseNumeric CultureInfo.InvariantCulture Double.NaN __.Expr
    member __.AsSingle() = parseNumeric CultureInfo.InvariantCulture Single.NaN __.Expr


[<RequireQualifiedAccess>]
module JsonValue =

    let (|Null|Bool|Number|String|Array|Object|) (json : JsonValue) =
        match json.Expr with
        | JsonExpr.Null -> Null
        | JsonExpr.Bool b -> Bool b
        | JsonExpr.Number n -> Number n.Value
        | JsonExpr.String s -> String s
        | JsonExpr.Array js -> Array(js |> Array.mapFast JsonValue)
        | JsonExpr.Object fs -> Object(fs |> Array.mapFast (fun f -> KeyValuePair(f.Key, JsonValue f.Value)))

    let (|Field|_|) (key : string) (json : JsonValue) =
        match json.Expr with
        | JsonExpr.Object fs -> fs |> Array.tryPickFast (fun kv -> if kv.Key = key then Some (JsonValue kv.Value) else None)
        | _ -> None

    let (|Element|_|) (index : int) (json : JsonValue) =
        match json.Expr with
        | JsonExpr.Array xs when index >= 0 && index < xs.Length -> Some(JsonValue xs.[index])
        | _ -> None