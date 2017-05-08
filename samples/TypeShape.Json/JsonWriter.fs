namespace TypeShape.Json

open System
open System.Text
open System.Globalization

type JsonWriter(indent : int, ?fmt : CultureInfo) =
    let mutable depth = 0
    let sb = new StringBuilder()

    member __.Null() = append sb Constants.Null
    member __.Bool b = if b then append sb Constants.True else append sb Constants.False
    member __.Number (n : string) = append sb n
    member __.String (s : string) = appendEscaped sb s
    member __.FieldName(name : string) =
        appendEscaped sb name
        append sb Constants.Colon

    member __.NextValue() = append sb Constants.Comma

    member __.StartObject() = append sb Constants.StartObject
    member __.EndObject() = append sb Constants.EndObject
    member __.StartArray() = append sb Constants.StartArray
    member __.EndArray() = append sb Constants.EndArray

    member __.ToJson() = sb.ToString()