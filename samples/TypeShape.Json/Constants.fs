namespace Vardusia

module internal Constants =
    open System

    let [<Literal>] Null = "null"
    let [<Literal>] NaN = "NaN"
    let [<Literal>] True = "true"
    let [<Literal>] False = "false"
    let [<Literal>] PositiveInfinity = "Infinity"
    let [<Literal>] NegativeInfinity = "-Infinity"

    let [<Literal>] StartObject = '{'
    let [<Literal>] EndObject = '}'
    let [<Literal>] StartArray = '['
    let [<Literal>] EndArray = ']'
    let [<Literal>] Comma = ','
    let [<Literal>] Quote = '"'
    let [<Literal>] Colon = ':'
    let [<Literal>] Space = ' '