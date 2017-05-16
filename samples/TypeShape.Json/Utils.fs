[<AutoOpen>]
module internal Vardusia.Utils

open System
open System.Text
open System.Globalization

let inline append sb t =
    let _ = (^StringBuilder : (member Append : ^t -> ^StringBuilder) (sb, t))
    ()

// Encode characters that are not valid in JS string. The implementation is based
// on https://github.com/mono/mono/blob/master/mcs/class/System.Web/System.Web/HttpUtility.cs
let inline appendEscaped (sb : StringBuilder) (str : string) =
    let n = str.Length - 1
    append sb '"'
    for i = 0 to n do
        let c = str.[i]
        let ci = int c
        if ci >= 0 && ci <= 7 || ci = 11 || ci >= 14 && ci <= 31 then
            sb.AppendFormat("\\u{0:x4}", ci) |> ignore
        else
            match c with
            | '\b' -> append sb "\\b"
            | '\t' -> append sb "\\t"
            | '\n' -> append sb "\\n"
            | '\f' -> append sb "\\f"
            | '\r' -> append sb "\\r"
            | '"'  -> append sb "\\\""
            | '\\' -> append sb "\\\\"
            | s -> append sb s
    append sb '"'

// used http://en.wikipedia.org/wiki/UTF-16#Code_points_U.2B010000_to_U.2B10FFFF as a guide below
let getUnicodeSurrogatePair num =
    // only code points U+010000 to U+10FFFF supported
    // for coversion to UTF16 surrogate pair
    let codePoint = num - 0x010000u
    let HIGH_TEN_BIT_MASK = 0xFFC00u                     // 1111|1111|1100|0000|0000
    let LOW_TEN_BIT_MASK = 0x003FFu                      // 0000|0000|0011|1111|1111
    let leadSurrogate = (codePoint &&& HIGH_TEN_BIT_MASK >>> 10) + 0xD800u
    let trailSurrogate = (codePoint &&& LOW_TEN_BIT_MASK) + 0xDC00u
    char leadSurrogate, char trailSurrogate

let inline getOrInit (ref : byref< ^t>) =
    match ref with
    | null -> ref <- new ^t() ; ref
    | _ -> (^t : (member Clear : unit -> unit) ref) ; ref

let inline format fmt (input : ^t) =
    (^t : (member ToString : IFormatProvider -> string) (input, fmt))

let inline parse fmt (input : string) =
    (^t : (static member Parse : string * IFormatProvider -> ^t) (input, fmt))

let inline tryParseNumber fmt (result : byref<_>) (input : string) =
    (^t : (static member TryParse : string * NumberStyles * IFormatProvider * byref< ^t> -> bool) 
                                (input, NumberStyles.Any, fmt, &result))

module Array =
    let inline mapFast (f : ^a -> ^b) (xs : ^a[]) =
        let n = xs.Length
        let ys = Array.zeroCreate< ^b> n
        for i = 0 to n - 1 do ys.[i] <- f xs.[i]
        ys

    let inline tryPickFast (f : ^a -> ^b option) (xs : ^a[]) =
        let n = xs.Length
        let mutable result = None
        let mutable i = 0
        while i < n && (match result with None -> true | Some _ -> false) do
            result <- f xs.[i]
            i <- i + 1
        result

module Seq =
    let inline mapFast (f : ^a -> ^b) (ts : seq<'a>) =
        let ra = ResizeArray()
        for t in ts do ra.Add (f t)
        ra.ToArray()
