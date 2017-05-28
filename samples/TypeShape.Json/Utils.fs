[<AutoOpen>]
module internal Vardusia.Utils

open System
open System.Reflection
open System.Globalization

type OAttribute = System.Runtime.InteropServices.OptionalAttribute
type DAttribute = System.Runtime.InteropServices.DefaultParameterValueAttribute

let inline isNull x = match x with null -> true | _ -> false

let inline hasFlag flag value = value &&& flag = flag

let inline append sb t =
    let _ = (^StringBuilder : (member Append : ^t -> ^StringBuilder) (sb, t))
    ()

type FP = IFormatProvider

let inline getDefaultFmt (fmt : IFormatProvider) =
    match fmt with null -> CultureInfo.InvariantCulture :> IFormatProvider | fmt -> fmt

let inline format fmt (input : ^t) =
    (^t : (member ToString : IFormatProvider -> string) (input, fmt))

let inline parse fmt (input : string) =
    (^t : (static member Parse : string * IFormatProvider -> ^t) (input, fmt))

let inline tryParseNumber fmt (result : byref<_>) (input : string) =
    (^t : (static member TryParse : string * NumberStyles * IFormatProvider * byref< ^t> -> bool) 
                                (input, NumberStyles.Any, fmt, &result))

let inline isNumber fmt (value : string) =
    let mutable x = 0.
    Double.TryParse(value, NumberStyles.Any, fmt, &x)

let allInstances = BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic 
let getParameterlessCtor (t : Type) = t.GetConstructor(allInstances, null, [||], [||])

let wrapDelegate<'Dele when 'Dele :> System.Delegate>(methodInfo : MethodInfo) =
    Delegate.CreateDelegate(typeof<'Dele>, methodInfo) :?> 'Dele

module Array =
    let inline clone (array : ^t[]) = array.Clone() :?> ^t[]
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
