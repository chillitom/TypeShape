#r "bin/Debug/Vardusia.dll"

open System
open Vardusia
open System.Reflection

let json =
    JsonValue.array 
        [ 
            JsonValue.obj [
                "foo" => JsonValue.num 42 ; 
                "bar" => JsonValue.num Double.PositiveInfinity
                "baz" => JsonValue.array [JsonValue.num 1 ; JsonValue.num 2]
                "bazz" => JsonValue.array []
                "asa" => JsonValue.obj []
            ] 
            
            JsonValue.Null

            JsonValue.num 42

            JsonValue.bool true

            JsonValue.string """{ "some" : "json", "here" : true }""" 
        ]


Pickler.pickle Pickler.auto<JsonValue>  json

json.ToJsonString(indent = Indent.Spaces 2) |> printfn "%s"
let string = json.ToJsonString(indent = Indent.Compact) |> JsonValue.Parse

open System.Reflection
type Foo = { A : string option ; B : int list ; FlAgs : System.Reflection.BindingFlags ; Date : DateTimeOffset ; TimeSpan : TimeSpan }

type Tree = { Value : int ; Nested : Tree option }

let pr = Pickler.auto<Tree>

let serializer = new JsonSerializer()

serializer.Pickle({ Value = 2 ; Nested = Some { Value = 2 ; Nested = None } }, indent = Indent.Spaces 2)

Pickler.pickle pr { Value = 2 ; Nested = Some { Value = 2 ; Nested = None } }
|> Pickler.unpickle pr

let pr2 = Pickler.auto<Foo>

let record = { A = Some "f00"; B = [1 .. 10] ; Date = DateTimeOffset.Now ; TimeSpan = TimeSpan.FromHours 48.1231 ; FlAgs = BindingFlags.NonPublic ||| BindingFlags.Instance }
Pickler.pickle pr2 record |> Pickler.unpickle pr2

Console.WriteLine string
let r = JsonValue.Parse <| json.ToJsonString(indent = Indent.Spaces 2)

type Union =
    | A
    | B of foo:int * bar:string
    | C of byte[]
    | D of int


let up = Pickler.auto<Union list>

Pickler.pickle up [D 42 ; A ;A; B(2,"foo") ; C [|1uy .. 10uy|]]
|> Pickler.unpickle up

Pickler.pickle Pickler.auto [Some 42; None; Some 12]

type P =
    | Z
    | S of next:P

let pp = Pickler.auto<P list>

[Z ; S (S Z)] |> Pickler.pickle pp

type ClassU =
    | CA of int64
    | CB of int64
    | CC of int64
    | CD of int64
    | CE of int64
    | CF of int64
    | CG of int64
    | CH of int64

[<Struct>]
type StructU =
    | SA of A:int64
    | SB of B:int64
    | SC of C:int64
    | SD of D:int64
    | SE of E:int64
    | SF of F:int64
    | SG of G:int64
    | SH of H:int64

sizeof<ClassU>
sizeof<StructU>

let ccreate i = 
    match i % 8L with 
    | 0L -> CA i | 1L -> CB i | 2L -> CC i | 3L -> CD i
    | 4L -> CE i | 5L -> CF i | 6L -> CG i | 7L -> CH i

let screate i = 
    match i % 8L with 
    | 0L -> SA i | 1L -> SB i | 2L -> SC i | 3L -> SD i
    | 4L -> SE i | 5L -> SF i | 6L -> SG i | 7L -> SH i

#time "on"

// Real: 00:00:01.616, CPU: 00:00:01.578, GC gen0: 1525, gen1: 1, gen2: 0
for i = 1 to 100000000 do
    let value = ccreate (int64 i)
    let _ = value.GetHashCode()
    ()

// Real: 00:00:02.421, CPU: 00:00:02.421, GC gen0: 0, gen1: 0, gen2: 0
for i = 1 to 100000000 do
    let value = screate (int64 i)
    let _ = value.GetHashCode()
    ()


open System.Collections.Generic

let dict = new Dictionary<ClassU, int>()
for i = 1 to 10000000 do
    let value = ccreate (int64 i)
    dict.Add(value, i)

let dict' = new Dictionary<StructU, int>()
for i = 1 to 10000000 do
    let value = screate (int64 i)
    dict'.Add(value, i)



#time "on"

open FSharp.NativeInterop

let n = 5000000
let string = String(' ', n)

let test1 () =
    let string = string
    let mutable x = 0
    for i = 0 to n - 1 do
        x <- x + int string.[i]
    x

let test2 () =
    let string = string
    let mutable x = 0
    use ptr = fixed string
    for i = 0 to n - 1 do 
        x <- x + int (NativePtr.get ptr i)
    x

for i = 1 to 100 do test1() |> ignore
for i = 1 to 100 do test2() |> ignore