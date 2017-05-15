#r "bin/Debug/Vardusia.dll"

open System
open Vardusia

let json =
    JsonValue.Array 
        [ 
            JsonValue.Object [
                "foo", JsonValue.Number 42 ; 
                "bar", JsonValue.Number Double.PositiveInfinity
            ] 
            
            JsonValue.Null 

            JsonValue.Number 42

            JsonValue.Bool true

            JsonValue.String """{ "some" : "json", "here" : true }""" 
        ]

let string = json.ToJson(indent = 4)

Console.WriteLine string
let r = JsonValue.FromJson <| json.ToJson(indent = 4)

match r with
| JsonValue.Element 0 (JsonValue.Field "bar" x) -> Some x
| _ -> None

reader.NextToken()

open System

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