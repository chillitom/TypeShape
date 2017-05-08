#r "bin/Debug/TypeShape.Json.dll"

open System
open TypeShape.Json

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

let r = JsonValue.FromJson <| json.ToJson()

match r with
| JsonValue.Element 0 (JsonValue.Field "bar" x) -> Some x
| _ -> None

let reader = new JsonReader(json.ToJson())

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

for i = 1 to 100000000 do
    let value = ccreate (int64 i)
    let _ = value.GetHashCode()
    ()

for i = 1 to 100000000 do
    let value = screate (int64 i)
    let _ = value.GetHashCode()
    ()