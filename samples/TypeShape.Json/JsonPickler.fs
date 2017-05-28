namespace Vardusia

open System

type JsonPickler<'T> =
    abstract Pickle : JsonWriter -> 'T -> unit
    abstract UnPickle : JsonReader -> 'T

type IPicklerResolver =
    abstract Resolve<'T> : unit -> JsonPickler<'T>

type IPicklerFactory<'T> =
    abstract Create<'T> : IPicklerResolver -> JsonPickler<'T>

[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Struct, AllowMultiple = false)>]
type JsonTypeAttribute() =
    inherit Attribute()
    member val RequiredFields = false with get,set
    member val Pickler : Type = null with get,set

[<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
type JsonPropertyAttribute() =
    inherit Attribute()
    member val Label : string = null with get,set
    member val Pickler : Type = null with get,set
    member val Required = false with get,set