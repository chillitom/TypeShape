namespace Vardusia

open System

type JsonPickler<'T> =
    abstract Pickle : JsonWriter -> 'T -> unit
    abstract UnPickle : JsonReader -> 'T

type IPicklerResolver =
    abstract Resolve<'T> : unit -> JsonPickler<'T>

[<AbstractClass>]
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Property, AllowMultiple = false)>]
type PicklerAttribute<'T>() =
    inherit Attribute()
    abstract Pickle : JsonWriter -> 'T -> unit
    abstract UnPickle : JsonReader -> 'T
    interface JsonPickler<'T> with
        member __.Pickle w t = __.Pickle w t
        member __.UnPickle r = __.UnPickle r

[<AbstractClass>]
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Property, AllowMultiple = false)>]
type PicklerFactoryAttribute<'T>() =
    inherit Attribute()
    abstract Create : IPicklerResolver -> JsonPickler<'T>