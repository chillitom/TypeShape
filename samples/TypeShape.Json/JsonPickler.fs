namespace Vardusia

type JsonPickler<'T> =
    abstract Pickle : JsonWriter -> 'T -> unit
    abstract UnPickle : JsonReader -> 'T

type IPicklerResolver =
    abstract Resolve<'T> : unit -> JsonPickler<'T>
