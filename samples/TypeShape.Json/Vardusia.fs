namespace Vardusia

open System
open System.Globalization

open TypeShape
open TypeShape_Utils

open Vardusia.PrimitivePicklers
open Vardusia.GenericPicklers
open Vardusia.PicklerGeneration

type PicklerCacheBuilder() =
    let factories = new TypeCache()
    member internal __.Factories = factories
    member __.Count = factories.Count
    member __.Types = factories.Keys
    member __.Register(generator : IPicklerResolver -> JsonPickler<'T>) = factories.ForceAdd generator
    member __.TryRegister(generator : IPicklerResolver -> JsonPickler<'T>) = factories.TryAdd generator
    member __.IsFactoryRegistered<'T>() = factories.ContainsKey<IPicklerResolver -> JsonPickler<'T>>()

    member pcb.TryRegister(pickler : JsonPickler<'T>) = pcb.TryRegister(fun _ -> pickler)
    member pcb.Register(generator : JsonPickler<'T>) = pcb.Register (fun _ -> generator)
    member pcb.ToPicklerCache() = new PicklerCache(pcb)
    member pcb.IsFactoryRegistered(t : Type) = 
        TypeShape.Create(t).Accept { 
            new ITypeShapeVisitor<bool> with 
                member __.Visit<'T>() = 
                    pcb.IsFactoryRegistered<IPicklerResolver -> JsonPickler<'T>>() }


and PicklerCache(?builder : PicklerCacheBuilder) =
    let cache = new TypeCache()
    let picklerFactories = match builder with None -> new TypeCache() | Some b -> b.Factories.Clone()

    static let defaultCache = new PicklerCache()

    member __.TotalPicklers = cache.Count
    member __.PicklerTypes = cache.Keys
    member __.Resolve<'T> () = resolve<'T> cache picklerFactories

    interface IPicklerResolver with
        member __.Resolve<'T> () = __.Resolve<'T> ()

    static member internal DefaultCache = defaultCache


type JsonSerializer(?cache : PicklerCache, [<O;D(null)>]?indent : Indent, [<O;D(null)>]?format : IFormatProvider) =
    let cache = defaultArg cache PicklerCache.DefaultCache

    member val Indent = defaultArg indent Indent.None with get, set
    member val Format = defaultArg format (CultureInfo.InvariantCulture :> _) with get, set

    member __.Pickle<'T>(value : 'T, [<O;D(null)>]?pickler : JsonPickler<'T>, [<O;D(null)>]?indent : Indent, [<O;D(null)>]?format : IFormatProvider) : string =
        let pickler = match pickler with None -> cache.Resolve<'T>() | Some p -> p
        let format = defaultArg format __.Format
        let indent = defaultArg indent __.Indent
        let writer = new JsonWriter(indent, format)
        pickler.Pickle writer value
        writer.ToJson()

    member __.UnPickle<'T>(json : string, [<O;D(null)>]?pickler : JsonPickler<'T>, [<O;D(null)>]?format : IFormatProvider) : 'T =
        let pickler = match pickler with None -> cache.Resolve<'T>() | Some p -> p
        let format = defaultArg format __.Format
        let reader = new JsonReader(json, format)
        pickler.UnPickle reader


[<RequireQualifiedAccess>]
module Pickler =
    
    let auto<'T> = PicklerCache.DefaultCache.Resolve<'T>()

    let private serializer = JsonSerializer()
    let pickle (pickler : JsonPickler<'T>) (value : 'T) : string = serializer.Pickle(value, pickler = pickler)
    let unpickle (pickler : JsonPickler<'T>) (json : string) : 'T = serializer.UnPickle(json, pickler = pickler)

    let unit = UnitPickler<unit>() :> JsonPickler<_>

    let bool = BoolPickler() :> JsonPickler<_>
    let byte = BytePickler() :> JsonPickler<_>
    let sbyte = SBytePickler() :> JsonPickler<_>

    let uint16   = UInt16Pickler() :> JsonPickler<_>
    let uint     = UInt32Pickler() :> JsonPickler<_>
    let uint64   = UInt64Pickler() :> JsonPickler<_>

    let int16   = Int16Pickler() :> JsonPickler<_>
    let int     = Int32Pickler() :> JsonPickler<_>
    let int64   = Int64Pickler() :> JsonPickler<_>

    let single = SinglePickler() :> JsonPickler<_>
    let double = DoublePickler() :> JsonPickler<_>
    let decimal = DecimalPickler() :> JsonPickler<_>
    let float = double

    let timespan = TimeSpanPickler() :> JsonPickler<_>
    let datetime = DateTimePickler() :> JsonPickler<_>
    let datetimeOffset = DateTimeOffsetPickler() :> JsonPickler<_>

    let option t = FSharpOptionPickler<'T>(t) :> JsonPickler<_>
    let nullable t = NullablePickler<'T>(t) :> JsonPickler<_>
    let enum () = EnumStringPickler<'T,_>() :> JsonPickler<_>

    let array t = mkArrayPickler t :> JsonPickler<_>
    let list t = mkListPickler t :> JsonPickler<_>
    let map t = mkMapPickler t :> JsonPickler<_>
    let set t = mkSetPickler t :> JsonPickler<_>

    let wrap f g p = IsomorphismPickler<'T, 'S>(f,g,p) :> JsonPickler<_>

    let createUninitialized<'T> () =
        let pRef = ref Unchecked.defaultof<JsonPickler<'T>>
        let inline read() =
            match pRef.Value with
            | p when obj.ReferenceEquals(p, null) ->
                sprintf "Recursive pickler for type '%O' has not been initialized" typeof<'T> |> invalidOp
            | p -> p

        let pickler = 
            { new JsonPickler<'T> with
                member __.Pickle writer t = read().Pickle writer t
                member __.UnPickle reader = read().UnPickle reader }

        pRef, pickler

    let Y (f : JsonPickler<'T> -> JsonPickler<'T>) : JsonPickler<'T> =
        let pRef, p = createUninitialized<'T>()
        pRef := f p
        p