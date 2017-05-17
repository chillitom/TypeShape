module internal Vardusia.PicklerGeneration

open System
open System.Collections.Generic

open TypeShape
open TypeShape_Utils

open Vardusia.PrimitivePicklers
open Vardusia.GenericPicklers

let generatePickler<'T> (resolver : IPicklerResolver) : JsonPickler<'T> =
    let EQ (jp : JsonPickler<'a>) = unbox<JsonPickler<'T>> jp
    match shapeof<'T> with
    | Shape.Unit -> UnitPickler<unit>(()) |> EQ
    | Shape.Byte -> BytePickler() |> EQ
    | Shape.SByte -> SBytePickler() |> EQ
    | Shape.Int16 -> Int16Pickler() |> EQ
    | Shape.Int32 -> Int32Pickler() |> EQ
    | Shape.Int64 -> Int64Pickler() |> EQ
    | Shape.UInt16 -> UInt16Pickler() |> EQ
    | Shape.UInt32 -> UInt32Pickler() |> EQ
    | Shape.UInt64 -> UInt64Pickler() |> EQ
    | Shape.TimeSpan -> TimeSpanPickler() |> EQ
    | Shape.DateTime -> DateTimePickler() |> EQ
    | Shape.DateTimeOffset -> DateTimeOffsetPickler() |> EQ
    | Shape.BigInt -> BigIntegerPickler() |> EQ
    | Shape.String -> StringPickler() |> EQ
    | Shape.ByteArray -> ByteArrayPickler() |> EQ
    | Shape.Enum s ->
        s.Accept { new IEnumVisitor<JsonPickler<'T>> with
            member __.Visit<'e, 'u when 'e : enum<'u>
                                    and 'e : struct
                                    and 'e :> ValueType
                                    and 'e : (new : unit -> 'e)> () =

                        EnumStringPickler<'e>() |> EQ }

    | Shape.Nullable s ->
        s.Accept { new INullableVisitor<JsonPickler<'T>> with
            member __.Visit<'t when 't : struct
                                and 't :> ValueType
                                and 't : (new : unit -> 't)> () =

                let tpickler = resolver.Resolve<'t>()
                NullablePickler<'t>(tpickler) |> EQ }

    | Shape.FSharpOption s ->
        s.Accept { new IFSharpOptionVisitor<JsonPickler<'T>> with
            member __.Visit<'t>() =
                let tpickler = resolver.Resolve<'t>()
                FSharpOptionPickler<'t>(tpickler) |> EQ }

    | Shape.Array s when s.Rank = 1 ->
        s.Accept { new IArrayVisitor<JsonPickler<'T>> with
            member __.Visit<'t> _ =
                let tpickler = resolver.Resolve<'t>()
                mkCollectionPickler<'t[], 't> tpickler (fun t -> t.ToArray()) |> EQ }

    | Shape.FSharpList s ->
        s.Accept { new IFSharpListVisitor<JsonPickler<'T>> with
            member __.Visit<'t> () =
                let tpickler = resolver.Resolve<'t>()
                mkCollectionPickler<'t list, 't> tpickler Seq.toList |> EQ }

    | Shape.ResizeArray s ->
        s.Accept { new IResizeArrayVisitor<JsonPickler<'T>> with
            member __.Visit<'t> () =
                let tpickler = resolver.Resolve<'t>()
                mkCollectionPickler<ResizeArray<'t>, 't> tpickler id |> EQ }

    | Shape.FSharpSet s ->
        s.Accept { new IFSharpSetVisitor<JsonPickler<'T>> with
            member __.Visit<'t when 't : comparison> () =
                let tpickler = resolver.Resolve<'t>()
                mkCollectionPickler<Set<'t>, 't> tpickler Set.ofSeq |> EQ }

    | Shape.Dictionary s when s.Key = shapeof<string> ->
        s.Accept { new IDictionaryVisitor<JsonPickler<'T>> with
            member __.Visit<'k,'v when 'k : equality> () =
                let vpickler = resolver.Resolve<'v>()
                let mkDict (items : seq<KeyValuePair<_,_>>) =
                    let d = Dictionary<string, 'v>()
                    for kv in items do d.[kv.Key] <- kv.Value
                    d

                mkDictionaryPickler<Dictionary<string,'v>, 'v> vpickler mkDict |> EQ }

    | Shape.FSharpMap s when s.Key = shapeof<string> ->
        s.Accept { new IFSharpMapVisitor<JsonPickler<'T>> with
            member __.Visit<'k,'v when 'k : comparison> () =
                let vpickler = resolver.Resolve<'v>()
                let mkMap (items : seq<KeyValuePair<_,_>>) =
                    items |> Seq.map (fun kv -> kv.Key,kv.Value) |> Map.ofSeq

                mkDictionaryPickler<Map<string,'v>, 'v> vpickler mkMap |> EQ }

    | Shape.FSharpRecord (:? ShapeFSharpRecord<'T> as shape) ->
        mkRecordShapePickler<'T> resolver shape.CreateUninitialized shape.Fields |> EQ

    | Shape.CliMutable (:? ShapeCliMutable<'T> as shape) ->
        mkRecordShapePickler<'T> resolver shape.CreateUninitialized shape.Properties |> EQ

    | _ -> raise <| UnsupportedShape(typeof<'T>)


let resolve<'T> (cache : TypeCache) : JsonPickler<'T> =
    let mutable p = Unchecked.defaultof<JsonPickler<'T>>
    if cache.TryGetValue(&p) then p
    else
        let ctx = cache.CreateRecTypeManager()
        let rec self =
            { new IPicklerResolver with 
                member __.Resolve<'t>() =
                    match ctx.TryFind<JsonPickler<'t>>() with
                    | Some p -> p
                    | None -> 
                        ctx.CreateUninitialized(fun cell -> 
                            { new JsonPickler<'t> with 
                                member __.Pickle writer t = cell.Value.Pickle writer t
                                member __.UnPickle reader = cell.Value.UnPickle reader })
                        |> ignore

                        let p = generatePickler<'t> self
                        ctx.Complete p }

        self.Resolve<'T> ()
    


//module Pickler =
//    open System.Globalization

//    let private cache = new TypeCache()

//    let resolve<'T> () : JsonPickler<'T> =


//    let pickle (pickler : JsonPickler<'T>) (value : 'T) : string =
//        let writer = new JsonWriter(2, CultureInfo.InvariantCulture)
//        pickler.Pickle writer value
//        writer.ToJson()

//    let unpickle (pickler : JsonPickler<'T>) (json : string) : 'T =
//        let reader = new JsonReader(json, CultureInfo.InvariantCulture)
//        pickler.UnPickle reader