module internal Vardusia.PicklerGeneration

open System
open System.Collections.Generic
open System.Reflection

open TypeShape
open TypeShape_Utils

open Vardusia.PrimitivePicklers
open Vardusia.GenericPicklers

let generatePickler<'T> (resolver : IPicklerResolver) (picklerFactories : TypeCache) : JsonPickler<'T> =
    let EQ (jp : JsonPickler<'a>) = unbox<JsonPickler<'T>> jp

    let mutable factory = Unchecked.defaultof<IPicklerResolver -> JsonPickler<'T>>
    if picklerFactories.TryGetValue(&factory) then factory resolver else

    let attr = typeof<'T>.GetCustomAttributes<JsonTypeAttribute>(false) |> Seq.tryPick Some

    let requiredFields = attr |> Option.exists (fun attr -> attr.RequiredFields)

    match attr with
    | Some attr when attr.Pickler <> null -> extractPicklerFromType<'T> resolver attr.Pickler
    | _ ->

    match tryExtractPicklerFromToJsonMethods<'T> resolver with
    | Some pickler -> pickler
    | None ->

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
    | Shape.Single -> SinglePickler() |> EQ
    | Shape.Double -> DoublePickler() |> EQ
    | Shape.Decimal -> DecimalPickler() |> EQ
    | Shape.BigInt -> BigIntegerPickler() |> EQ
    | Shape.TimeSpan -> TimeSpanPickler() |> EQ
    | Shape.DateTime -> DateTimePickler() |> EQ
    | Shape.DateTimeOffset -> DateTimeOffsetPickler() |> EQ
    | Shape.String -> StringPickler() |> EQ
    | Shape.ByteArray -> ByteArrayPickler() |> EQ
    | Shape.Enum s ->
        s.Accept { new IEnumVisitor<JsonPickler<'T>> with
            member __.Visit<'e, 'u when 'e : enum<'u>
                                    and 'e : struct
                                    and 'e :> ValueType
                                    and 'e : (new : unit -> 'e)> () =

                        EnumStringPickler<'e, 'u>() |> EQ }

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
                mkArrayPickler tpickler  |> EQ }

    | Shape.FSharpList s ->
        s.Accept { new IFSharpListVisitor<JsonPickler<'T>> with
            member __.Visit<'t> () =
                let tpickler = resolver.Resolve<'t>()
                mkListPickler tpickler |> EQ }

    | Shape.ResizeArray s ->
        s.Accept { new IResizeArrayVisitor<JsonPickler<'T>> with
            member __.Visit<'t> () =
                let tpickler = resolver.Resolve<'t>()
                mkResizeArrayPickler tpickler |> EQ }

    | Shape.FSharpSet s ->
        s.Accept { new IFSharpSetVisitor<JsonPickler<'T>> with
            member __.Visit<'t when 't : comparison> () =
                let tpickler = resolver.Resolve<'t>()
                mkSetPickler tpickler |> EQ }

    | Shape.Dictionary s when s.Key = shapeof<string> ->
        s.Accept { new IDictionaryVisitor<JsonPickler<'T>> with
            member __.Visit<'k,'v when 'k : equality> () =
                let vpickler = resolver.Resolve<'v>()
                mkDictPickler vpickler |> EQ }

    | Shape.FSharpMap s when s.Key = shapeof<string> ->
        s.Accept { new IFSharpMapVisitor<JsonPickler<'T>> with
            member __.Visit<'k,'v when 'k : comparison> () =
                let vpickler = resolver.Resolve<'v>()
                mkMapPickler vpickler |> EQ }

    | Shape.FSharpRecord (:? ShapeFSharpRecord<'T> as shape) ->
        RecordPickler<'T>(resolver, requiredFields, shape.CreateUninitialized, shape.Fields) |> EQ

    | Shape.FSharpUnion (:? ShapeFSharpUnion<'T> as shape) ->
        FSharpUnionPickler<'T>(resolver, requiredFields, shape) |> EQ

    | Shape.CliMutable (:? ShapeCliMutable<'T> as shape) ->
        RecordPickler<'T>(resolver, requiredFields, shape.CreateUninitialized, shape.Properties) |> EQ

    | _ -> raise <| NonSerializableTypeException<'T>()


let resolve<'T> (cache : TypeCache) (picklerFactories : TypeCache) : JsonPickler<'T> =
    let mutable p = Unchecked.defaultof<JsonPickler<'T>>
    if cache.TryGetValue(&p) then p
    else
        use ctx = cache.CreateRecTypeManager()
        let rec resolver =
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

                        let p = generatePickler<'t> resolver picklerFactories
                        ctx.Complete p }

        resolver.Resolve<'T> ()