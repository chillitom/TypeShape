namespace Vardusia

open System
open System.Runtime.Serialization

type VardusiaException =
    inherit Exception
    new (message : string, ?inner : exn) = { inherit Exception(message, defaultArg inner null) }
    new (si : SerializationInfo, sc : StreamingContext) = { inherit Exception(si,sc) }

type NonSerializableTypeException =
    inherit VardusiaException
    val private _type : Type
    member __.Type = __._type
    new (ty : Type, ?message : string, ?inner : exn) =
        let message =
            match message with
            | None -> sprintf "Could not resolve a JSON pickler for type '%O'" ty
            | Some m -> m

        { inherit VardusiaException(message, ?inner = inner) ; _type = ty }

    new (si : SerializationInfo, sc : StreamingContext) = 
        { inherit VardusiaException(si,sc) ; _type = si.GetValue("Type", typeof<Type>) :?> Type }

    interface ISerializable with
        member __.GetObjectData(si : SerializationInfo, sc : StreamingContext) =
            si.AddValue("Type", __._type)
            base.GetObjectData(si, sc)