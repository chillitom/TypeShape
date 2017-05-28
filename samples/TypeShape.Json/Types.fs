namespace Vardusia

open System
open System.Runtime.Serialization

type VardusiaException =
    inherit Exception
    new (message : string, ?inner : exn) = { inherit Exception(message, defaultArg inner null) }
    new (si : SerializationInfo, sc : StreamingContext) = { inherit Exception(si,sc) }

[<AbstractClass>]
type NonSerializableTypeException =
    inherit VardusiaException
    abstract Type : Type

    new (message : string, ?inner : exn) = { inherit VardusiaException(message, ?inner = inner) }
    new (si : SerializationInfo, sc : StreamingContext) = { inherit VardusiaException(si,sc) }

type NonSerializableTypeException<'T> =
    inherit NonSerializableTypeException
    override __.Type = typeof<'T>

    new (?message : string, ?inner : exn) =
        let message =
            match message with
            | None -> sprintf "Could not resolve a JSON pickler for type '%O'" typeof<'T>
            | Some m -> m

        { inherit NonSerializableTypeException(message, ?inner = inner) }
        
    new (si : SerializationInfo, sc : StreamingContext) = 
        { inherit NonSerializableTypeException(si,sc) }