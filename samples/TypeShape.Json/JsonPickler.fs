namespace TypeShape.Json

//type JsonPickler<'T> =
//    abstract Pickle : JsonWriter -> 'T -> unit
//    abstract UnPickle : JsonReader -> 'T


//type StringPickler() =
//    interface JsonPickler<string> with
//        member __.Pickle writer str = 
//            match str with
//            | null -> writer.Null()
//            | _ -> writer.String str

//        member __.UnPickle reader =
//            let tok = reader.NextToken()
//            match tok.Tag with
//            | JsonTag.Null -> null
//            | JsonTag.String -> tok.Value
//            | JsonTag.Number -> tok.Value
//            | JsonTag.Bool -> if tok.Bool then "true" else "false"
//            | _ -> failwithf "Unexpected JSON token %O" tok.Tag

//type Int32Pickler() =
//    interface JsonPickler<int> with
//        member __.Pickle writer int =
            