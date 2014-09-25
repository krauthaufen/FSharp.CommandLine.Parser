namespace FSharp.CLI

[<AutoOpen>]
module CLI =
    
    let entryPoint<'a> (f : 'a -> int) =
        let usage,parser = usageAndParser<'a>

        fun (args : string[]) ->
            match parser args with
                | Some args ->
                    f args
                | None ->
                    printfn "usage: %s" usage
                    0
