namespace FSharp.CLI

open System
open Microsoft.FSharp.Reflection
open System.Reflection


module private ValueParserHelpers =
    open System.Collections.Generic
    
    type ParserMap = internal { parsers : Dictionary<Type, ref<Option<obj>> -> string -> ParserResult> }

    type ParserMapBuilder() =
        member x.Yield(()) = { parsers = Dictionary() }

        [<CustomOperation("parser")>]
        member x.Parser(p : ParserMap, f : Option<'a> -> string -> ParserResult * 'a) =

            let objParser (r : ref<Option<obj>>) (str : string) =
                let (res,newValue) =
                    match !r with
                        | Some old ->
                            match old with
                                | :? 'a as old ->
                                    let (res, v) = f (Some old) str
                                    match res with
                                        | Error e -> Error e, old
                                        | _ -> res, v
                                | _ -> 
                                    printfn "invalid type for %A" typeof<'a> 
                                    f None str
                           
                        | None ->
                            f None str
                match res with
                    | Success|Done -> 
                        r := Some (newValue :> obj)
                    | _ -> 
                        ()
                res

            p.parsers.Add(typeof<'a>, objParser)

            p

        member x.Run(map : ParserMap) =
            let parsers = map.parsers |> Seq.map (fun (KeyValue(k,v)) -> k,v) |> Seq.toList


            // create list-parsers
            for (t, p) in parsers do
                let listType = typedefof<list<_>>.MakeGenericType t

                let consM = listType.GetMethod "Cons"
                let nilM = (listType.GetProperty "Empty").GetMethod

                let cons (head : obj) (tail : obj) =
                    consM.Invoke(null, [|head; tail|])

                let nil () =
                    nilM.Invoke(null, [||])

                let listParser (r : ref<Option<obj>>) (str : string) =

                    let output = ref None
                    let res = p output str
                    match res with
                        | Success | Done ->
                            match !output with
                                | Some o ->
                                    let oldValue =
                                        match !r with
                                            | Some l -> l
                                            | None -> nil()

                                    r := Some (cons o oldValue)

                                    res
                                | None ->
                                    res
                            
                        | Error e ->
                            Error e

                map.parsers.Add(listType, listParser)

            // create option-parsers
            for (t, p) in parsers do
                let optionType = typedefof<Option<_>>.MakeGenericType t
                let someM = optionType.GetMethod "Some"
                let noneM = (optionType.GetProperty "None").GetMethod

                let some o = someM.Invoke(null, [|o|])
                let none() = noneM.Invoke(null, [||])

                let optionParser (r : ref<Option<obj>>) (str : string) =
                    let output = ref None
                    let res = p output str

                    match res with
                        | Success | Done -> 
                            match !output with
                                | Some o -> 
                                    r := Some (some o)
                                    Done
                                | None ->
                                    r := None
                                    Done
                        | Error e ->
                            Error e

                map.parsers.Add(optionType, optionParser)



            map

    let parser = ParserMapBuilder()

    let inline tryRead< ^a when ^a : (static member TryParse : string * byref<'a> -> bool)>  =
        fun str ->
            let mutable result = Unchecked.defaultof< ^a>
            let success = (^a : (static member TryParse : string * byref<'a> -> bool) (str, &result))
            if success then
                (Success, result)
            else
                (Error "could not parse", result)

[<AutoOpen>]
module ValueParser =
    open ValueParserHelpers

    let private parseMap =
        parser {
            parser (fun _ str -> Success, str)
            parser (fun _ -> tryRead<int>)
            parser (fun _ -> tryRead<float>)
        }


    let getParser (t : Type) =
        match parseMap.parsers.TryGetValue t with
            | (true, p) ->
                p
            | _ ->
                fun _ _ -> Error (sprintf "no parser for type: %A" t)

