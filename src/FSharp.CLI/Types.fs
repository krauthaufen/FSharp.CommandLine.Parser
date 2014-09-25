#if INTERACTIVE
#else
namespace FSharp.CLI
#endif

open System
open Microsoft.FSharp.Reflection
open System.Reflection

[<AutoOpen>]
module Attributes =

    type DescriptionAttribute(desc : string) =
        inherit System.Attribute()

        member x.Description = desc

    type CountAttribute(minCount : int, maxCount : int) =
        inherit System.Attribute()
        member x.MinCount = minCount
        member x.MaxCount = maxCount

        new(exaclty) = CountAttribute(exaclty, exaclty)

    type AtLeastAttribute(min : int) =
        inherit CountAttribute(min, Int32.MaxValue)

    type SwitchesAttribute([<ParamArray>] switches : string[]) =
        inherit Attribute()

        member x.Switches = switches |> Set.ofArray

    type NoSwitchAttribute() =
        inherit SwitchesAttribute("")

    type DefaultAttribute(value : obj) =
        inherit Attribute()

        member x.DefaultValue = value

[<AutoOpen>]
module ValueParser =
    open System.Collections.Generic
    type ParserResult = Error of string | Success | Done

    type ParserMap = private { parsers : Dictionary<Type, ref<Option<obj>> -> string -> ParserResult> }

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

    let parseMap =
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



[<AutoOpen>]
module Parser =

    let inline tryRead< ^a when ^a : (static member TryParse : string * byref<'a> -> bool)> : string -> Option< ^a> =
        fun str ->
            let mutable result = Unchecked.defaultof< ^a>
            let success = (^a : (static member TryParse : string * byref<'a> -> bool) (str, &result))
            if success then
                Some result
            else
                None

    let private getAttribute<'a> (pi : PropertyInfo) =
        let att = pi.GetCustomAttribute(typeof<'a>, true)
        if att = null then
            None
        else
            Some (att |> unbox<'a>)

    let private getAttributeValue<'a,'b> (f : 'a -> 'b) (pi : PropertyInfo)  : Option<'b> =
        match getAttribute<'a> pi with
            | Some att -> Some (f att)
            | None -> None

    let private description (pi : PropertyInfo) =
        pi |> getAttributeValue<DescriptionAttribute,_> (fun d -> d.Description)

    let private counts (pi : PropertyInfo) =
        match pi |> getAttributeValue<CountAttribute,_> (fun d -> d.MinCount, d.MaxCount) with
            | Some tup -> tup
            | None ->
                let t = pi.PropertyType
                let i = t.GetInterface("IEnumerable`1")
                
                if i <> null then
                    (0,Int32.MaxValue)
                elif t = typeof<bool> then
                    (0,0)
                elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Option<_>> then
                    (0,1)
                else
                    (1,1)
                    

    let private defaultValue (pi : PropertyInfo) =
        let user = pi |> getAttributeValue<DefaultAttribute,_> (fun d -> d.DefaultValue)
        
        match user with
            | Some u -> Some u
            | None ->
                let genType =
                    if pi.PropertyType.IsGenericType then pi.PropertyType.GetGenericTypeDefinition()
                    else pi.PropertyType

                if genType = typeof<bool> then Some (false :> obj)
                elif genType = typedefof<Option<_>> then Some (pi.PropertyType.GetProperty("None").GetValue(null))
                elif genType = typedefof<list<_>> then Some (pi.PropertyType.GetProperty("Empty").GetValue(null))
                else None

    let private switches (pi : PropertyInfo) =
        pi |> getAttributeValue<SwitchesAttribute,_> (fun d -> d.Switches)
        

    let withShortNames (userDefined : Map<string, Option<list<string>>>) (properties : seq<PropertyInfo>) =
        let userMap = 
            userDefined |> Map.toList 
                        |> List.choose (fun (k,v) -> match v with | Some v -> Some (k,v) | None -> None)
                        |> Map.ofList

        let userTaken =
            userMap |> Map.toList |> List.collect (fun (_,n) -> n) |> Set.ofList

        
        let autoProps = properties |> Seq.toList |> List.filter (fun pi -> not <| Map.containsKey pi.Name userMap)

        let mutable map = userMap
        let mutable taken = userTaken

        for pi in autoProps do
            let short = pi.Name.Substring(0,1)

            if not <| Set.contains short taken then
                let newNames = 
                    match Map.tryFind pi.Name map with
                        | Some l -> short::pi.Name::l
                        | None -> [short; pi.Name]
                map <- Map.add pi.Name newNames map

                taken <- Set.add short taken
            else
                let newNames = 
                    match Map.tryFind pi.Name map with
                        | Some l -> pi.Name::l
                        | None -> [pi.Name]
                map <- Map.add pi.Name newNames map

                printfn "short-name collision with: %A (please annotate)" pi.Name

        let map = map

        properties |> Seq.map (fun pi ->
            match Map.tryFind pi.Name map with
                | Some l -> (pi,l)
                | None -> (pi, [])
        ) |> Seq.toList
  
    let patternName (str : string) =
        str.Replace(".", @"\.")
                    
    let rx (str : string) =
        System.Text.RegularExpressions.Regex(str)

    type ListHelper<'a>() =
        static member Reverse(l : list<'a>) =
            l |> List.rev

    let usageAndParser<'a> : (unit -> string) * (string[] -> Option<'a>) =
        
        let fields = FSharpType.GetRecordFields(typeof<'a>)
        
        let definedNames = 
            fields |> Array.map (fun pi ->
                        match switches pi with
                            | Some sw -> (pi.Name, sw |> Set.toList |> Some)
                            | None -> (pi.Name, None)
                      ) 
                   |> Map.ofArray

        let allNames = withShortNames definedNames fields

        let switchPattern =
            allNames |> List.collect (fun (_,names) -> names) 
                     |> List.filter (fun n -> n <> "")
                     |> List.map (fun n -> 
                            let n = patternName n
                            if n.Length = 1 then
                                sprintf "-(?<name>%s)" n
                            else
                                sprintf "--(?<name>%s)" n
                        )
                     |> String.concat "|"
                     |> rx

        let noSwitchProp =
            allNames |> List.tryPick (fun (p,names) -> if names |> List.exists (fun n -> n = "") then Some p else None)

        let inverseMap = 
            allNames |> List.collect (fun (p,names) -> names |> List.map (fun n -> (n,p))) |> Map.ofList

        let getProp (m : System.Text.RegularExpressions.Match) =
            if m.Groups.["name"].Success then
                let name = m.Groups.["name"].Value
                match Map.tryFind name inverseMap with
                    | Some prop -> Some prop
                    | None -> 
                        printfn "invalid name %A" name
                        None
            else
                printfn "could not get property for flag: %A" m.Value
                None

        
        let append (pi : PropertyInfo) (r : ref<Option<obj>>) =
            let (min,max) = counts pi
            let count =
                match !r with
                    | Some _ -> ref 1
                    | None -> ref 0

            let parser = getParser pi.PropertyType

            parser r

        let parse = 
            fun (str : string[]) ->
                let values = 
                    fields |> Array.map (fun pi -> 
                                match defaultValue pi with
                                    | Some def -> (pi.Name, (ref (Some def)))
                                    | None -> (pi.Name, (ref None))
                              )
                           |> Map.ofArray

                let getCurrent (p : Option<PropertyInfo>) =
                    match p with
                        | Some p ->
                            let ref = Map.find p.Name values
                            ref
                        | None ->
                            // sink
                            ref <| None

                let writer old (pi : Option<PropertyInfo>) =
                    match pi with
                        | Some pi ->
                            let v = Map.find pi.Name values
                            if pi.PropertyType = typeof<bool> then
                                v := Some (true :> obj)
                                match old with
                                    | Some old -> old
                                    | None -> fun v -> Error (sprintf "don't know what to do with %A" v)
                            else
                                append pi v
                        | None ->
                            // sink
                            fun v -> Error (sprintf "don't know what to do with %A" v)

            
                let mutable currentProp = noSwitchProp
                let noneWriter = writer None currentProp
                let mutable currentWriter = noneWriter

                for i in 0..str.Length-1 do
                    let s = str.[i]
                    let m = switchPattern.Match s
                    if m.Success then
                        currentProp <- getProp m
                        currentWriter <- writer (Some noneWriter) currentProp
                    else
                        match currentWriter s with
                            | Success -> ()
                            | Done -> 
                                currentProp <- noSwitchProp
                                currentWriter <- writer (Some noneWriter) currentProp
                            | Error e ->
                                printfn "WARNING: %s" e
                        ()
                    ()

                let values = fields |> Array.map (fun pi -> !(Map.find pi.Name values))



                if values |> Array.forall (fun o -> o.IsSome) then
                    let values = values |> Array.map (fun o -> o.Value)
                                        |> Array.map (fun v ->
                                            let vType = v.GetType()

                                            if vType.IsGenericType && vType.GetGenericTypeDefinition() = typedefof<list<_>> then
                                                //reverse the list
                                                let t = vType.GetGenericArguments().[0]
                                                let helper = typedefof<ListHelper<_>>.MakeGenericType t
                                                let revM = helper.GetMethod "Reverse"

                                                revM.Invoke(null, [|v|])
                                            else
                                                v
                                        )
                

                    let res = FSharpValue.MakeRecord(typeof<'a>, values)
                    Some (res |> unbox<'a>)
                else
                    None

        

        let usage() =
            let self = System.Diagnostics.Process.GetCurrentProcess().ProcessName |> System.IO.Path.GetFileNameWithoutExtension

            let usage = System.Text.StringBuilder()
            let descriptions = System.Text.StringBuilder()
            let appendf fmt = Printf.kprintf (fun str -> usage.Append str |> ignore) fmt
            let descf fmt = Printf.kprintf (fun str -> descriptions.Append str |> ignore) fmt

            appendf "%s " self

            let optionName (str : string) =
                if str.Length <= 1 then
                    sprintf "-%s" str
                else
                    sprintf "--%s" str


            let isOptional (pi : PropertyInfo) =
                let pType =
                    if pi.PropertyType.IsGenericType then pi.PropertyType.GetGenericTypeDefinition()
                    else pi.PropertyType

                if pType = typedefof<Option<_>> then true
                elif pType = typedefof<list<_>> then false
                else
                    match defaultValue pi with
                        | Some _ -> true
                        | None -> false

            let isList (pi : PropertyInfo) =
                let pType =
                    if pi.PropertyType.IsGenericType then pi.PropertyType.GetGenericTypeDefinition()
                    else pi.PropertyType

                if pType = typedefof<list<_>> then true
                else false


            for (pi, names) in allNames do
                let desc = description pi
                
                let shortName = names |> Seq.minBy(fun str -> str.Length) |> optionName
                let shortName = 
                    if shortName = "-" then ""
                    else shortName + " "

                let argName = pi.Name

                let argString = 
                    if pi.PropertyType = typeof<bool> then
                        sprintf "[%s]" (shortName.TrimEnd(' '))
                    else
                        if isList pi then
                            if shortName <> "" then
                                sprintf "[%s{%s}]" shortName argName
                            else
                                sprintf "{%s}" argName

                        elif isOptional pi then
                            sprintf "[%s%s]" shortName argName

                        else
                            sprintf "%s%s" shortName argName
                
 
                appendf "%s " argString


                if shortName = "" then
                    descf "%s:\r\n     %A\r\n\r\n" argName (match desc with | Some desc -> desc | None -> "no description")
                else
                    descf "%s:\r\n     %s\r\n    %A\r\n\r\n" argName (names |> List.map optionName |> String.concat " | ") (match desc with | Some desc -> desc | None -> "no description")

                ()


            sprintf "%s\r\n%s" (usage.ToString()) (descriptions.ToString())

        (usage, parse)



    let usage<'a> =
        let (u,_) = usageAndParser<'a>
        u

    let parser<'a> =
        let (_,p) = usageAndParser<'a>
        p


module Tests =
    type Parameters = {

        [<NoSwitch>]
        [<Description("the input files for the tool")>]
        inputs : list<string>

        [<Description("sets the output-file for the tool")>]
        output : Option<string>

        [<Default(7)>]
        [<Description("sets the iterations for the tool")>]
        [<Switches("n", "iter", "iterations")>]
        iterations : int

        [<Description("includes the given files")>]
        includes : list<string>

        [<Description("enables the batch mode")>]
        batch : bool
    }


    let p = parser<Parameters>
    let usage = usage<Parameters>
    let run() =
        let args = [|"test.txt"; "--output"; "out.txt"; "hugo.txt"; "-b"; "sepp.txt"; "-i"; "10" |]

        usage() |> printfn "%s"

        let res = p args
        match res with
            | Some res ->
                printfn "%A" res
            | None ->
                usage() |> printfn "usage: %s"