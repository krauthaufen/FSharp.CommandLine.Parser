namespace FSharp.CLI

open System
open Microsoft.FSharp.Reflection
open System.Reflection

[<AutoOpen>]
module Parser =

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
        

    let private withShortNames (userDefined : Map<string, Option<list<string>>>) (properties : seq<PropertyInfo>) =
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
  
    let private patternName (str : string) =
        str.Replace(".", @"\.")
                    
    let private rx (str : string) =
        System.Text.RegularExpressions.Regex(str)

    type ListHelper<'a>() =
        static member Reverse(l : list<'a>) =
            l |> List.rev

    let usageAndParser<'a> : string * (string[] -> Option<'a>) =
        
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

        
        let append (pi : PropertyInfo) =
            let parser = getParser pi.PropertyType
            parser

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

        

        let usage =
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
