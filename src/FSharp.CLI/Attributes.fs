namespace FSharp.CLI

open System

type DescriptionAttribute(desc : string) =
    inherit Attribute()

    member x.Description = desc

type SwitchesAttribute([<ParamArray>] switches : string[]) =
    inherit Attribute()

    member x.Switches = switches |> Set.ofArray

type NoSwitchAttribute() =
    inherit SwitchesAttribute("")

type DefaultAttribute(value : obj) =
    inherit Attribute()

    member x.DefaultValue = value


type ParserResult = Error of string | Success | Done
