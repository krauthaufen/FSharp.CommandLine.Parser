﻿// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open FSharp.CLI

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

    [<Description("some required argument")>]
    required : string
}


let test0() =
    let args = [|"test.txt"; "--output"; "out.txt"; "-o"; "otherOutput.txt"; "hugo.txt"; "-b"; "sepp.txt"; "-i"; "10"; "-r"; "a"|]
    args |> entryPoint<Parameters> (fun p ->
        printfn "%A" p
        0
    )

[<EntryPoint>]
let main args = 
    test0()
