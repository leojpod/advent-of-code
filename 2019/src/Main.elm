module Main exposing (main)

import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Dec01.FuelConsumption
import Dec02.IntCode
import Ports


type alias Flags =
    Program.FlagsIncludingArgv {}


type AdventOptions
    = FuelConsumption Dec01.FuelConsumption.Input
    | IntCode Dec02.IntCode.State


programConfig : Program.Config AdventOptions
programConfig =
    Program.config
        |> Program.add (OptionsParser.map FuelConsumption Dec01.FuelConsumption.options)
        |> Program.add (OptionsParser.map IntCode Dec02.IntCode.options)


init : Flags -> AdventOptions -> Cmd Never
init _ options =
    case options of
        FuelConsumption input ->
            Dec01.FuelConsumption.calculator input

        IntCode state ->
            Dec02.IntCode.run state


main : Program.StatelessProgram Never {}
main =
    Program.stateless
        { printAndExitFailure = Ports.printAndExitFailure
        , printAndExitSuccess = Ports.printAndExitSuccess
        , init = init
        , config = programConfig
        }
