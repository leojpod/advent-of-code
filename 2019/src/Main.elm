module Main exposing (main)

import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Dec01.FuelConsumption
import Ports


type alias Flags =
    Program.FlagsIncludingArgv {}


type AdventOptions
    = FuelConsumption Dec01.FuelConsumption.Input


programConfig : Program.Config AdventOptions
programConfig =
    Program.config
        |> Program.add (OptionsParser.map FuelConsumption Dec01.FuelConsumption.options)


init : Flags -> AdventOptions -> Cmd Never
init _ options =
    case options of
        FuelConsumption input ->
            Dec01.FuelConsumption.calculator input


main : Program.StatelessProgram Never {}
main =
    Program.stateless
        { printAndExitFailure = Ports.printAndExitFailure
        , printAndExitSuccess = Ports.printAndExitSuccess
        , init = init
        , config = programConfig
        }
