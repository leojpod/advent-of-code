module Main exposing (main)

import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Dec01.FuelConsumption
import Dec02.IntCode
import Dec03.WireLogo
import Ports


type alias Flags =
    Program.FlagsIncludingArgv {}


type AdventOptions
    = FuelConsumption Dec01.FuelConsumption.Input
    | IntCode Dec02.IntCode.State
    | IntCodeBis Dec02.IntCode.IntCode
    | WireCrossing (List (List Dec03.WireLogo.Move))
    | WireCrossingBis (List (List Dec03.WireLogo.Move))


programConfig : Program.Config AdventOptions
programConfig =
    Program.config
        |> Program.add (OptionsParser.map FuelConsumption Dec01.FuelConsumption.options)
        |> Program.add (OptionsParser.map IntCode Dec02.IntCode.options)
        |> Program.add (OptionsParser.map IntCodeBis Dec02.IntCode.optionsPart2)
        |> Program.add (OptionsParser.map WireCrossing <| Dec03.WireLogo.options "wire-crossing")
        |> Program.add (OptionsParser.map WireCrossingBis <| Dec03.WireLogo.options "wire-crossing-2")


init : Flags -> AdventOptions -> Cmd Never
init _ options =
    case options of
        FuelConsumption input ->
            Dec01.FuelConsumption.calculator input

        IntCode state ->
            Dec02.IntCode.run state

        IntCodeBis intCode ->
            case Dec02.IntCode.findStartCode ( 0, 0 ) intCode of
                Ok value ->
                    String.fromInt value |> Ports.printAndExitSuccess

                Err msg ->
                    Ports.printAndExitFailure msg

        WireCrossing wireMoves ->
            Dec03.WireLogo.run wireMoves

        WireCrossingBis wireMoves ->
            Dec03.WireLogo.runPart2 wireMoves


main : Program.StatelessProgram Never {}
main =
    Program.stateless
        { printAndExitFailure = Ports.printAndExitFailure
        , printAndExitSuccess = Ports.printAndExitSuccess
        , init = init
        , config = programConfig
        }
