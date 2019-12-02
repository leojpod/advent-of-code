module Dec01.FuelConsumption exposing
    ( Input
    , calculator
    , options
    )

import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.OptionsParser.BuilderState as BuilderState
import Cli.Validate as Validate
import Ports
import Result.Extra


type alias Input =
    List Int


options : OptionsParser.OptionsParser Input BuilderState.NoMoreOptions
options =
    OptionsParser.buildSubCommand "fuel-consumption" identity
        |> OptionsParser.withDoc "run the fuel-consumption calculator"
        |> (OptionsParser.withRestArgs <|
                (Option.restArgs "the modules' weights for the rocket"
                    |> Option.validate (Validate.predicate "needs at least one module to be given" (List.isEmpty >> not))
                    |> Option.validateMap
                        (List.map (String.toInt >> Result.fromMaybe "This is not a valid weight")
                            >> Result.Extra.combine
                        )
                )
           )


calculator : Input -> Cmd Never
calculator =
    List.map (\weight -> weight // 3 - 2)
        >> List.sum
        >> String.fromInt
        >> Ports.print
