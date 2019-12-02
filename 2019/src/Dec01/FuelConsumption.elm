module Dec01.FuelConsumption exposing
    ( Input
    , calculator
    , improvedWeightToFuelWeight
    , options
    )

import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.OptionsParser.BuilderState as BuilderState
import Cli.Validate as Validate
import Ports
import Result.Extra


type alias Input =
    { shouldCountFuelsOwnWeight : Bool
    , modules : List Int
    }


options : OptionsParser.OptionsParser Input BuilderState.NoMoreOptions
options =
    OptionsParser.buildSubCommand "fuel-consumption" Input
        |> OptionsParser.withDoc "run the fuel-consumption calculator"
        |> OptionsParser.with (Option.flag "fuel-weight")
        |> (OptionsParser.withRestArgs <|
                (Option.restArgs "the modules' weights for the rocket"
                    |> Option.validate (Validate.predicate "needs at least one module to be given" (List.isEmpty >> not))
                    |> Option.validateMap
                        (List.map (String.toInt >> Result.fromMaybe "This is not a valid weight")
                            >> Result.Extra.combine
                        )
                )
           )


weightToFuelWeight : Int -> Int
weightToFuelWeight weight =
    weight // 3 - 2


improvedWeightToFuelWeight : Int -> Int
improvedWeightToFuelWeight =
    weightToFuelWeight
        >> (\weight ->
                if weight <= 0 then
                    0

                else
                    weight + improvedWeightToFuelWeight weight
           )


moduleFuelRequirements : Bool -> Int -> Int
moduleFuelRequirements shouldCountFuelsOwnWeight =
    if Debug.log "shouldCountFuelsOwnWeight -> " shouldCountFuelsOwnWeight then
        improvedWeightToFuelWeight

    else
        weightToFuelWeight


calculator : Input -> Cmd Never
calculator { shouldCountFuelsOwnWeight, modules } =
    modules
        |> List.map (moduleFuelRequirements shouldCountFuelsOwnWeight)
        |> List.sum
        |> String.fromInt
        |> Ports.print
