module Dec01.FuelConsumptionTest exposing (suite)

import Dec01.FuelConsumption exposing (improvedWeightToFuelWeight)
import Expect
import Test exposing (Test, describe, test)


improvedWeightToFuelWeightData : List ( Int, Int )
improvedWeightToFuelWeightData =
    [ ( 14, 2 )
    , ( 1969, 966 )
    , ( 100756, 50346 )
    ]


suite : Test
suite =
    describe "FuelConsumption"
        [ describe "improvedWeightToFuelWeight" <|
            List.map
                (\( weight, fuelWeight ) ->
                    test (String.fromInt weight ++ " -> " ++ String.fromInt fuelWeight) <|
                        \_ -> Expect.equal (improvedWeightToFuelWeight weight) fuelWeight
                )
                improvedWeightToFuelWeightData
        ]
