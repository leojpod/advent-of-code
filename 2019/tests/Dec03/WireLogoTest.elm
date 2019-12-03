module Dec03.WireLogoTest exposing (suite)

import Dec03.WireLogo exposing (Move(..), Path, pathFromMoves)
import Expect
import Test exposing (Test, describe, test)


type alias TestData =
    { name : String
    , moves : List Move
    , path : Path
    }


testData : List TestData
testData =
    [ { name = "example 1"
      , moves = [ Right 8, Up 5, Left 5, Down 3 ]
      , path = [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ), ( 3, 0 ), ( 4, 0 ), ( 5, 0 ), ( 6, 0 ), ( 7, 0 ), ( 8, 0 ), ( 8, 1 ), ( 8, 2 ), ( 8, 3 ), ( 8, 4 ), ( 8, 5 ), ( 7, 5 ), ( 6, 5 ), ( 5, 5 ), ( 4, 5 ), ( 3, 5 ), ( 3, 4 ), ( 3, 3 ), ( 3, 2 ) ]
      }
    , { name = "example 2"
      , moves = [ Up 7, Right 6, Down 4, Left 4 ]
      , path = [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 0, 3 ), ( 0, 4 ), ( 0, 5 ), ( 0, 6 ), ( 0, 7 ), ( 1, 7 ), ( 2, 7 ), ( 3, 7 ), ( 4, 7 ), ( 5, 7 ), ( 6, 7 ), ( 6, 6 ), ( 6, 5 ), ( 6, 4 ), ( 6, 3 ), ( 5, 3 ), ( 4, 3 ), ( 3, 3 ), ( 2, 3 ) ]
      }
    ]


suite : Test
suite =
    Test.only <|
        describe "WireLogo"
            [ describe "pathFromMoves"
                (testData
                    |> List.map
                        (\{ name, moves, path } ->
                            test name <|
                                \_ -> pathFromMoves moves |> Expect.equal path
                        )
                )
            ]
