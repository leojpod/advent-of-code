module Dec02.IntCodeTest exposing (suite)

import Array
import Dec02.IntCode as IntCode exposing (Instruction(..), IntCode, State(..), readInstruction, runNextInstruction, runner)
import Expect
import Test exposing (Test, describe, test)


type alias TestData =
    { name : String
    , start : IntCode
    , expectedInstruction : Instruction
    , expectedNextState : State
    , expectedFinalState : State
    }


runnerTestData : List TestData
runnerTestData =
    [ { name = "example 1a"
      , start = IntCode 0 <| Array.fromList [ 1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50 ]
      , expectedInstruction = IntCode.Add 9 10 3
      , expectedNextState = IntCode.Ok <| IntCode 4 <| Array.fromList [ 1, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50 ]
      , expectedFinalState = Finished 3500
      }
    , { name = "example 1b"
      , start =
            IntCode 4 <|
                Array.fromList [ 1, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50 ]
      , expectedInstruction = IntCode.Multiply 3 11 0
      , expectedNextState = IntCode.Ok <| IntCode 8 <| Array.fromList [ 3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50 ]
      , expectedFinalState = Finished 3500
      }
    , { name = "example 1c"
      , start = IntCode 8 <| Array.fromList [ 3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50 ]
      , expectedInstruction = IntCode.Exit
      , expectedNextState = Finished 3500
      , expectedFinalState = Finished 3500
      }
    , { name = "simple 1"
      , start = IntCode 0 <| Array.fromList [ 1, 0, 0, 0, 99 ]
      , expectedInstruction = IntCode.Add 0 0 0
      , expectedNextState = IntCode.Ok <| IntCode 4 <| Array.fromList [ 2, 0, 0, 0, 99 ]
      , expectedFinalState = Finished 2
      }
    , { name = "simple 2"
      , start = IntCode 0 <| Array.fromList [ 2, 3, 0, 3, 99 ]
      , expectedInstruction = IntCode.Multiply 3 0 3
      , expectedNextState = IntCode.Ok <| IntCode 4 <| Array.fromList [ 2, 3, 0, 6, 99 ]
      , expectedFinalState = Finished 2
      }
    , { name = "simple 3"
      , start = IntCode 0 <| Array.fromList [ 2, 4, 4, 5, 99, 0 ]
      , expectedInstruction = IntCode.Multiply 4 4 5
      , expectedNextState = IntCode.Ok <| IntCode 4 <| Array.fromList [ 2, 4, 4, 5, 99, 9801 ]
      , expectedFinalState = Finished 2
      }
    , { name = "simple 4"
      , start = IntCode 0 <| Array.fromList [ 1, 1, 1, 4, 99, 5, 6, 0, 99 ]
      , expectedInstruction = IntCode.Add 1 1 4
      , expectedNextState = IntCode.Ok <| IntCode 4 <| Array.fromList [ 1, 1, 1, 4, 2, 5, 6, 0, 99 ]
      , expectedFinalState = Finished 30
      }
    ]


suite : Test
suite =
    describe "IntCode"
        [ describe "readInstruction"
            (runnerTestData
                |> List.map
                    (\{ name, start, expectedInstruction } ->
                        test name <|
                            \_ ->
                                readInstruction start
                                    |> Result.map Tuple.first
                                    |> Expect.equal (Result.Ok expectedInstruction)
                    )
            )
        , describe "runNextInstruction"
            (runnerTestData
                |> List.map
                    (\{ name, start, expectedNextState } ->
                        test name <|
                            \_ ->
                                runNextInstruction start
                                    |> Expect.equal expectedNextState
                    )
            )
        , describe "runner"
            (runnerTestData
                |> List.map
                    (\{ name, start, expectedFinalState } ->
                        test name <|
                            \_ -> runner (IntCode.Ok start) |> Expect.equal expectedFinalState
                    )
            )
        ]
