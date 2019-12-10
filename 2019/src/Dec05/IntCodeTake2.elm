module Dec05.IntCodeTake2 exposing
    ( IntCode
    , State
    , options
    , run
    )

import Array exposing (Array)
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.OptionsParser.BuilderState as BuilderState
import Maybe.Extra
import Ports
import Result.Extra


type alias IntCode =
    { pointer : Int
    , memory : Array Int
    }


type State
    = Err String
    | Ok IntCode
    | Finished Int


options : OptionsParser.OptionsParser State BuilderState.AnyOptions
options =
    OptionsParser.buildSubCommand "diagnostic" Ok
        |> OptionsParser.withDoc "run the intCode computer"
        |> OptionsParser.with
            (Option.requiredPositionalArg "instructions"
                |> Option.map (String.split ",")
                |> Option.validateMap
                    (List.map
                        (String.toInt
                            >> Result.fromMaybe "This is not a valid instruction"
                        )
                        >> Result.Extra.combine
                    )
                |> Option.map (Array.fromList >> IntCode 0)
            )


andThen : State -> (IntCode -> State) -> State
andThen state fct =
    case state of
        Ok intCode ->
            fct intCode

        Err _ ->
            state

        Finished _ ->
            state


fromMaybe : String -> Maybe IntCode -> State
fromMaybe errMsg maybeIntCode =
    case maybeIntCode of
        Just intCode ->
            Ok intCode

        Nothing ->
            Err errMsg


fromResult : Result String IntCode -> State
fromResult result =
    case result of
        Result.Ok intCode ->
            Ok intCode

        Result.Err msg ->
            Err msg


type OpMode
    = Position
    | Immediate


type Instruction
    = Add ( OpMode, Int ) ( OpMode, Int ) Int
    | Multiply ( OpMode, Int ) ( OpMode, Int ) Int
    | Input Int
    | Output ( OpMode, Int )
    | Exit


fromIntToOpMode : Int -> Result String OpMode
fromIntToOpMode code =
    case code of
        0 ->
            Result.Ok Position

        1 ->
            Result.Ok Immediate

        _ ->
            Result.Err <| String.fromInt code ++ " is not a valid Mode"


readInstruction : IntCode -> Result String ( Instruction, IntCode )
readInstruction ({ pointer, memory } as intCode) =
    Array.get pointer memory
        |> Result.fromMaybe ("Error accessing incorrect memory place (" ++ String.fromInt pointer ++ ")")
        |> Result.andThen
            (\instructionCode ->
                let
                    _ =
                        Debug.log "instructionCode -> " ( pointer, instructionCode )

                    opCode =
                        modBy 100 instructionCode

                    modeBlock =
                        instructionCode // 100

                    firstOpMode =
                        modBy 10 modeBlock |> fromIntToOpMode

                    secondOpMode =
                        modBy 10 (modeBlock // 10) |> fromIntToOpMode

                    thirdOpMode =
                        modeBlock // 100 |> fromIntToOpMode
                in
                case opCode of
                    1 ->
                        Result.map3
                            (\firstMode secondMode thirdMode ->
                                case thirdMode of
                                    Immediate ->
                                        Result.Err "the third paramter of an addition cannot be immediate"

                                    Position ->
                                        Result.Ok ( firstMode, secondMode )
                            )
                            firstOpMode
                            secondOpMode
                            thirdOpMode
                            |> Result.andThen identity
                            |> Result.andThen
                                (\( firstMode, secondMode ) ->
                                    Maybe.map3 Add
                                        (Array.get (pointer + 1) memory |> Maybe.map (Tuple.pair firstMode))
                                        (Array.get (pointer + 2) memory |> Maybe.map (Tuple.pair secondMode))
                                        (Array.get (pointer + 3) memory)
                                        |> Result.fromMaybe
                                            ("Error while accessing blocks following "
                                                ++ String.fromInt pointer
                                                ++ " to build an addition operation"
                                            )
                                        |> Result.map (\instruction -> ( instruction, { intCode | pointer = pointer + 4 } ))
                                )

                    2 ->
                        Result.map3
                            (\firstMode secondMode thirdMode ->
                                case thirdMode of
                                    Immediate ->
                                        Result.Err "the third paramter of a multiplication cannot be immediate"

                                    Position ->
                                        Result.Ok ( firstMode, secondMode )
                            )
                            firstOpMode
                            secondOpMode
                            thirdOpMode
                            |> Result.andThen identity
                            |> Result.andThen
                                (\( firstMode, secondMode ) ->
                                    Maybe.map3 Multiply
                                        (Array.get (pointer + 1) memory |> Maybe.map (Tuple.pair firstMode))
                                        (Array.get (pointer + 2) memory |> Maybe.map (Tuple.pair secondMode))
                                        (Array.get (pointer + 3) memory)
                                        |> Result.fromMaybe
                                            ("Error while accessing blocks following "
                                                ++ String.fromInt pointer
                                                ++ " to build a multiplication operation"
                                            )
                                        |> Result.map (\instruction -> ( instruction, { intCode | pointer = pointer + 4 } ))
                                )

                    3 ->
                        Result.map3
                            (\firstMode secondMode thirdMode ->
                                firstMode == Position && secondMode == Position && thirdMode == Position
                            )
                            firstOpMode
                            secondOpMode
                            thirdOpMode
                            |> Result.andThen
                                (\isValid ->
                                    if isValid then
                                        Result.Ok ()

                                    else
                                        Result.Err "the input operation requires all the params to be in position mode"
                                )
                            |> Result.andThen
                                (\_ ->
                                    Maybe.map Input
                                        (Array.get (pointer + 1) memory)
                                        |> Result.fromMaybe ("Error while accessing blocks following " ++ String.fromInt pointer ++ " to build an input operation")
                                )
                            |> Result.map (\instruction -> ( instruction, { intCode | pointer = pointer + 2 } ))

                    4 ->
                        Result.map3
                            (\firstMode secondMode thirdMode ->
                                if secondMode == Position && thirdMode == Position then
                                    Result.Ok firstMode

                                else
                                    Result.Err "the output operation can only have a mode for it's first and only argument"
                            )
                            firstOpMode
                            secondOpMode
                            thirdOpMode
                            |> Result.andThen identity
                            |> Result.andThen
                                (\firstMode ->
                                    Maybe.map Output
                                        (Array.get (pointer + 1) memory |> Maybe.map (Tuple.pair firstMode))
                                        |> Result.fromMaybe ("Error while accessing blocks following " ++ String.fromInt pointer ++ " to build a output operation")
                                )
                            |> Result.map (\instruction -> ( instruction, { intCode | pointer = pointer + 2 } ))

                    99 ->
                        Result.Ok ( Exit, intCode )

                    _ ->
                        Result.Err <| "Wrong opCode: " ++ String.fromInt opCode
            )


performOperation : IntCode -> ( OpMode, Int ) -> ( OpMode, Int ) -> Int -> (Int -> Int -> Int) -> Result String IntCode
performOperation ({ memory } as intCode) ( firstOpMode, firstOp ) ( secondOpMode, secondOp ) resultAddress operation =
    Maybe.map2 operation
        (case firstOpMode of
            Immediate ->
                Just firstOp

            Position ->
                Array.get firstOp memory
        )
        (case secondOpMode of
            Immediate ->
                Just secondOp

            Position ->
                Array.get secondOp memory
        )
        |> Result.fromMaybe "Error cannot acccess memory address for the operands"
        |> Result.andThen
            (\res ->
                if resultAddress >= Array.length memory || resultAddress < 0 then
                    Result.Err "Trying to write the result outside the limit of the memory"

                else
                    Array.set resultAddress res memory
                        |> Result.Ok
            )
        |> Result.map (\newMemory -> { intCode | memory = newMemory })


{--}
runNextInstruction : IntCode -> ( State, Cmd Never )
runNextInstruction intCode =
    case readInstruction intCode of
        Result.Ok ( instruction, { memory } as intCode_ ) ->
            case instruction of
                Add firstOperandAddress secondOperandAddress resultAddress ->
                    ( performOperation intCode_ firstOperandAddress secondOperandAddress resultAddress (+)
                        |> fromResult
                    , Cmd.none
                    )

                Multiply firstOperandAddress secondOperandAddress resultAddress ->
                    ( performOperation intCode_ firstOperandAddress secondOperandAddress resultAddress (*)
                        |> fromResult
                    , Cmd.none
                    )

                Input address ->
                    -- Note for now this is hardcoded
                    ( Array.set address 1 memory
                        |> (\newMemory -> { intCode_ | memory = newMemory })
                        |> Ok
                    , Cmd.none
                    )

                Output ( mode, operand ) ->
                    ( Ok intCode_
                    , (case mode of
                        Immediate ->
                            Just operand

                        Position ->
                            Array.get operand memory
                      )
                        |> Maybe.Extra.unwrap Cmd.none (String.fromInt >> Ports.print)
                    )

                Exit ->
                    ( case Array.get 0 memory of
                        Nothing ->
                            Err "everything went snafu"

                        Just value ->
                            Finished value
                    , Cmd.none
                    )

        Result.Err msg ->
            ( Err msg, Cmd.none )
--}


{--}
runner : ( State, Cmd Never ) -> ( State, Cmd Never )
runner ( state, cmd ) =
    case state of
        Err msg ->
            ( Err msg, cmd )

        Finished value ->
            ( Finished value, cmd )

        Ok intCode ->
            runNextInstruction intCode
                |> (\( newState, newCmd ) -> runner ( newState, Cmd.batch [ newCmd, cmd ] ))
--}


run : State -> Cmd Never
run state =
    runner ( state, Cmd.none )
        |> (\( finalState, cmd ) ->
                case finalState of
                    Err msg ->
                        Cmd.batch
                            [ cmd
                            , Ports.printAndExitFailure <| "woops -> " ++ msg
                            ]

                    Finished value ->
                        Cmd.batch
                            [ cmd
                            , Ports.printAndExitSuccess <| ":tada: " ++ String.fromInt value
                            ]

                    Ok _ ->
                        Cmd.batch
                            [ cmd
                            , Ports.printAndExitFailure "snafu ... "
                            ]
           )
