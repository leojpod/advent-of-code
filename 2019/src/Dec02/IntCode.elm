module Dec02.IntCode exposing
    ( Instruction(..)
    , IntCode
    , State(..)
    , options
    , readInstruction
    , run
    , runNextInstruction
    , runner
    )

import Array exposing (Array)
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.OptionsParser.BuilderState as BuilderState
import Ports
import Result.Extra


type alias IntCode =
    { pointer : Int
    , memory : Array Int
    }


options : OptionsParser.OptionsParser State BuilderState.AnyOptions
options =
    OptionsParser.buildSubCommand "int-code" Ok
        |> OptionsParser.withDoc "run the intCode computer"
        |> OptionsParser.with
            (Option.requiredKeywordArg "instructions"
                |> Option.map (String.split ", ")
                |> Option.validateMap
                    (List.map
                        (String.toInt
                            >> Result.fromMaybe "This is not a valid instruction"
                        )
                        >> Result.Extra.combine
                    )
                |> Option.map (Array.fromList >> Array.set 1 12 >> Array.set 2 2 >> IntCode 0)
            )


type State
    = Err String
    | Ok IntCode
    | Finished Int


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


type Instruction
    = Add Int Int Int
    | Multiply Int Int Int
    | Exit


readInstruction : IntCode -> Result String ( Instruction, IntCode )
readInstruction ({ pointer, memory } as intCode) =
    Array.get pointer memory
        |> Result.fromMaybe ("Error accessing incorrect memory place (" ++ String.fromInt pointer ++ ")")
        |> Result.andThen
            (\opCode ->
                case opCode of
                    1 ->
                        Maybe.map3 Add
                            (Array.get (pointer + 1) memory)
                            (Array.get (pointer + 2) memory)
                            (Array.get (pointer + 3) memory)
                            |> Result.fromMaybe
                                ("Error while accessing blocks following "
                                    ++ String.fromInt pointer
                                    ++ " to build an addition operation"
                                )
                            |> Result.map (\instruction -> ( instruction, { intCode | pointer = pointer + 4 } ))

                    2 ->
                        Maybe.map3 Multiply
                            (Array.get (pointer + 1) memory)
                            (Array.get (pointer + 2) memory)
                            (Array.get (pointer + 3) memory)
                            |> Result.fromMaybe
                                ("Error while accessing blocks following "
                                    ++ String.fromInt pointer
                                    ++ " to build a multiplication operation"
                                )
                            |> Result.map (\instruction -> ( instruction, { intCode | pointer = pointer + 4 } ))

                    99 ->
                        Result.Ok ( Exit, intCode )

                    _ ->
                        Result.Err <| "Wrong opCode: " ++ String.fromInt opCode
            )


performOperation : IntCode -> Int -> Int -> Int -> (Int -> Int -> Int) -> Result String IntCode
performOperation ({ memory } as intCode) firstOperandAddress secondOperandAddress resultAddress operation =
    Maybe.map2 operation
        (Array.get firstOperandAddress memory)
        (Array.get secondOperandAddress memory)
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
runNextInstruction : IntCode -> State
runNextInstruction intCode =
    case readInstruction intCode of
        Result.Ok ( instruction, { memory } as intCode_ ) ->
            case instruction of
                Add firstOperandAddress secondOperandAddress resultAddress ->
                    performOperation intCode_ firstOperandAddress secondOperandAddress resultAddress (+)
                        |> fromResult

                Multiply firstOperandAddress secondOperandAddress resultAddress ->
                    performOperation intCode_ firstOperandAddress secondOperandAddress resultAddress (*)
                        |> fromResult

                Exit ->
                    case Array.get 0 memory of
                        Nothing ->
                            Err "everything went snafu"

                        Just value ->
                            Finished value

        Result.Err msg ->
            Err msg
--}


{--}
runner : State -> State
runner state =
    case state of
        Err msg ->
            Err msg

        Finished value ->
            Finished value

        Ok intCode ->
            runNextInstruction intCode
                |> runner
--}


run : State -> Cmd Never
run state =
    runner state
        |> (\finalState ->
                case finalState of
                    Err msg ->
                        Ports.printAndExitFailure <| "woops -> " ++ msg

                    Finished value ->
                        Ports.printAndExitSuccess <| ":tada: " ++ String.fromInt value

                    Ok _ ->
                        Ports.printAndExitFailure "snafu ... "
           )
