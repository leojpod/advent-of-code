module Dec04.PasswordFinder exposing
    ( PasswordFinderOptions
    , options
    , run
    , run2
    )

import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.OptionsParser.BuilderState as BuilderState
import List.Extra
import Maybe.Extra
import Ports


type alias PasswordFinderOptions =
    { from : Int
    , to : Int
    }


options : String -> OptionsParser.OptionsParser PasswordFinderOptions BuilderState.AnyOptions
options name =
    OptionsParser.buildSubCommand name PasswordFinderOptions
        |> OptionsParser.withDoc "find out how many password matches from criteria within a given range"
        |> OptionsParser.with
            (Option.requiredPositionalArg "from"
                |> Option.validateMap (String.toInt >> Result.fromMaybe "from is not a valid int value")
            )
        |> OptionsParser.with
            (Option.requiredPositionalArg "to"
                |> Option.validateMap (String.toInt >> Result.fromMaybe "to is not a valid int value")
            )


run : PasswordFinderOptions -> Cmd Never
run { from, to } =
    generatePotentialPasswords
        (if isValidPasswordCandidate from then
            from

         else
            findNextPassword from
        )
        to
        []
        |> List.length
        |> String.fromInt
        |> Ports.printAndExitSuccess


run2 : PasswordFinderOptions -> Cmd Never
run2 { from, to } =
    List.range from to
        |> List.filter isValidPasswordCandidate2
        |> List.length
        |> String.fromInt
        |> Ports.printAndExitSuccess


fromIntToListDigits : Int -> List Int
fromIntToListDigits =
    String.fromInt
        >> String.toList
        >> List.map (String.fromChar >> String.toInt)
        >> Maybe.Extra.combine
        >> Maybe.withDefault []


isValidPasswordCandidate : Int -> Bool
isValidPasswordCandidate =
    fromIntToListDigits
        >> List.Extra.uncons
        >> Maybe.Extra.unwrap False
            (\( head, tails ) ->
                List.foldl
                    (\digit ( prevDigit, neverDecrease, hasDouble ) ->
                        ( digit
                        , neverDecrease && (digit >= prevDigit)
                        , hasDouble || (digit == prevDigit)
                        )
                    )
                    ( head, True, False )
                    tails
                    |> (\( _, neverDecrease, hasDouble ) -> neverDecrease && hasDouble)
            )


isValidPasswordCandidate2 : Int -> Bool
isValidPasswordCandidate2 =
    fromIntToListDigits
        >> (\digits ->
                (List.Extra.groupWhile (<=) digits |> List.length |> (==) 1)
                    && (List.Extra.group digits
                            |> List.any
                                (Tuple.second >> List.length >> (==) 1)
                       )
           )


generatePotentialPasswords : Int -> Int -> List Int -> List Int
generatePotentialPasswords from to foundSoFar =
    if from > to then
        foundSoFar

    else
        findNextPassword from
            |> (\newFrom -> generatePotentialPasswords newFrom to (from :: foundSoFar))


findNextPassword : Int -> Int
findNextPassword =
    (+) 1
        >> fromIntToListDigits
        >> List.Extra.uncons
        >> Maybe.Extra.unwrap []
            (\( head, tails ) ->
                List.foldl
                    (\digit ( prevDigit, hasDouble, newDigits ) ->
                        if digit < prevDigit then
                            ( prevDigit, True, prevDigit :: newDigits )

                        else
                            ( digit, hasDouble || digit == prevDigit, prevDigit :: newDigits )
                    )
                    ( head, False, [] )
                    tails
                    |> (\( lastDigit, hasDouble, newDigits ) ->
                            if hasDouble then
                                lastDigit :: newDigits

                            else
                                lastDigit
                                    :: lastDigit
                                    :: (List.tail newDigits |> Maybe.withDefault [])
                       )
                    |> List.reverse
            )
        >> List.map String.fromInt
        >> String.join ""
        >> String.toInt
        -- this should by construction never ever happen
        >> Maybe.withDefault 0
