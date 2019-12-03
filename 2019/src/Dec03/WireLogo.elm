module Dec03.WireLogo exposing
    ( Move(..)
    , Path
    , options
    , pathFromMoves
    , run
    , runPart2
    )

import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.OptionsParser.BuilderState as BuilderState
import List
import List.Extra
import Ports
import Result.Extra
import Set


type Move
    = Left Int
    | Down Int
    | Up Int
    | Right Int


type alias Path =
    List ( Int, Int )


pathFromMoves : List Move -> Path
pathFromMoves =
    List.foldl
        (\move ( ( lastX, lastY ), positions ) ->
            case move of
                Left distance ->
                    ( ( lastX - distance, lastY )
                    , List.range (lastX - distance) lastX
                        |> List.map (\x -> ( x, lastY ))
                        |> List.drop 1
                        |> (\newPositions -> newPositions ++ positions)
                    )

                Down distance ->
                    ( ( lastX, lastY - distance )
                    , List.range (lastY - distance) lastY
                        |> List.map (\y -> ( lastX, y ))
                        |> List.drop 1
                        |> (\newPositions -> newPositions ++ positions)
                    )

                Up distance ->
                    ( ( lastX, lastY + distance )
                    , List.range lastY (lastY + distance)
                        |> List.map (\y -> ( lastX, y ))
                        |> List.reverse
                        |> List.drop 1
                        |> (\newPositions -> newPositions ++ positions)
                    )

                Right distance ->
                    ( ( lastX + distance, lastY )
                    , List.range lastX (lastX + distance)
                        |> List.map (\x -> ( x, lastY ))
                        |> List.reverse
                        |> List.drop 1
                        |> (\newPositions -> newPositions ++ positions)
                    )
        )
        ( ( 0, 0 ), [] )
        >> (\( lastPosition, positions ) -> lastPosition :: positions)
        >> List.reverse


options : String -> OptionsParser.OptionsParser (List (List Move)) BuilderState.AnyOptions
options cmdName =
    OptionsParser.buildSubCommand cmdName identity
        |> OptionsParser.withDoc "run the wire-crossing detection algo-thingy"
        |> OptionsParser.with
            (Option.requiredPositionalArg "wires-moves"
                |> Option.validateMap
                    (String.lines
                        >> List.map
                            (String.split ","
                                >> List.map fromInstructionToMove
                                >> Result.Extra.combine
                            )
                        >> Result.Extra.combine
                    )
            )


fromInstructionToMove : String -> Result String Move
fromInstructionToMove string =
    let
        direction =
            String.left 1 string

        maybeDistance =
            String.dropLeft 1 string |> String.toInt
    in
    maybeDistance
        |> Result.fromMaybe (String.dropLeft 1 string ++ " is not a valid distance")
        |> Result.andThen
            (\distance ->
                case direction of
                    "L" ->
                        Ok <| Left distance

                    "D" ->
                        Ok <| Down distance

                    "U" ->
                        Ok <| Up distance

                    "R" ->
                        Ok <| Right distance

                    _ ->
                        Err "invalid direction"
            )


run : List (List Move) -> Cmd Never
run =
    List.map pathFromMoves
        >> List.map (List.drop 1)
        >> List.map Set.fromList
        >> List.Extra.uncons
        >> Maybe.map
            (\( firstPaths, restOfThem ) ->
                restOfThem
                    |> List.foldl
                        Set.intersect
                        firstPaths
            )
        >> (\potentialIntersections ->
                case potentialIntersections of
                    Nothing ->
                        Ports.printAndExitFailure "I need more than one path to find intersections"

                    Just intersections ->
                        intersections
                            |> Set.map (\( a, b ) -> abs a + abs b)
                            |> Set.toList
                            |> List.head
                            |> (\maybeSolution ->
                                    case maybeSolution of
                                        Nothing ->
                                            Ports.printAndExitFailure "looks like I couldn't find a proper solution ... "

                                        Just solution ->
                                            Ports.printAndExitSuccess <| String.fromInt solution
                               )
           )


runPart2 : List (List Move) -> Cmd Never
runPart2 =
    List.map pathFromMoves
        >> List.map (List.drop 1)
        >> (\paths ->
                List.map Set.fromList paths
                    |> List.Extra.uncons
                    |> Maybe.map
                        (\( firstPaths, restOfThem ) ->
                            restOfThem
                                |> List.foldl
                                    Set.intersect
                                    firstPaths
                        )
                    |> (\potentialIntersections ->
                            case potentialIntersections of
                                Nothing ->
                                    Ports.printAndExitFailure "I need more than one path to find intersections"

                                Just intersections ->
                                    intersections
                                        |> Set.map
                                            (\position ->
                                                paths
                                                    |> List.map (List.Extra.elemIndex position >> Maybe.withDefault 0)
                                                    |> List.sum
                                            )
                                        |> Set.toList
                                        |> List.head
                                        |> Maybe.withDefault 0
                                        |> (+) (List.length paths)
                                        |> String.fromInt
                                        |> Ports.printAndExitSuccess
                       )
           )
