module Main exposing (..)

import Browser
import Random exposing (Seed)
import Model exposing (..)
import View exposing (view)


main =
    Browser.sandbox
        { init = initialModel
        , update = update
        , view = view
        }


initialModel : Model
initialModel =
    let
        sizeX =
            15

        sizeY =
            10

        pieceSize =
            50

        ( groups, seed ) =
            generateGroups sizeX sizeY pieceSize (Random.initialSeed 0)
    in
        { sizeX = sizeX
        , sizeY = sizeY
        , pieceSize = pieceSize
        , groups = groups
        , dragging = Nothing
        , seed = seed
        }


generateGroups sizeX sizeY pieceSize seed =
    let
        plain =
            plainPieces sizeX sizeY

        trimX position hooks =
            if position.x == 0 then
                { hooks | west = None }
            else if position.x == sizeX - 1 then
                { hooks | east = None }
            else
                hooks

        trimY position hooks =
            if position.y == 0 then
                { hooks | north = None }
            else if position.y == sizeY - 1 then
                { hooks | south = None }
            else
                hooks

        reducePieces (Piece position hooks) ( pieces, mySeed ) =
            let
                ( generatedHooks, updatedSeed ) =
                    generateHooks mySeed
            in
                ( withHooksAssigned position
                    (generatedHooks
                        |> trimX position
                        |> trimY position
                    )
                    pieces
                , updatedSeed
                )

        ( generatedPieces, lastSeed ) =
            plain |> List.foldl reducePieces ( plain, seed )
    in
        ( generatedPieces
            |> List.map
                (\piece ->
                    { pieces = [ piece ]
                    , position = defaultPosition pieceSize [ piece ]
                    , isSettled = True
                    }
                )
        , lastSeed
        )


plainPieces sizeX sizeY =
    List.range 0 (sizeX - 1)
        |> List.concatMap
            (\x ->
                List.range 0 (sizeY - 1)
                    |> List.map
                        (\y ->
                            Piece
                                { x = x
                                , y = y
                                }
                                { north = None
                                , east = None
                                , south = None
                                , west = None
                                }
                        )
            )


generateHooks : Seed -> ( Hooks, Seed )
generateHooks seed0 =
    let
        maxDeviation =
            0.06

        deviationGenerator =
            Random.float -maxDeviation maxDeviation

        hookTypeGenerator =
            Random.uniform (Positive 0 0) [ (Negative 0 0) ]

        withDeviation ( hook, seed00 ) =
            let
                ( positionDeviation, seed01 ) =
                    Random.step deviationGenerator seed00

                ( sizeDeviation, seed02 ) =
                    Random.step deviationGenerator seed01
            in
                case hook of
                    Positive _ _ ->
                        ( Positive positionDeviation sizeDeviation, seed02 )

                    Negative _ _ ->
                        ( Negative positionDeviation sizeDeviation, seed02 )

                    None ->
                        ( None, seed02 )

        ( north, seed1 ) =
            Random.step hookTypeGenerator seed0 |> withDeviation

        ( east, seed2 ) =
            Random.step hookTypeGenerator seed1 |> withDeviation

        ( south, seed3 ) =
            Random.step hookTypeGenerator seed2 |> withDeviation

        ( west, seed4 ) =
            Random.step hookTypeGenerator seed3 |> withDeviation
    in
        ( { north = north
          , east = east
          , south = south
          , west = west
          }
        , seed4
        )


withHooksAssigned : Position -> Hooks -> List Piece -> List Piece
withHooksAssigned { x, y } { north, east, south, west } pieces =
    let
        distanceTo position =
            ( x - position.x |> String.fromInt
            , y - position.y |> String.fromInt
            )

        assignHooks =
            \(Piece position hooks) ->
                let
                    updatedHooks =
                        case distanceTo position of
                            ( "0", "0" ) ->
                                { north = north
                                , east = east
                                , south = south
                                , west = west
                                }

                            ( "-1", "0" ) ->
                                { hooks | west = negate east }

                            ( "1", "0" ) ->
                                { hooks | east = negate west }

                            ( "0", "-1" ) ->
                                { hooks | north = negate south }

                            ( "0", "1" ) ->
                                { hooks | south = negate north }

                            _ ->
                                hooks
                in
                    Piece position updatedHooks
    in
        pieces |> List.map assignHooks


negate hook =
    case hook of
        None ->
            None

        Positive positionDeviation sizeDiviation ->
            Negative -positionDeviation sizeDiviation

        Negative positionDeviation sizeDiviation ->
            Positive -positionDeviation sizeDiviation


update : Msg -> Model -> Model
update msg model =
    case ( model.dragging, msg ) of
        ( Nothing, StartDragging targetPiece touchPosition ) ->
            { model
                | dragging = Just ( targetPiece, touchPosition )
                , groups =
                    model.groups
                        |> List.map (\group -> { group | isSettled = False })
                        |> List.sortBy (sortOrderOfPiece targetPiece)
            }

        ( Just ( targetPiece, _ ), EndDragging ) ->
            { model
                | dragging = Nothing
                , groups =
                    model.groups
                        |> List.map (updatePieceOnDrop model targetPiece)
                        |> List.sortBy (sortOrderOfPiece targetPiece)
            }

        ( Just ( draggingPiece, ( touchX, touchY ) ), MouseMove ( x, y ) ) ->
            let
                updatePosition group =
                    if group.pieces == [ draggingPiece ] then
                        { group | position = ( x - touchX, y - touchY ) }
                    else
                        group
            in
                { model | groups = model.groups |> List.map updatePosition }

        ( _, Reset ) ->
            let
                ( groups, seed ) =
                    generateGroups
                        model.sizeX
                        model.sizeY
                        model.pieceSize
                        model.seed
            in
                { model
                    | dragging = Nothing
                    , groups = groups
                    , seed = seed
                }

        ( _, Scatter ) ->
            let
                reduceGroups group ( passed, seed0 ) =
                    let
                        ( x, seed1 ) =
                            Random.step (Random.int 0 ((model.sizeX - 1) * model.pieceSize)) seed0

                        ( y, seed2 ) =
                            Random.step (Random.int 0 ((model.sizeY - 1) * model.pieceSize)) seed1
                    in
                        ( List.concat
                            [ passed
                            , [ { group | position = ( x, y ), isSettled = False } ]
                            ]
                        , seed2
                        )

                ( groups, seed ) =
                    model.groups |> List.foldl reduceGroups ( [], model.seed )
            in
                { model
                    | groups = groups
                    , seed = seed
                }

        _ ->
            model


sortOrderOfPiece : Piece -> PieceGroup -> Int
sortOrderOfPiece targetPiece group =
    if group.isSettled then
        -1
    else if group.pieces == [ targetPiece ] then
        1
    else
        0


defaultPosition : Int -> List Piece -> ( Int, Int )
defaultPosition pieceSize pieces =
    case pieces |> List.head of
        Just (Piece { x, y } _) ->
            ( x * pieceSize, y * pieceSize )

        Nothing ->
            ( 0, 0 )


isCorrectDrop : Model -> PieceGroup -> Bool
isCorrectDrop { pieceSize } group =
    let
        ( actualX, actualY ) =
            group.position

        ( defaultX, defaultY ) =
            defaultPosition pieceSize group.pieces

        distance =
            sqrt (toFloat (defaultX - actualX) ^ 2 + toFloat (defaultY - actualY) ^ 2)
    in
        distance < 10


updatePieceOnDrop : Model -> Piece -> PieceGroup -> PieceGroup
updatePieceOnDrop model targetPiece group =
    if group.pieces /= [ targetPiece ] then
        group
    else if isCorrectDrop model group then
        { group
            | position = defaultPosition model.pieceSize group.pieces
            , isSettled = True
        }
    else
        let
            ( pX, pY ) =
                group.position

            withSnapApplied ( min, max ) position =
                if position < min + 10 then
                    min
                else if position > max - 10 then
                    max
                else
                    position
        in
            { group
                | position =
                    ( withSnapApplied ( 0, (model.sizeX - 1) * model.pieceSize ) pX
                    , withSnapApplied ( 0, (model.sizeY - 1) * model.pieceSize ) pY
                    )
            }
