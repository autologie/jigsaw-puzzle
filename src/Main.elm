module Main exposing (main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Random exposing (Seed)
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (..)
import GamePlay
import Model exposing (..)
import Point exposing (Point)
import JigsawPuzzle exposing (Piece(..))


main =
    Browser.document
        { init = \flag -> ( initialModel flag, Cmd.none )
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = view
        , subscriptions = \_ -> Browser.Events.onResize (\x y -> ResizeWindow ( x, y ))
        }


initialModel : ( Int, Int ) -> Model
initialModel windowSize =
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
        { offset = getOffset windowSize ( sizeX, sizeY ) pieceSize
        , sizeX = sizeX
        , sizeY = sizeY
        , pieceSize = pieceSize
        , groups = groups
        , dragging = Nothing
        , seed = seed
        , selection = Nothing
        , selectedGroups = []
        }


generateGroups : Int -> Int -> Int -> Seed -> ( Dict PieceGroupId PieceGroup, Seed )
generateGroups sizeX sizeY pieceSize seed =
    let
        ( pieces, nextSeed ) =
            JigsawPuzzle.generate sizeX sizeY seed

        pieceMap =
            pieces
                |> List.map (\((Piece index hooks) as piece) -> ( index, piece ))
                |> Dict.fromList
    in
        ( Dict.singleton 0
            { pieces = pieceMap
            , isSettled = True
            , zIndex = 0
            , position = defaultPosition pieceSize pieceMap
            }
        , nextSeed
        )


view : Model -> Browser.Document Msg
view model =
    { title = ""
    , body =
        [ Html.div []
            [ GamePlay.view model
            , Html.div [ Html.Attributes.class "control" ]
                [ Html.button [ onClick Scatter ] [ Html.text "Scatter" ]
                , Html.button [ onClick Reset ] [ Html.text "New Puzzle" ]
                ]
            ]
        ]
    }


update : Msg -> Model -> Model
update msg model =
    case ( model.dragging, msg ) of
        ( Nothing, StartDragging targetGroupId touchPosition ) ->
            { model
                | dragging = Just ( targetGroupId, touchPosition )
                , groups =
                    model.groups
                        |> Dict.map (\_ group -> { group | isSettled = False })
            }

        ( Nothing, StartSelection touchPosition ) ->
            let
                selectPosition =
                    touchPosition |> Point.sub model.offset
            in
                { model
                    | selection = Just ( selectPosition, selectPosition )
                }

        ( Just ( targetGroupId, _ ), EndDragging ) ->
            { model
                | dragging = Nothing
                , groups =
                    model.groups
                        |> updateGroupsOnDrop model targetGroupId
                , selectedGroups = []
            }

        ( Nothing, MouseMove mousePosition ) ->
            { model
                | selection =
                    model.selection
                        |> Maybe.map
                            (\( position, _ ) ->
                                ( position
                                , mousePosition |> Point.sub model.offset
                                )
                            )
            }

        ( Just ( draggingGroupId, touchPosition ), MouseMove mousePosition ) ->
            case Dict.get draggingGroupId model.groups of
                Just { position } ->
                    let
                        offsetPosition =
                            Point.origin
                                |> Point.add touchPosition
                                |> Point.sub mousePosition
                                |> Point.add position

                        isDraggingSelectedGroup =
                            List.member draggingGroupId model.selectedGroups

                        updatePosition groupId group =
                            if
                                (groupId == draggingGroupId)
                                    || (isDraggingSelectedGroup
                                            && List.member groupId model.selectedGroups
                                       )
                            then
                                { group
                                    | position =
                                        group.position
                                            |> Point.sub offsetPosition
                                }
                            else
                                group
                    in
                        { model
                            | groups =
                                model.groups
                                    |> Dict.map updatePosition
                        }

                Nothing ->
                    model

        ( Nothing, EndSelection ) ->
            case model.selection of
                Just ( from, to ) ->
                    let
                        ( minSelectionX, minSelectionY ) =
                            Point.min from to

                        ( maxSelectionX, maxSelectionY ) =
                            Point.max from to

                        hasIntersection ( _, { pieces, position } ) =
                            let
                                ( minGroupX, minGroupY ) =
                                    position

                                ( maxGroupX, maxGroupY ) =
                                    pieces
                                        |> Dict.keys
                                        |> List.foldl (\offset passed -> Point.max offset passed) Point.origin
                                        |> Point.scale model.pieceSize
                                        |> Point.add position
                            in
                                (maxSelectionX > minGroupX)
                                    && (minSelectionX < maxGroupX)
                                    && (maxSelectionY > minGroupY)
                                    && (minSelectionY < maxGroupY)
                    in
                        { model
                            | selection = Nothing
                            , selectedGroups =
                                model.groups
                                    |> Dict.toList
                                    |> List.filter hasIntersection
                                    |> List.map (\( groupId, _ ) -> groupId)
                        }

                Nothing ->
                    model

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
                reducePieces piece ( passed, index, seed0 ) =
                    let
                        ( x, seed1 ) =
                            Random.step (Random.int (-1 * model.pieceSize) (model.sizeX * model.pieceSize)) seed0

                        ( y, seed2 ) =
                            Random.step (Random.int (-1 * model.pieceSize) (model.sizeY * model.pieceSize)) seed1
                    in
                        ( Dict.insert
                            index
                            { pieces = Dict.singleton Point.origin piece
                            , position = ( x, y )
                            , isSettled = False
                            , zIndex = 0
                            }
                            passed
                        , index + 1
                        , seed2
                        )

                ( groups, _, seed ) =
                    model.groups
                        |> Dict.toList
                        |> List.concatMap (\( _, { pieces } ) -> Dict.values pieces)
                        |> List.foldl reducePieces ( Dict.empty, 0, model.seed )
            in
                { model
                    | groups = groups
                    , seed = seed
                }

        ( _, ResizeWindow ( x, y ) ) ->
            { model
                | offset = getOffset ( x, y ) ( model.sizeX, model.sizeY ) model.pieceSize
            }

        _ ->
            model


getOffset : Point -> Point -> Int -> Point
getOffset windowSize boardSize pieceSize =
    windowSize
        |> Point.sub (boardSize |> Point.scale pieceSize)
        |> Point.divide 2


defaultPosition : Int -> Dict Point Piece -> Point
defaultPosition pieceSize pieces =
    let
        minIndex =
            case pieces |> Dict.values of
                (Piece headIndex _) :: tail ->
                    List.foldl
                        (\(Piece index _) passed -> Point.min index passed)
                        headIndex
                        tail

                _ ->
                    Point.origin
    in
        minIndex |> Point.scale pieceSize


isCorrectDrop : Model -> PieceGroup -> Bool
isCorrectDrop { pieceSize } group =
    (Point.distance
        group.position
        (defaultPosition pieceSize group.pieces)
    )
        < 10


updateGroupsOnDrop : Model -> PieceGroupId -> Dict PieceGroupId PieceGroup -> Dict PieceGroupId PieceGroup
updateGroupsOnDrop model targetGroupId groups =
    Dict.get targetGroupId groups
        |> Maybe.map
            (\targetGroup ->
                let
                    ( mergeableGroups, restGroups ) =
                        groups
                            |> Dict.partition
                                (\groupId group ->
                                    groupId == targetGroupId || isMergeable model.pieceSize group targetGroup
                                )

                    mergeableGroupList =
                        mergeableGroups
                            |> Dict.toList

                    _ =
                        mergeableGroupList |> List.length

                    mergedGroupId =
                        mergeableGroupList
                            |> List.map (\( id, _ ) -> id)
                            |> List.minimum
                            |> Maybe.withDefault targetGroupId

                    mergedGroupX =
                        mergeableGroupList
                            |> List.map (\( _, { position } ) -> Tuple.first position)
                            |> List.minimum
                            |> Maybe.withDefault 0

                    mergedGroupY =
                        mergeableGroupList
                            |> List.map (\( _, { position } ) -> Tuple.second position)
                            |> List.minimum
                            |> Maybe.withDefault 0

                    mergedGroup =
                        { pieces =
                            mergeableGroupList
                                |> List.foldl
                                    (\( _, { position, pieces } ) passed ->
                                        let
                                            groupPosition =
                                                position
                                                    |> Point.sub ( mergedGroupX, mergedGroupY )
                                                    |> Point.divideRound model.pieceSize

                                            updatedPieces =
                                                pieces
                                                    |> Dict.toList
                                                    |> List.map
                                                        (\( piecePosition, piece ) ->
                                                            ( piecePosition
                                                                |> Point.add groupPosition
                                                            , piece
                                                            )
                                                        )
                                                    |> Dict.fromList
                                        in
                                            Dict.union passed updatedPieces
                                    )
                                    Dict.empty
                        , isSettled = mergeableGroups |> Dict.toList |> List.any (\( _, { isSettled } ) -> isSettled)
                        , position =
                            ( mergedGroupX
                            , mergedGroupY
                            )
                        , zIndex =
                            groups
                                |> Dict.toList
                                |> List.map (\( _, { zIndex } ) -> zIndex + 1)
                                |> List.maximum
                                |> Maybe.withDefault 0
                        }
                in
                    Dict.insert mergedGroupId mergedGroup restGroups
            )
        |> Maybe.withDefault groups
        |> Dict.map
            (\groupId group ->
                if isCorrectDrop model group then
                    { group
                        | position = defaultPosition model.pieceSize group.pieces
                        , isSettled = True
                        , zIndex = 0
                    }
                else
                    group
            )


isMergeable : Int -> PieceGroup -> PieceGroup -> Bool
isMergeable pieceSize group anotherGroup =
    group.pieces
        |> Dict.toList
        |> List.any
            (\( pieceOffset, Piece pieceIndex _ ) ->
                let
                    position =
                        pieceOffset
                            |> Point.scale pieceSize
                            |> Point.add group.position

                    isNear p =
                        Point.distance position p < 10

                    isAdjacentToPiece anotherPieceOffset (Piece anotherPieceIndex _) =
                        let
                            anotherPosition =
                                anotherPieceOffset
                                    |> Point.scale pieceSize
                                    |> Point.add anotherGroup.position

                            diffOfIndex =
                                pieceIndex |> Point.sub anotherPieceIndex
                        in
                            case Point.toString diffOfIndex of
                                "(0,1)" ->
                                    isNear (Point.add anotherPosition ( 0, pieceSize ))

                                "(0,-1)" ->
                                    isNear (Point.add anotherPosition ( 0, -pieceSize ))

                                "(1,0)" ->
                                    isNear (Point.add anotherPosition ( pieceSize, 0 ))

                                "(-1,0)" ->
                                    isNear (Point.add anotherPosition ( -pieceSize, 0 ))

                                _ ->
                                    False
                in
                    anotherGroup.pieces
                        |> Dict.filter isAdjacentToPiece
                        |> Dict.isEmpty
                        |> not
            )
