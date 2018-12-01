module Main exposing (..)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Random exposing (Seed)
import Model exposing (..)
import View exposing (view)
import Point exposing (Point)


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
        { offset = offset windowSize ( sizeX, sizeY ) pieceSize
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
        plain =
            plainPieces sizeX sizeY
                |> List.map (\((Piece point _) as piece) -> ( point, piece ))
                |> Dict.fromList

        trimX x hooks =
            if x == 0 then
                { hooks | west = None }
            else if x == sizeX - 1 then
                { hooks | east = None }
            else
                hooks

        trimY y hooks =
            if y == 0 then
                { hooks | north = None }
            else if y == sizeY - 1 then
                { hooks | south = None }
            else
                hooks

        reducePieces _ (Piece (( pieceX, pieceY ) as position) hooks) ( pieces, mySeed ) =
            let
                ( generatedHooks, updatedSeed ) =
                    generateHooks mySeed
            in
                ( withHooksAssigned
                    position
                    (generatedHooks
                        |> trimX pieceX
                        |> trimY pieceY
                    )
                    pieces
                , updatedSeed
                )

        ( generatedPieces, lastSeed ) =
            plain |> Dict.foldl reducePieces ( plain, seed )
    in
        ( Dict.singleton 0
            { pieces = generatedPieces
            , isSettled = True
            , zIndex = 0
            , position = defaultPosition pieceSize generatedPieces
            }
        , lastSeed
        )


plainPieces : Int -> Int -> List Piece
plainPieces sizeX sizeY =
    List.range 0 (sizeX - 1)
        |> List.concatMap
            (\x ->
                List.range 0 (sizeY - 1)
                    |> List.map
                        (\y ->
                            Piece
                                ( x, y )
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


withHooksAssigned : Point -> Hooks -> Dict Point Piece -> Dict Point Piece
withHooksAssigned targetPosition { north, east, south, west } pieces =
    let
        assignHooks =
            \_ (Piece position hooks) ->
                let
                    updatedHooks =
                        case
                            (targetPosition
                                |> Point.sub position
                                |> Point.toString
                            )
                        of
                            "(0,0)" ->
                                { north = north
                                , east = east
                                , south = south
                                , west = west
                                }

                            "(-1,0)" ->
                                { hooks | west = negate east }

                            "(1,0)" ->
                                { hooks | east = negate west }

                            "(0,-1)" ->
                                { hooks | north = negate south }

                            "(0,1)" ->
                                { hooks | south = negate north }

                            _ ->
                                hooks
                in
                    Piece position updatedHooks
    in
        pieces |> Dict.map assignHooks


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
                Just ( ( fromX, fromY ), ( toX, toY ) ) ->
                    let
                        minSelectionX =
                            min fromX toX

                        maxSelectionX =
                            max fromX toX

                        minSelectionY =
                            min fromY toY

                        maxSelectionY =
                            max fromY toY
                    in
                        { model
                            | selection = Nothing
                            , selectedGroups =
                                model.groups
                                    |> Dict.toList
                                    |> List.filter
                                        (\( _, { pieces, position } ) ->
                                            let
                                                ( minGroupX, minGroupY ) =
                                                    position

                                                maxGroupX =
                                                    minGroupX
                                                        + model.pieceSize
                                                        * (pieces
                                                            |> Dict.keys
                                                            |> List.map Tuple.first
                                                            |> List.maximum
                                                            |> Maybe.withDefault 0
                                                          )

                                                maxGroupY =
                                                    minGroupY
                                                        + model.pieceSize
                                                        * (pieces
                                                            |> Dict.keys
                                                            |> List.map Tuple.second
                                                            |> List.maximum
                                                            |> Maybe.withDefault 0
                                                          )
                                            in
                                                (maxSelectionX > minGroupX)
                                                    && (minSelectionX < maxGroupX)
                                                    && (maxSelectionY > minGroupY)
                                                    && (minSelectionY < maxGroupY)
                                        )
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
                | offset = offset ( x, y ) ( model.sizeX, model.sizeY ) model.pieceSize
            }

        _ ->
            model


offset : Point -> Point -> Int -> Point
offset windowSize boardSize pieceSize =
    windowSize
        |> Point.sub (boardSize |> Point.scale pieceSize)
        |> Point.divide 2


defaultPosition : Int -> Dict Point Piece -> Point
defaultPosition pieceSize pieces =
    let
        minPosition =
            case pieces |> Dict.values of
                (Piece headPosition _) :: tail ->
                    List.foldl
                        (\(Piece position _) passed -> Point.min position passed)
                        headPosition
                        tail

                _ ->
                    Point.origin
    in
        minPosition |> Point.scale pieceSize


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
            (\( pieceOffset, Piece piecePosition _ ) ->
                let
                    position =
                        pieceOffset
                            |> Point.scale pieceSize
                            |> Point.add group.position
                in
                    anotherGroup.pieces
                        |> Dict.filter
                            (\anotherPieceOffset (Piece anotherPiecePosition _) ->
                                let
                                    anotherPosition =
                                        anotherPieceOffset
                                            |> Point.scale pieceSize
                                            |> Point.add anotherGroup.position
                                in
                                    case
                                        (piecePosition
                                            |> Point.sub anotherPiecePosition
                                            |> Point.toString
                                        )
                                    of
                                        "(0,1)" ->
                                            Point.distance position (Point.add anotherPosition ( 0, pieceSize )) < 10

                                        "(0,-1)" ->
                                            Point.distance position (Point.add anotherPosition ( 0, -pieceSize )) < 10

                                        "(1,0)" ->
                                            Point.distance position (Point.add anotherPosition ( pieceSize, 0 )) < 10

                                        "(-1,0)" ->
                                            Point.distance position (Point.add anotherPosition ( -pieceSize, 0 )) < 10

                                        _ ->
                                            False
                            )
                        |> Dict.isEmpty
                        |> not
            )
