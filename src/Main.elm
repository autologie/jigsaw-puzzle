module Main exposing (..)

import Browser
import Dict exposing (Dict)
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


generateGroups : Int -> Int -> Int -> Seed -> ( Dict PieceGroupId PieceGroup, Seed )
generateGroups sizeX sizeY pieceSize seed =
    let
        plain =
            plainPieces sizeX sizeY
                |> List.map (\((Piece { x, y } _) as piece) -> ( ( x, y ), piece ))
                |> Dict.fromList

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

        reducePieces _ (Piece position hooks) ( pieces, mySeed ) =
            let
                ( generatedHooks, updatedSeed ) =
                    generateHooks mySeed
            in
                ( withHooksAssigned
                    position
                    (generatedHooks
                        |> trimX position
                        |> trimY position
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


withHooksAssigned : Position -> Hooks -> Dict ( Int, Int ) Piece -> Dict ( Int, Int ) Piece
withHooksAssigned { x, y } { north, east, south, west } pieces =
    let
        distanceTo position =
            ( x - position.x |> String.fromInt
            , y - position.y |> String.fromInt
            )

        assignHooks =
            \_ (Piece position hooks) ->
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

        ( Just ( targetGroupId, _ ), EndDragging ) ->
            { model
                | dragging = Nothing
                , groups =
                    model.groups
                        |> updateGroupsOnDrop model targetGroupId
            }

        ( Just ( draggingGroupId, ( touchX, touchY ) ), MouseMove ( x, y ) ) ->
            let
                updatePosition groupId group =
                    if groupId == draggingGroupId then
                        { group | position = ( x - touchX, y - touchY ) }
                    else
                        group
            in
                { model | groups = model.groups |> Dict.map updatePosition }

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
                            Random.step (Random.int 0 ((model.sizeX - 1) * model.pieceSize)) seed0

                        ( y, seed2 ) =
                            Random.step (Random.int 0 ((model.sizeY - 1) * model.pieceSize)) seed1
                    in
                        ( Dict.insert
                            index
                            { pieces = Dict.singleton ( 0, 0 ) piece
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

        _ ->
            model


defaultPosition : Int -> Dict ( Int, Int ) Piece -> ( Int, Int )
defaultPosition pieceSize pieces =
    let
        ( minX, minY ) =
            pieces
                |> Dict.foldl
                    (\_ (Piece { x, y } _) ( passedX, passedY ) ->
                        ( min passedX x, min passedY y )
                    )
                    ( 0, 0 )
    in
        ( minX * pieceSize, minY * pieceSize )


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
                                    groupId == targetGroupId || isMergeable group targetGroup
                                )

                    mergeableGroupList =
                        mergeableGroups
                            |> Dict.toList

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
                                            groupX =
                                                (Tuple.first position - mergedGroupX) // model.pieceSize

                                            groupY =
                                                (Tuple.second position - mergedGroupY) // model.pieceSize

                                            updatedPieces =
                                                pieces
                                                    |> Dict.toList
                                                    |> List.map (\( ( x, y ), piece ) -> ( ( x + groupX, y + groupY ), piece ))
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
                if groupId /= targetGroupId then
                    group
                else if isCorrectDrop model group then
                    { group
                        | position = defaultPosition model.pieceSize group.pieces
                        , isSettled = True
                        , zIndex = 0
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
            )


isMergeable : PieceGroup -> PieceGroup -> Bool
isMergeable group anotherGroup =
    False
