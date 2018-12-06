module Board
    exposing
        ( Model
        , Msg(..)
        , view
        , update
        , initialModel
        , scatterPieces
        , withScreenSize
        )

import Json.Decode as Decode exposing (Decoder)
import Random exposing (Seed)
import Svg exposing (..)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Svg.Keyed
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Svg.Lazy as Lazy
import Point exposing (Point)
import JigsawPuzzle exposing (Piece(..), Hook(..))
import PieceGroup


type Msg
    = StartDragging GroupId Point
    | EndDragging
    | MouseMove Point
    | StartSelection Point
    | EndSelection Bounds


type alias Model =
    { offset : Point
    , sizeX : Int
    , sizeY : Int
    , pieceSize : Int
    , groups : Dict GroupId PieceGroup.Model
    , selection : Maybe Bounds
    }


type alias Bounds =
    ( Point, Point )


type alias GroupId =
    Int


initialModel : Point -> Seed -> ( Model, Seed )
initialModel screenSize seed =
    let
        sizeX =
            15

        sizeY =
            10

        pieceSize =
            50

        ( groups, updatedSeed ) =
            generateGroups sizeX sizeY pieceSize seed
    in
        ( { offset = getOffset screenSize ( sizeX, sizeY ) pieceSize
          , sizeX = sizeX
          , sizeY = sizeY
          , pieceSize = pieceSize
          , groups = groups
          , selection = Nothing
          }
        , updatedSeed
        )


view : Model -> Html Msg
view { offset, sizeX, sizeY, pieceSize, groups, selection } =
    svg
        ([ width "100vw"
         , height "100vh"
         , viewBox "0 0 100vw 100vh"
         , on "mousemove" (Decode.map MouseMove decodeMouseEvent)
         , on "mousedown" (Decode.map (\position -> StartSelection position) decodeMouseEvent)
         ]
            ++ (selection
                    |> Maybe.map (\sel -> [ on "mouseup" (Decode.succeed (EndSelection sel)) ])
                    |> Maybe.withDefault []
               )
        )
        [ Svg.Keyed.node "g"
            [ transform ("translate" ++ (Point.toString offset)) ]
            (List.concat
                [ [ ( "board"
                    , rect
                        [ Svg.Attributes.style "fill: #eee;"
                        , width (String.fromInt (sizeX * pieceSize))
                        , height (String.fromInt (sizeY * pieceSize))
                        ]
                        []
                    )
                  ]
                , (groups
                    |> Dict.toList
                    |> List.sortBy (groupZIndex (groups |> Dict.values))
                    |> List.map (groupView pieceSize)
                  )
                ]
            )
        ]


update : Msg -> Model -> Model
update msg model =
    case
        ( model.selection
        , model.groups
            |> Dict.values
            |> List.filterMap
                (\group ->
                    case group.dragHandle of
                        Just handle ->
                            Just ( group, handle )

                        Nothing ->
                            Nothing
                )
            |> List.head
        , msg
        )
    of
        ( _, Nothing, StartDragging targetGroupId touchPosition ) ->
            { model
                | groups =
                    model.groups
                        |> Dict.map
                            (\id group ->
                                { group
                                    | dragHandle =
                                        if id == targetGroupId then
                                            Just touchPosition
                                        else
                                            Nothing
                                }
                            )
            }

        ( _, Nothing, StartSelection touchPosition ) ->
            let
                selectPosition =
                    touchPosition |> Point.sub model.offset
            in
                { model
                    | selection = Just ( selectPosition, selectPosition )
                }

        ( _, Just ( draggingGroup, _ ), EndDragging ) ->
            { model
                | groups =
                    model.groups
                        |> updateGroupsOnDrop model.pieceSize draggingGroup
            }

        ( Just ( position, _ ), Nothing, MouseMove mousePosition ) ->
            let
                selection =
                    ( position
                    , mousePosition |> Point.sub model.offset
                    )

                selectedGroups =
                    model.groups
                        |> getSelection selection model.pieceSize
            in
                { model
                    | selection = Just selection
                    , groups =
                        model.groups
                            |> Dict.map
                                (\id group ->
                                    { group | isSelected = List.member id selectedGroups }
                                )
                }

        ( _, Just ( draggingGroup, touchPosition ), MouseMove mousePosition ) ->
            let
                offsetPosition =
                    Point.origin
                        |> Point.add touchPosition
                        |> Point.sub mousePosition
                        |> Point.add draggingGroup.position

                isDraggingSelectedGroup =
                    isSelectedGroupDragged (model.groups |> Dict.values)

                updatePosition _ group =
                    if
                        (group == draggingGroup)
                            || (isDraggingSelectedGroup && group.isSelected)
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

        ( _, Nothing, EndSelection selection ) ->
            let
                selectedGroups =
                    model.groups
                        |> getSelection selection model.pieceSize
            in
                { model
                    | selection = Nothing
                    , groups =
                        model.groups
                            |> Dict.map
                                (\id group ->
                                    { group | isSelected = List.member id selectedGroups }
                                )
                }

        _ ->
            model


groupView : Int -> ( GroupId, PieceGroup.Model ) -> ( String, Svg.Svg Msg )
groupView pieceSize ( groupId, group ) =
    let
        toMsg msg =
            case msg of
                PieceGroup.StartDragging point ->
                    StartDragging groupId point

                PieceGroup.EndDragging ->
                    EndDragging
    in
        (Lazy.lazy2
            PieceGroup.view
            pieceSize
            group
        )
            |> Svg.map toMsg
            |> (\element -> ( String.fromInt groupId, element ))


groupZIndex : List PieceGroup.Model -> ( a, PieceGroup.Model ) -> Int
groupZIndex groups ( _, group ) =
    let
        isDragged =
            group.dragHandle
                |> Maybe.map (\_ -> True)
                |> Maybe.withDefault False

        addition =
            if isDragged || (group.isSelected && isSelectedGroupDragged groups) then
                1000
            else
                0
    in
        group.zIndex + addition


isSelectedGroupDragged : List PieceGroup.Model -> Bool
isSelectedGroupDragged groups =
    groups
        |> List.any
            (\{ dragHandle, isSelected } ->
                case ( dragHandle, isSelected ) of
                    ( Just _, True ) ->
                        True

                    _ ->
                        False
            )


decodeMouseEvent : Decode.Decoder Point
decodeMouseEvent =
    Decode.map2 (\eventX eventY -> ( eventX, eventY ))
        (Decode.at [ "offsetX" ] Decode.int)
        (Decode.at [ "offsetY" ] Decode.int)


getSelection : Bounds -> Int -> Dict GroupId PieceGroup.Model -> List GroupId
getSelection ( from, to ) pieceSize groups =
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
                        |> Point.scale pieceSize
                        |> Point.add position
            in
                (maxSelectionX > minGroupX)
                    && (minSelectionX < maxGroupX)
                    && (maxSelectionY > minGroupY)
                    && (minSelectionY < maxGroupY)
    in
        groups
            |> Dict.toList
            |> List.filter hasIntersection
            |> List.map (\( groupId, _ ) -> groupId)


generateGroups : Int -> Int -> Int -> Seed -> ( Dict GroupId PieceGroup.Model, Seed )
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
            , zIndex = 0
            , position = defaultPosition pieceSize pieceMap
            , dragHandle = Nothing
            , isSelected = False
            }
        , nextSeed
        )


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


isCorrectDrop : Int -> PieceGroup.Model -> Bool
isCorrectDrop pieceSize group =
    (Point.distance
        group.position
        (defaultPosition pieceSize group.pieces)
    )
        < 10


updateGroupsOnDrop : Int -> PieceGroup.Model -> Dict GroupId PieceGroup.Model -> Dict GroupId PieceGroup.Model
updateGroupsOnDrop pieceSize targetGroup groups =
    let
        ( mergeableGroups, restGroups ) =
            groups
                |> Dict.partition
                    (\_ group ->
                        group == targetGroup || isMergeable pieceSize group targetGroup
                    )

        mergeableGroupList =
            mergeableGroups
                |> Dict.toList

        ( mergedGroupId, mergedGroupPosition ) =
            mergeableGroupList
                |> List.foldl
                    (\( groupId, { position } ) ( passedGroupId, passedPosition ) ->
                        ( Basics.max passedGroupId groupId
                        , Point.min position passedPosition
                        )
                    )
                    ( 0, targetGroup.position )

        mergedGroup =
            { pieces =
                mergeableGroupList
                    |> List.foldl
                        (\( _, { position, pieces } ) passed ->
                            let
                                groupPosition =
                                    position
                                        |> Point.sub mergedGroupPosition
                                        |> Point.divideRound pieceSize

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
            , position = mergedGroupPosition
            , zIndex =
                groups
                    |> Dict.toList
                    |> List.map (\( _, { zIndex } ) -> zIndex + 1)
                    |> List.maximum
                    |> Maybe.withDefault 0
            , isSelected = False
            , dragHandle = Nothing
            }
    in
        Dict.insert mergedGroupId mergedGroup restGroups
            |> Dict.map
                (\groupId group ->
                    if isCorrectDrop pieceSize group then
                        { group
                            | position = defaultPosition pieceSize group.pieces
                            , zIndex = 0
                            , dragHandle = Nothing
                            , isSelected = False
                        }
                    else
                        { group
                            | dragHandle = Nothing
                            , isSelected = False
                        }
                )


isMergeable : Int -> PieceGroup.Model -> PieceGroup.Model -> Bool
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


scatterPieces : Seed -> Model -> ( Model, Seed )
scatterPieces initialSeed model =
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
                    , zIndex = 0
                    , dragHandle = Nothing
                    , isSelected = False
                    }
                    passed
                , index + 1
                , seed2
                )

        ( groups, _, seed ) =
            model.groups
                |> Dict.toList
                |> List.concatMap (\( _, { pieces } ) -> Dict.values pieces)
                |> List.foldl reducePieces ( Dict.empty, 0, initialSeed )
    in
        ( { model | groups = groups }
        , seed
        )


withScreenSize : Point -> Model -> Model
withScreenSize size model =
    { model
        | offset =
            getOffset
                size
                ( model.sizeX, model.sizeY )
                model.pieceSize
    }


getOffset : Point -> Point -> Int -> Point
getOffset screenSize boardSize pieceSize =
    screenSize
        |> Point.sub (boardSize |> Point.scale pieceSize)
        |> Point.divide 2
