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
import Svg.Keyed
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Svg.Lazy as Lazy
import Point exposing (Point)
import JigsawPuzzle exposing (Piece(..), Hook(..))
import PieceGroup


type Msg
    = StartDragging PieceGroup.Model Point
    | EndDragging
    | MouseMove Point
    | StartSelection Point
    | EndSelection Bounds


type alias Model =
    { offset : Point
    , sizeX : Int
    , sizeY : Int
    , pieceSize : Int
    , groups : List PieceGroup.Model
    , selection : Maybe Bounds
    , tolerance : Float
    }


type alias Bounds =
    ( Point, Point )


initialModel : Point -> Seed -> ( Model, Seed )
initialModel screenSize seed =
    let
        sizeX =
            10

        sizeY =
            6

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
          , tolerance = 10
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
                    |> List.sortBy (groupZIndex groups)
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
        ( _, Nothing, StartDragging targetGroup touchPosition ) ->
            model
                |> withGroupsUpdated
                    (\group ->
                        { group
                            | dragHandle =
                                if PieceGroup.isSame group targetGroup then
                                    Just touchPosition
                                else
                                    Nothing
                        }
                    )

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
                | groups = model.groups |> drop model.pieceSize model.tolerance draggingGroup
            }

        ( Just ( position, _ ), Nothing, MouseMove mousePosition ) ->
            let
                selection =
                    ( position
                    , mousePosition |> Point.sub model.offset
                    )
            in
                { model | selection = Just selection }
                    |> withGroupsUpdated
                        (\group ->
                            { group | isSelected = isSelected selection model.pieceSize group }
                        )

        ( _, Just ( draggingGroup, touchPosition ), MouseMove mousePosition ) ->
            let
                offsetPosition =
                    Point.origin
                        |> Point.add touchPosition
                        |> Point.sub mousePosition
                        |> Point.add draggingGroup.position

                isDraggingSelectedGroup =
                    isSelectedGroupDragged model.groups

                updatePosition group =
                    if
                        group
                            == draggingGroup
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
                model |> withGroupsUpdated updatePosition

        ( _, Nothing, EndSelection selection ) ->
            { model | selection = Nothing }
                |> withGroupsUpdated
                    (\group ->
                        { group | isSelected = isSelected selection model.pieceSize group }
                    )

        _ ->
            model


groupView : Int -> PieceGroup.Model -> ( String, Svg.Svg Msg )
groupView pieceSize group =
    let
        toMsg msg =
            case msg of
                PieceGroup.StartDragging point ->
                    StartDragging group point

                PieceGroup.EndDragging ->
                    EndDragging
    in
        (Lazy.lazy2
            PieceGroup.view
            pieceSize
            group
        )
            |> Svg.map toMsg
            |> (\element -> ( PieceGroup.id group, element ))


groupZIndex : List PieceGroup.Model -> PieceGroup.Model -> Int
groupZIndex groups group =
    let
        isDragged =
            group.dragHandle
                |> Maybe.map (\_ -> True)
                |> Maybe.withDefault False

        addition =
            if isDragged || (group.isSelected && isSelectedGroupDragged groups) then
                (groups |> List.length) + 1
            else
                0
    in
        group.zIndex + addition


isSelectedGroupDragged : List PieceGroup.Model -> Bool
isSelectedGroupDragged groups =
    groups
        |> List.any
            (\group ->
                case ( group.dragHandle, group.isSelected ) of
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


isSelected : Bounds -> Int -> PieceGroup.Model -> Bool
isSelected ( from, to ) pieceSize { pieces, position } =
    let
        ( minSelectionX, minSelectionY ) =
            Point.min from to

        ( maxSelectionX, maxSelectionY ) =
            Point.max from to

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


generateGroups : Int -> Int -> Int -> Seed -> ( List PieceGroup.Model, Seed )
generateGroups sizeX sizeY pieceSize seed =
    let
        ( pieces, nextSeed ) =
            JigsawPuzzle.generate sizeX sizeY seed

        pieceMap =
            pieces
                |> List.map (\((Piece index hooks) as piece) -> ( index, piece ))
                |> Dict.fromList
    in
        ( [ { pieces = pieceMap
            , zIndex = 0
            , position = defaultPosition pieceSize pieceMap
            , dragHandle = Nothing
            , isSelected = False
            }
          ]
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


isCorrectDrop : Int -> Float -> PieceGroup.Model -> Bool
isCorrectDrop pieceSize tolerance group =
    (Point.distance
        group.position
        (defaultPosition pieceSize group.pieces)
    )
        < tolerance


drop : Int -> Float -> PieceGroup.Model -> List PieceGroup.Model -> List PieceGroup.Model
drop pieceSize tolerance targetGroup groups =
    let
        ( mergeableGroups, restGroups ) =
            groups
                |> List.partition
                    (\group ->
                        group
                            == targetGroup
                            || isMergeable pieceSize tolerance group targetGroup
                    )

        mergedGroupPosition =
            mergeableGroups
                |> List.foldl
                    (\{ position } passedPosition ->
                        Point.min position passedPosition
                    )
                    targetGroup.position

        reduceMergeableGroups { position, pieces } passed =
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

        mergedGroup =
            { pieces =
                mergeableGroups
                    |> List.foldl reduceMergeableGroups Dict.empty
            , position = mergedGroupPosition
            , zIndex =
                groups
                    |> List.map (\{ zIndex } -> zIndex + 1)
                    |> List.maximum
                    |> Maybe.withDefault 0
            , isSelected = False
            , dragHandle = Nothing
            }
    in
        (mergedGroup :: restGroups)
            |> List.map
                (\group ->
                    if isCorrectDrop pieceSize tolerance group then
                        { group
                            | position = defaultPosition pieceSize group.pieces
                            , zIndex = 0
                        }
                    else
                        group
                )
            |> List.map
                (\group ->
                    { group
                        | dragHandle = Nothing
                        , isSelected = False
                    }
                )


isMergeable : Int -> Float -> PieceGroup.Model -> PieceGroup.Model -> Bool
isMergeable pieceSize tolerance group anotherGroup =
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
                        Point.distance position p < tolerance

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
        reducePieces piece ( passed, seed0 ) =
            let
                ( x, seed1 ) =
                    Random.step (Random.int (-1 * model.pieceSize) (model.sizeX * model.pieceSize)) seed0

                ( y, seed2 ) =
                    Random.step (Random.int (-1 * model.pieceSize) (model.sizeY * model.pieceSize)) seed1
            in
                ( { pieces = Dict.singleton Point.origin piece
                  , position = ( x, y )
                  , zIndex = 0
                  , dragHandle = Nothing
                  , isSelected = False
                  }
                    :: passed
                , seed2
                )

        ( groups, seed ) =
            model.groups
                |> List.concatMap (\{ pieces } -> Dict.values pieces)
                |> List.foldl reducePieces ( [], initialSeed )
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


withGroupsUpdated : (PieceGroup.Model -> PieceGroup.Model) -> Model -> Model
withGroupsUpdated updateGroup model =
    { model | groups = model.groups |> List.map updateGroup }


getOffset : Point -> Point -> Int -> Point
getOffset screenSize boardSize pieceSize =
    screenSize
        |> Point.sub (boardSize |> Point.scale pieceSize)
        |> Point.divide 2
