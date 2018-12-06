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
            (( "board"
             , rect
                [ Svg.Attributes.style "fill: #eee;"
                , width (String.fromInt (sizeX * pieceSize))
                , height (String.fromInt (sizeY * pieceSize))
                ]
                []
             )
                :: (groups
                        |> List.sortBy (\{ zIndex } -> zIndex)
                        |> List.map (groupView pieceSize)
                   )
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
            updateOnStartDragging targetGroup touchPosition model

        ( _, Nothing, StartSelection touchPosition ) ->
            updateOnStartSelection touchPosition model

        ( _, Just ( draggingGroup, _ ), EndDragging ) ->
            updateOnEndDragging draggingGroup model

        ( Just ( position, _ ), Nothing, MouseMove mousePosition ) ->
            updateOnSelectionChange ( position, mousePosition ) model

        ( _, Just ( draggingGroup, touchPosition ), MouseMove mousePosition ) ->
            updateOnDrag draggingGroup touchPosition mousePosition model

        ( _, Nothing, EndSelection selection ) ->
            updateOnEndSelection selection model

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


isDragging : List PieceGroup.Model -> PieceGroup.Model -> Bool
isDragging groups group =
    let
        isSelectedAndDragged { dragHandle, isSelected } =
            case ( dragHandle, isSelected ) of
                ( Just _, True ) ->
                    True

                _ ->
                    False
    in
        group.dragHandle
            |> Maybe.map (\_ -> True)
            |> Maybe.withDefault (group.isSelected && (groups |> List.any isSelectedAndDragged))


decodeMouseEvent : Decode.Decoder Point
decodeMouseEvent =
    Decode.map2 (\eventX eventY -> ( eventX, eventY ))
        (Decode.at [ "offsetX" ] Decode.int)
        (Decode.at [ "offsetY" ] Decode.int)


isInSelection : Bounds -> Int -> PieceGroup.Model -> Bool
isInSelection ( from, to ) pieceSize { pieces, position } =
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

        group =
            { pieces = pieceMap
            , zIndex = 0
            , position = Point.origin
            , dragHandle = Nothing
            , isSelected = False
            }
    in
        ( [ { group | position = PieceGroup.defaultPosition pieceSize group }
          ]
        , nextSeed
        )


updateOnStartSelection : Point -> Model -> Model
updateOnStartSelection touchPosition model =
    let
        selectPosition =
            touchPosition |> Point.sub model.offset
    in
        { model
            | selection = Just ( selectPosition, selectPosition )
        }


updateOnStartDragging : PieceGroup.Model -> Point -> Model -> Model
updateOnStartDragging targetGroup touchPosition model =
    let
        maxZIndex =
            model.groups
                |> List.map (\{ zIndex } -> zIndex)
                |> List.maximum
                |> Maybe.withDefault targetGroup.zIndex

        assignHandle group =
            { group
                | dragHandle =
                    if PieceGroup.isSame group targetGroup then
                        Just touchPosition
                    else
                        Nothing
            }

        groupsWithHandle =
            model.groups |> List.map assignHandle

        updatedGroups =
            groupsWithHandle
                |> List.map
                    (\group ->
                        if isDragging groupsWithHandle group then
                            { group | zIndex = group.zIndex + maxZIndex + 1 }
                        else
                            group
                    )
    in
        { model | groups = updatedGroups }


updateOnSelectionChange : Bounds -> Model -> Model
updateOnSelectionChange ( position, mousePosition ) model =
    let
        selection =
            ( position
            , mousePosition |> Point.sub model.offset
            )
    in
        { model | selection = Just selection }
            |> withGroupsUpdated
                (\group ->
                    { group | isSelected = isInSelection selection model.pieceSize group }
                )


updateOnDrag : PieceGroup.Model -> Point -> Point -> Model -> Model
updateOnDrag draggingGroup initialTouchPosition mousePosition model =
    let
        offsetPosition =
            Point.origin
                |> Point.add initialTouchPosition
                |> Point.sub mousePosition
                |> Point.add draggingGroup.position

        updatePosition group =
            if isDragging model.groups group then
                { group | position = group.position |> Point.sub offsetPosition }
            else
                group
    in
        model |> withGroupsUpdated updatePosition


updateOnEndSelection : Bounds -> Model -> Model
updateOnEndSelection selection model =
    { model | selection = Nothing }
        |> withGroupsUpdated
            (\group ->
                { group | isSelected = isInSelection selection model.pieceSize group }
            )


updateOnEndDragging : PieceGroup.Model -> Model -> Model
updateOnEndDragging targetGroup ({ pieceSize, tolerance, groups } as model) =
    let
        minZIndex =
            model.groups
                |> List.map (\{ zIndex } -> zIndex)
                |> List.minimum
                |> Maybe.withDefault targetGroup.zIndex

        ( mergeableGroups, restGroups ) =
            groups
                |> List.partition
                    (\group ->
                        group
                            /= targetGroup
                            && isMergeable pieceSize tolerance group targetGroup
                    )

        mergedGroup =
            PieceGroup.merge pieceSize targetGroup mergeableGroups

        updatedGroups =
            (mergedGroup :: (restGroups |> List.filter (\g -> g /= targetGroup)))
                |> List.map
                    (\group ->
                        if isCorrectDrop pieceSize tolerance group then
                            { group
                                | position = PieceGroup.defaultPosition pieceSize group
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
                            , zIndex = group.zIndex - minZIndex
                        }
                    )
    in
        { model | groups = updatedGroups }


isCorrectDrop : Int -> Float -> PieceGroup.Model -> Bool
isCorrectDrop pieceSize tolerance group =
    (Point.distance
        group.position
        (PieceGroup.defaultPosition pieceSize group)
    )
        < tolerance


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
                            (List.member diffOfIndex [ ( 0, 1 ), ( 0, -1 ), ( 1, 0 ), ( -1, 0 ) ])
                                && (diffOfIndex
                                        |> Point.scale pieceSize
                                        |> Point.add anotherPosition
                                        |> isNear
                                   )
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
