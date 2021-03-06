module PieceGroup
    exposing
        ( Model
        , Msg(..)
        , view
        , id
        , isSame
        , defaultPosition
        , merge
        , isMergeable
        )

import Svg exposing (..)
import Set
import Dict exposing (Dict)
import Html
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Svg.Lazy as Lazy
import Point exposing (Point)
import JigsawPuzzle exposing (Piece(..), Hook(..))
import Piece


type Msg
    = StartDragging Point
    | EndDragging


type alias Model =
    { pieces : Dict Point Piece
    , position : Point
    , zIndex : Int
    , dragHandle : Maybe Point
    , isSelected : Bool
    }


view : Int -> Model -> Svg Msg
view pieceSize group =
    let
        toMsg msg =
            case msg of
                Piece.StartDragging point ->
                    StartDragging (point |> Point.sub group.position)

                Piece.EndDragging ->
                    EndDragging
    in
        g
            [ transform ("translate" ++ Point.toString group.position) ]
            (group.pieces
                |> Dict.map (Lazy.lazy4 Piece.view pieceSize group.isSelected)
                |> Dict.values
                |> List.map (Html.map toMsg)
            )


id : Model -> String
id group =
    group.pieces
        |> Dict.values
        |> List.map (\(Piece point _) -> Point.toString point)
        |> String.join ","


isSame : Model -> Model -> Bool
isSame one another =
    let
        pointSet group =
            group.pieces
                |> Dict.values
                |> List.map (\(Piece point _) -> point)
                |> Set.fromList
    in
        pointSet one == pointSet another


defaultPosition : Int -> Model -> Point
defaultPosition pieceSize { pieces } =
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


merge : Int -> Model -> List Model -> Model
merge pieceSize group otherGroups =
    let
        mergedGroupPosition =
            otherGroups
                |> List.foldl
                    (\{ position } passedPosition ->
                        Point.min position passedPosition
                    )
                    group.position

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
    in
        { pieces =
            (group :: otherGroups)
                |> List.foldl reduceMergeableGroups Dict.empty
        , position = mergedGroupPosition
        , zIndex =
            otherGroups
                |> List.map (\{ zIndex } -> zIndex + 1)
                |> List.maximum
                |> Maybe.withDefault group.zIndex
        , isSelected = False
        , dragHandle = Nothing
        }


isMergeable : Int -> Float -> Model -> Model -> Bool
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
                            List.member diffOfIndex [ ( 0, 1 ), ( 0, -1 ), ( 1, 0 ), ( -1, 0 ) ]
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
