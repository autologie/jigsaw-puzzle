module PieceGroup exposing (Model, Msg(..), view)

import Json.Decode as Decode exposing (Decoder)
import Random exposing (Seed)
import Svg exposing (..)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
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
            [ transform ("translate" ++ (Point.toString group.position)) ]
            (group.pieces
                |> Dict.map (Lazy.lazy4 Piece.view pieceSize group.isSelected)
                |> Dict.values
                |> List.map (Html.map toMsg)
            )
