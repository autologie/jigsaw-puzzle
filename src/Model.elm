module Model exposing (..)

import Random
import Dict exposing (Dict)
import Point exposing (Point)
import JigsawPuzzle exposing (Piece)


type alias PieceGroupId =
    Int


type alias PieceGroup =
    { pieces : Dict Point Piece
    , position : Point
    , isSettled : Bool
    , zIndex : Int
    }


type alias Model =
    { offset : Point
    , sizeX : Int
    , sizeY : Int
    , pieceSize : Int
    , groups : Dict PieceGroupId PieceGroup
    , dragging : Maybe ( PieceGroupId, Point )
    , seed : Random.Seed
    , selection : Maybe ( Point, Point )
    , selectedGroups : List PieceGroupId
    }


type Msg
    = StartDragging PieceGroupId Point
    | EndDragging
    | MouseMove Point
    | Scatter
    | Reset
    | ResizeWindow Point
    | StartSelection Point
    | EndSelection
