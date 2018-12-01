module Model exposing (..)

import Random
import Dict exposing (Dict)


type Piece
    = Piece Point Hooks


type alias Point =
    ( Int, Int )


type alias Hooks =
    { north : Hook
    , east : Hook
    , south : Hook
    , west : Hook
    }


type Hook
    = Positive Float Float
    | Negative Float Float
    | None


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
