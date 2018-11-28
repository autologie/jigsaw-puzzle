module Model exposing (..)

import Random
import Dict exposing (Dict)


type Piece
    = Piece Position Hooks


type alias Position =
    { x : Int, y : Int }


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
    { pieces : Dict ( Int, Int ) Piece
    , position : ( Int, Int )
    , isSettled : Bool
    , zIndex : Int
    }


type alias Model =
    { offset : ( Int, Int )
    , sizeX : Int
    , sizeY : Int
    , pieceSize : Int
    , groups : Dict PieceGroupId PieceGroup
    , dragging : Maybe ( PieceGroupId, ( Int, Int ) )
    , seed : Random.Seed
    }


type Msg
    = StartDragging PieceGroupId ( Int, Int )
    | EndDragging
    | MouseMove ( Int, Int )
    | Scatter
    | Reset
    | ResizeWindow ( Int, Int )
