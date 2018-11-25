module Model exposing (..)

import Random


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


type alias PieceGroup =
    { pieces : List Piece
    , position : ( Int, Int )
    , isSettled : Bool
    }


type alias Model =
    { sizeX : Int
    , sizeY : Int
    , pieceSize : Int
    , groups : List PieceGroup
    , dragging : Maybe ( Piece, ( Int, Int ) )
    , seed : Random.Seed
    }


type Msg
    = StartDragging Piece ( Int, Int )
    | EndDragging
    | MouseMove ( Int, Int )
    | Scatter
    | Reset
