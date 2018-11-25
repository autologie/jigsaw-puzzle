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
    = Positive Float
    | Negative Float
    | None


type alias PieceScattering =
    { piece : Piece
    , position : ( Int, Int )
    , isSettled : Bool
    }


type alias Model =
    { sizeX : Int
    , sizeY : Int
    , pieceSize : Int
    , pieces : List PieceScattering
    , dragging : Maybe ( Piece, ( Int, Int ) )
    , seed : Random.Seed
    }


type Msg
    = StartDragging Piece ( Int, Int )
    | EndDragging
    | MouseMove ( Int, Int )
    | Scatter
    | Reset
