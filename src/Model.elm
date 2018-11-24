module Model exposing (..)


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


type alias Model =
    { sizeX : Int
    , sizeY : Int
    , pieceSize : Int
    , pieces : List Piece
    }
