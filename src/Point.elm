module Point
    exposing
        ( Point
        , origin
        , add
        , sub
        , scale
        , divide
        , divideRound
        , min
        , max
        , distance
        , toString
        )


type alias Point =
    ( Int, Int )


origin : Point
origin =
    ( 0, 0 )


add : Point -> Point -> Point
add ( p1X, p1Y ) ( p2X, p2Y ) =
    ( p1X + p2X, p1Y + p2Y )


sub : Point -> Point -> Point
sub ( p1X, p1Y ) ( p2X, p2Y ) =
    ( p2X - p1X, p2Y - p1Y )


scale : Int -> Point -> Point
scale factor ( pX, pY ) =
    ( pX * factor, pY * factor )


divide : Int -> Point -> Point
divide factor ( pX, pY ) =
    ( pX // factor, pY // factor )


divideRound : Int -> Point -> Point
divideRound factor ( pX, pY ) =
    ( toFloat pX / toFloat factor |> round
    , toFloat pY / toFloat factor |> round
    )


min : Point -> Point -> Point
min ( p1X, p1Y ) ( p2X, p2Y ) =
    ( Basics.min p2X p1X, Basics.min p2Y p1Y )


max : Point -> Point -> Point
max ( p1X, p1Y ) ( p2X, p2Y ) =
    ( Basics.max p2X p1X, Basics.max p2Y p1Y )


distance : Point -> Point -> Float
distance ( p1X, p1Y ) ( p2X, p2Y ) =
    sqrt (toFloat (p1X - p2X) ^ 2 + toFloat (p1Y - p2Y) ^ 2)


toString : Point -> String
toString ( x, y ) =
    "("
        ++ String.fromInt x
        ++ ","
        ++ String.fromInt y
        ++ ")"
