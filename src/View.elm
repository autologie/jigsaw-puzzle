module View exposing (view)

import Json.Decode as Decode exposing (Decoder)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Model exposing (..)


view { sizeX, sizeY, pieceSize, pieces, dragging } =
    svg
        [ width (String.fromInt (sizeX * pieceSize))
        , height (String.fromInt (sizeY * pieceSize))
        , viewBox
            ([ 0
             , 0
             , sizeX * pieceSize
             , sizeY * pieceSize
             ]
                |> List.map String.fromInt
                |> String.join " "
            )
        , Svg.Attributes.style "background: #eee"
        , on "mousemove"
            (Decode.map
                MouseMove
                (Decode.map2 (\x y -> ( x, y ))
                    (Decode.at [ "offsetX" ] Decode.int)
                    (Decode.at [ "offsetY" ] Decode.int)
                )
            )
        ]
        (pieces |> List.map (pieceView pieceSize))


describePiece (Piece { x, y } { north, east, south, west }) =
    "("
        ++ (String.fromInt x)
        ++ ", "
        ++ (String.fromInt y)
        ++ ")\n"
        ++ ([ north, east, south, west ] |> List.map describeHook |> String.join ", ")


describeHook hook =
    case hook of
        None ->
            "*"

        Positive _ ->
            "+"

        Negative _ ->
            "-"


pieceView pieceSize piece =
    case piece.position of
        ( myX, myY ) ->
            g
                [ transform ("translate(" ++ (String.fromInt myX) ++ "," ++ (String.fromInt myY) ++ ")") ]
                [ Svg.path
                    [ d (piecePath pieceSize piece.piece)
                    , stroke "red"
                    , fill "white"
                    , onMouseDown (StartDragging piece.piece)
                    , onMouseUp EndDragging
                    ]
                    []
                , text_
                    [ x "0"
                    , y "20"
                    , fill "black"
                    ]
                    [{- text (describePiece piece) -}]
                ]


piecePath pieceSize (Piece position hooks) =
    let
        coords =
            List.concat
                [ side hooks.north
                , side hooks.east |> List.map ((\( x, y ) -> ( -y, x )) >> translate 1 0)
                , side hooks.south |> List.map ((\( x, y ) -> ( -x, -y )) >> translate 1 1)
                , side hooks.west |> List.map ((\( x, y ) -> ( y, -x )) >> translate 0 1)
                ]
                |> List.map (\( x, y ) -> ( x * (toFloat pieceSize), y * (toFloat pieceSize) ))
                |> List.map (\( x, y ) -> (String.fromFloat x) ++ " " ++ (String.fromFloat y))
    in
        case coords of
            head :: tail ->
                "M " ++ head ++ (tail |> List.map (\v -> "L " ++ v) |> String.join " ") ++ " z"

            _ ->
                ""


translate diffX diffY ( x, y ) =
    ( x + diffX, y + diffY )


side hook =
    case hook of
        None ->
            [ ( 0, 0 ), ( 1, 0 ) ]

        Positive deviation ->
            [ ( 0, 0 )
            , ( 0.3 + deviation, 0 )
            , ( 0.5 + deviation, -0.3 )
            , ( 0.7 + deviation, 0 )
            , ( 1, 0 )
            ]

        Negative deviation ->
            [ ( 0, 0 )
            , ( 0.3 - deviation, 0 )
            , ( 0.5 - deviation, 0.3 )
            , ( 0.7 - deviation, 0 )
            , ( 1, 0 )
            ]
