module View exposing (view)

import Json.Decode as Decode exposing (Decoder)
import Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Model exposing (..)


view { sizeX, sizeY, pieceSize, pieces, dragging } =
    Html.div []
        [ svg
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
            , Svg.Attributes.style "background: #eee; display: block;"
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
        , Html.button [ onClick Scatter ] [ Html.text "Scatter" ]
        , Html.button [ onClick Reset ] [ Html.text "New Puzzle" ]
        ]


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


type PathElement
    = M Point
    | C Point Point Point
    | L Point


type alias Point =
    ( Float, Float )


piecePath pieceSize (Piece position hooks) =
    List.concat
        [ side hooks.north True
        , side hooks.east False |> List.map (translate (\( x, y ) -> ( 1 - y, x )))
        , side hooks.south False |> List.map (translate (\( x, y ) -> ( 1 - x, 1 - y )))
        , side hooks.west False |> List.map (translate (\( x, y ) -> ( y, 1 - x )))
        ]
        |> List.map (translate (\( x, y ) -> ( x * (toFloat pieceSize), y * (toFloat pieceSize) )))
        |> List.map
            (\pathElement ->
                case pathElement of
                    M ( x, y ) ->
                        "M " ++ ([ x, y ] |> List.map String.fromFloat |> String.join " ")

                    L ( x, y ) ->
                        "L " ++ ([ x, y ] |> List.map String.fromFloat |> String.join " ")

                    C ( x1, y1 ) ( x2, y2 ) ( x, y ) ->
                        "C "
                            ++ ([ [ x1, y1 ], [ x2, y2 ], [ x, y ] ]
                                    |> List.map (List.map String.fromFloat >> String.join ",")
                                    |> String.join " "
                               )
            )
        |> String.join " "
        |> (\path -> path ++ " z")


translate : (Point -> Point) -> PathElement -> PathElement
translate fn pathElement =
    case pathElement of
        M p ->
            M (fn p)

        C p1 p2 p ->
            C (fn p1) (fn p2) (fn p)

        L p ->
            L (fn p)


side : Hook -> Bool -> List PathElement
side hook isInitial =
    let
        initial =
            if isInitial then
                M ( 0, 0 )
            else
                L ( 0, 0 )

        height =
            0.3

        sink =
            0.03
    in
        case hook of
            None ->
                [ initial, L ( 1, 0 ) ]

            Positive deviation ->
                [ initial
                , C ( (0.35 + deviation) / 2, 0 ) ( (0.35 + deviation) / 2, sink ) ( 0.35 + deviation, sink )
                , C ( 0.5 + deviation, sink ) ( 0.2 + deviation, -height ) ( 0.5 + deviation, -height )
                , C ( 0.8 + deviation, -height ) ( 0.5 + deviation, sink ) ( 0.65 + deviation, sink )
                , C ( (1.65 + deviation) / 2, -sink ) ( (1.65 + deviation) / 2, 0 ) ( 1, 0 )
                ]

            Negative deviation ->
                [ initial
                , C ( (0.35 + deviation) / 2, 0 ) ( (0.35 + deviation) / 2, -sink ) ( 0.35 + deviation, -sink )
                , C ( 0.5 + deviation, -sink ) ( 0.2 + deviation, height ) ( 0.5 + deviation, height )
                , C ( 0.8 + deviation, height ) ( 0.5 + deviation, -sink ) ( 0.65 + deviation, -sink )
                , C ( (1.65 + deviation) / 2, -sink ) ( (1.65 + deviation) / 2, 0 ) ( 1, 0 )
                ]
