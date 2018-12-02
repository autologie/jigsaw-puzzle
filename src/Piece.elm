module Piece exposing (Msg(..), view)

import Json.Decode as Decode exposing (Decoder)
import Random exposing (Seed)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Point exposing (Point)
import JigsawPuzzle exposing (Piece(..), Hook(..))


type Msg
    = StartDragging Point
    | EndDragging


view :
    Point
    -> Int
    -> Point
    -> Bool
    -> Piece
    -> Svg Msg
view ( offsetX, offsetY ) pieceSize ( x, y ) isSelected piece =
    -- TODO: draw connected sides with lighter color
    g
        [ transform
            ("translate("
                ++ (String.fromInt (x * pieceSize + offsetX))
                ++ ","
                ++ (String.fromInt (y * pieceSize + offsetY))
                ++ ")"
            )
        ]
        [ Svg.path
            [ d (piecePath pieceSize piece)
            , stroke "red"
            , fill (fillColor isSelected)
            , on "mousedown"
                (Decode.map
                    (\( eventX, eventY ) ->
                        StartDragging
                            ( eventX - offsetX
                            , eventY - offsetY
                            )
                    )
                    decodeMouseEvent
                )
            , onMouseUp EndDragging
            ]
            []
        ]


fillColor isSelected =
    if isSelected then
        "#fdd"
    else
        "white"


decodeMouseEvent =
    Decode.map2 (\eventX eventY -> ( eventX, eventY ))
        (Decode.at [ "offsetX" ] Decode.int)
        (Decode.at [ "offsetY" ] Decode.int)


type PathElement
    = M PathStop
    | C PathStop PathStop PathStop
    | L PathStop


type alias PathStop =
    ( Float, Float )


piecePath : Int -> Piece -> String
piecePath pieceSize (Piece position hooks) =
    List.concat
        [ sidePath hooks.north True
        , sidePath hooks.east False |> List.map (translate (\( x, y ) -> ( 1 - y, x )))
        , sidePath hooks.south False |> List.map (translate (\( x, y ) -> ( 1 - x, 1 - y )))
        , sidePath hooks.west False |> List.map (translate (\( x, y ) -> ( y, 1 - x )))
        ]
        |> List.map (translate (\( x, y ) -> ( x * (toFloat pieceSize), y * (toFloat pieceSize) )))
        |> List.map (translate (\( x, y ) -> ( toFloat (round x), toFloat (round y) )))
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


sidePath : Hook -> Bool -> List PathElement
sidePath hook isInitial =
    let
        initial =
            if isInitial then
                M ( 0, 0 )
            else
                L ( 0, 0 )

        path d1 d2 =
            let
                height =
                    0.3 - d2

                sink =
                    0.03

                a =
                    0.35 + d2

                c =
                    0.5

                w =
                    0.3
            in
                [ initial
                , C ( (a + d1) / 2, 0 ) ( (a + d1) / 2, sink ) ( a + d1, sink )
                , C ( c + d1, sink ) ( c - w + d1, -height ) ( c + d1, -height )
                , C ( c + w + d1, -height ) ( c + d1, sink ) ( (1 - a) + d1, sink )
                , C ( ((2 - a) + d1) / 2, sink ) ( ((2 - a) + d1) / 2, 0 ) ( 1, 0 )
                ]
    in
        case hook of
            None ->
                [ initial, L ( 1, 0 ) ]

            Positive positionDeviation sizeDiviation ->
                path positionDeviation sizeDiviation

            Negative positionDeviation sizeDiviation ->
                path positionDeviation sizeDiviation
                    |> List.map (translate (\( x, y ) -> ( x, -y )))


translate : (PathStop -> PathStop) -> PathElement -> PathElement
translate fn pathElement =
    case pathElement of
        M p ->
            M (fn p)

        C p1 p2 p ->
            C (fn p1) (fn p2) (fn p)

        L p ->
            L (fn p)
