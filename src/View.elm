module View exposing (view)

import Json.Decode as Decode exposing (Decoder)
import Html exposing (Html)
import Svg exposing (..)
import Dict
import Browser
import Html.Attributes
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Svg.Lazy as Lazy
import Model exposing (..)


view : Model -> Browser.Document Msg
view { offset, sizeX, sizeY, pieceSize, groups, dragging } =
    { title = ""
    , body =
        [ Html.div []
            [ svg
                [ width "100vw"
                , height "100vh"
                , viewBox "0 0 100vw 100vh"
                , on "mousemove"
                    (Decode.map
                        MouseMove
                        (Decode.map2 (\x y -> ( x, y ))
                            (Decode.at [ "offsetX" ] Decode.int)
                            (Decode.at [ "offsetY" ] Decode.int)
                        )
                    )
                ]
                [ g
                    [ transform
                        ("translate("
                            ++ (String.fromInt (Tuple.first offset))
                            ++ ","
                            ++ (String.fromInt (Tuple.second offset))
                            ++ ")"
                        )
                    ]
                    (List.concat
                        [ [ rect
                                [ Svg.Attributes.style "fill: #eee;"
                                , width (String.fromInt (sizeX * pieceSize))
                                , height (String.fromInt (sizeY * pieceSize))
                                ]
                                []
                          ]
                        , (groups
                            |> Dict.toList
                            |> List.sortBy
                                (\( groupId, group ) ->
                                    dragging
                                        |> Maybe.andThen
                                            (\( id, _ ) ->
                                                if id == groupId then
                                                    Just 1000
                                                else
                                                    Nothing
                                            )
                                        |> Maybe.withDefault group.zIndex
                                )
                            |> List.concatMap (groupViews pieceSize)
                          )
                        ]
                    )
                ]
            , Html.div [ Html.Attributes.class "control" ]
                [ Html.button [ onClick Scatter ] [ Html.text "Scatter" ]
                , Html.button [ onClick Reset ] [ Html.text "New Puzzle" ]
                ]
            ]
        ]
    }


groupViews : Int -> ( PieceGroupId, PieceGroup ) -> List (Svg Msg)
groupViews pieceSize ( groupId, group ) =
    let
        ( gX, gY ) =
            group.position

        exists ( pX, pY ) =
            group.pieces
                |> Dict.values
                |> List.any (\(Piece position _) -> position == { x = pX, y = pY })
    in
        group.pieces
            |> Dict.map
                (\position piece ->
                    Lazy.lazy6
                        pieceView
                        gX
                        gY
                        pieceSize
                        groupId
                        position
                        piece
                )
            |> Dict.values


pieceView :
    Int
    -> Int
    -> Int
    -> PieceGroupId
    -> ( Int, Int )
    -> Piece
    -> Svg Msg
pieceView groupX groupY pieceSize groupId ( x, y ) piece =
    -- TODO: draw connected sides with lighter color
    g
        [ transform
            ("translate("
                ++ (String.fromInt (x * pieceSize + groupX))
                ++ ","
                ++ (String.fromInt (y * pieceSize + groupY))
                ++ ")"
            )
        ]
        [ Svg.path
            [ d (piecePath pieceSize piece)
            , stroke "red"
            , fill "white"
            , on "mousedown"
                (Decode.map
                    (\position -> StartDragging groupId position)
                    (Decode.map2 (\eventX eventY -> ( eventX - groupX, eventY - groupY ))
                        (Decode.at [ "offsetX" ] Decode.int)
                        (Decode.at [ "offsetY" ] Decode.int)
                    )
                )
            , onMouseUp EndDragging
            ]
            []
        , text_
            [ Svg.Attributes.x "0"
            , Svg.Attributes.y "20"
            , fill "black"
            ]
            []
        ]


type PathElement
    = M Point
    | C Point Point Point
    | L Point


type alias Point =
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


translate : (Point -> Point) -> PathElement -> PathElement
translate fn pathElement =
    case pathElement of
        M p ->
            M (fn p)

        C p1 p2 p ->
            C (fn p1) (fn p2) (fn p)

        L p ->
            L (fn p)
