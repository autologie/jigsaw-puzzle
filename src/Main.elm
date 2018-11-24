module Main exposing (..)

import Browser
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Random


main =
    Browser.sandbox { init = initialModel, update = update, view = view }


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
    = Positive
    | Negative
    | None


initialModel =
    { sizeX = 8
    , sizeY = 6
    , pieceSize = 100
    , pieces =
        generatePieces 8 6
    }


generatePieces sizeX sizeY =
    let
        plain =
            plainPieces sizeX sizeY

        trimX position hooks =
            if position.x == 0 then
                { hooks | west = None }
            else if position.x == sizeX - 1 then
                { hooks | east = None }
            else
                hooks

        trimY position hooks =
            if position.y == 0 then
                { hooks | north = None }
            else if position.y == sizeY - 1 then
                { hooks | south = None }
            else
                hooks

        ( generatedPieces, _ ) =
            plain
                |> List.foldl
                    (\(Piece position hooks) ( pieces, seed ) ->
                        let
                            ( generatedHooks, updatedSeed ) =
                                generateHooks seed
                        in
                            ( withHooksAssigned position
                                (generatedHooks
                                    |> trimX position
                                    |> trimY position
                                )
                                pieces
                            , updatedSeed
                            )
                    )
                    ( plain, Random.initialSeed 0 )
    in
        generatedPieces


plainPieces sizeX sizeY =
    List.range 0 (sizeX - 1)
        |> List.concatMap
            (\x ->
                List.range 0 (sizeY - 1)
                    |> List.map
                        (\y ->
                            Piece { x = x, y = y }
                                noHooks
                        )
            )


noHooks =
    { north = None
    , east = None
    , south = None
    , west = None
    }


generateHooks seed0 =
    let
        generator =
            Random.uniform Positive [ Negative ]

        ( north, seed1 ) =
            Random.step generator seed0

        ( east, seed2 ) =
            Random.step generator seed1

        ( south, seed3 ) =
            Random.step generator seed2

        ( west, seed4 ) =
            Random.step generator seed3
    in
        ( { north = north
          , east = east
          , south = south
          , west = west
          }
        , seed4
        )


withHooksAssigned { x, y } { north, east, south, west } pieces =
    let
        isRelativeTo position offsetX offsetY =
            x == position.x + offsetX && y == position.y + offsetY

        isJust position =
            isRelativeTo position 0 0

        isWestOf position =
            isRelativeTo position -1 0

        isEastOf position =
            isRelativeTo position 1 0

        isSouthOf position =
            isRelativeTo position 0 1

        isNorthOf position =
            isRelativeTo position 0 -1
    in
        pieces
            |> List.map
                (\piece ->
                    case piece of
                        Piece position hooks ->
                            if isJust position then
                                Piece position
                                    { north = north
                                    , east = east
                                    , south = south
                                    , west = west
                                    }
                            else if isWestOf position then
                                Piece position { hooks | west = negate east }
                            else if isEastOf position then
                                Piece position { hooks | east = negate west }
                            else if isNorthOf position then
                                Piece position { hooks | north = negate south }
                            else if isSouthOf position then
                                Piece position { hooks | south = negate north }
                            else
                                piece
                )


negate hook =
    case hook of
        None ->
            None

        Positive ->
            Negative

        Negative ->
            Positive


update msg model =
    model


view { sizeX, sizeY, pieceSize, pieces } =
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

        Positive ->
            "+"

        Negative ->
            "-"


pieceView pieceSize (Piece position hooks) =
    let
        myX =
            position.x * pieceSize

        myY =
            position.y * pieceSize
    in
        g
            [ transform ("translate(" ++ (String.fromInt myX) ++ "," ++ (String.fromInt myY) ++ ")") ]
            [ Svg.path
                [ d (piecePath pieceSize (Piece position hooks))
                , stroke "red"
                , fill "white"
                ]
                []
            , text_
                [ x "0"
                , y "20"
                , fill "black"
                ]
                [ text (describePiece (Piece position hooks)) ]
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


rotate90 ( x, y ) =
    ( y, -x )


translate diffX diffY ( x, y ) =
    ( x + diffX, y + diffY )


side hook =
    case hook of
        None ->
            [ ( 0, 0 ), ( 1, 0 ) ]

        Positive ->
            [ ( 0, 0 ), ( 0.3, 0 ), ( 0.5, -0.3 ), ( 0.7, 0 ), ( 1, 0 ) ]

        Negative ->
            [ ( 0, 0 ), ( 0.3, 0 ), ( 0.5, 0.3 ), ( 0.7, 0 ), ( 1, 0 ) ]
