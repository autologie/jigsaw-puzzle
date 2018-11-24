module Main exposing (..)

import Browser
import Random exposing (Seed)
import Model exposing (..)
import View exposing (view)


main =
    Browser.sandbox
        { init = initialModel
        , update = update
        , view = view
        }


initialModel : Model
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


generateHooks : Seed -> ( Hooks, Seed )
generateHooks seed0 =
    let
        withDeviation ( hook, seed ) =
            let
                ( deviation, updatedSeed ) =
                    Random.step (Random.float 0 0.1) seed
            in
                case hook of
                    Positive _ ->
                        ( Positive deviation, updatedSeed )

                    Negative _ ->
                        ( Negative deviation, updatedSeed )

                    None ->
                        ( None, updatedSeed )

        generator =
            Random.uniform (Positive 0) [ (Negative 0) ]

        ( north, seed1 ) =
            Random.step generator seed0 |> withDeviation

        ( east, seed2 ) =
            Random.step generator seed1 |> withDeviation

        ( south, seed3 ) =
            Random.step generator seed2 |> withDeviation

        ( west, seed4 ) =
            Random.step generator seed3 |> withDeviation
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

        Positive deviation ->
            Negative deviation

        Negative deviation ->
            Positive deviation


update msg model =
    model
