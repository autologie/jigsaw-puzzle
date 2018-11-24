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
    let
        sizeX =
            15

        sizeY =
            10

        pieceSize =
            50

        ( pieces, seed ) =
            generatePieces sizeX sizeY pieceSize (Random.initialSeed 0)
    in
        { sizeX = sizeX
        , sizeY = sizeY
        , pieceSize = pieceSize
        , pieces = pieces
        , dragging = Nothing
        , seed = seed
        }


generatePieces sizeX sizeY pieceSize seed =
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

        ( generatedPieces, lastSeed ) =
            plain
                |> List.foldl
                    (\(Piece position hooks) ( pieces, mySeed ) ->
                        let
                            ( generatedHooks, updatedSeed ) =
                                generateHooks mySeed
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
                    ( plain, seed )
    in
        ( generatedPieces
            |> List.map
                (\piece ->
                    { piece = piece
                    , position = desiredPosition pieceSize piece
                    }
                )
        , lastSeed
        )


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
        maxDiviation =
            0.06

        withDeviation ( hook, seed ) =
            let
                ( deviation, updatedSeed ) =
                    Random.step (Random.float -maxDiviation maxDiviation) seed
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
                (\(Piece position hooks) ->
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
                        Piece position hooks
                )


negate hook =
    case hook of
        None ->
            None

        Positive deviation ->
            Negative -deviation

        Negative deviation ->
            Positive -deviation


update msg model =
    case msg of
        StartDragging targetPiece touchPosition ->
            { model
                | dragging = Just ( targetPiece, touchPosition )
                , pieces =
                    model.pieces
                        |> List.sortBy
                            (\piece ->
                                if piece.piece == targetPiece then
                                    1
                                else
                                    0
                            )
            }

        EndDragging ->
            case model.dragging of
                Just ( targetPiece, _ ) ->
                    { model
                        | dragging = Nothing
                        , pieces =
                            model.pieces
                                |> List.map
                                    (\piece ->
                                        if piece.piece == targetPiece then
                                            if isCorrectDrop model.pieceSize piece then
                                                { piece
                                                    | piece = piece.piece
                                                    , position = desiredPosition model.pieceSize piece.piece
                                                }
                                            else
                                                case ( piece.position, piece.piece ) of
                                                    ( ( pX, pY ), Piece { x, y } _ ) ->
                                                        let
                                                            nextX =
                                                                if x == 0 && pX < 10 then
                                                                    0
                                                                else if x == model.sizeX - 1 && pX > (model.sizeX - 1) * model.pieceSize - 10 then
                                                                    (model.sizeX - 1) * model.pieceSize
                                                                else
                                                                    pX

                                                            nextY =
                                                                if y == 0 && pY < 10 then
                                                                    0
                                                                else if y == model.sizeY - 1 && pY > (model.sizeY - 1) * model.pieceSize - 10 then
                                                                    (model.sizeY - 1) * model.pieceSize
                                                                else
                                                                    pY
                                                        in
                                                            { piece | position = ( nextX, nextY ) }
                                        else
                                            piece
                                    )
                    }

                Nothing ->
                    model

        MouseMove ( x, y ) ->
            { model
                | pieces =
                    case model.dragging of
                        Just ( draggingPiece, ( touchX, touchY ) ) ->
                            model.pieces
                                |> List.map
                                    (\piece ->
                                        if piece.piece == draggingPiece then
                                            { position = ( x - touchX, y - touchY )
                                            , piece = piece.piece
                                            }
                                        else
                                            piece
                                    )

                        Nothing ->
                            model.pieces
            }

        Reset ->
            let
                ( pieces, seed ) =
                    generatePieces
                        model.sizeX
                        model.sizeY
                        model.pieceSize
                        model.seed
            in
                { model
                    | dragging = Nothing
                    , pieces = pieces
                    , seed = seed
                }

        Scatter ->
            let
                ( pieces, seed ) =
                    model.pieces
                        |> List.foldl
                            (\piece ( passed, seed0 ) ->
                                let
                                    ( x, seed1 ) =
                                        Random.step (Random.int 0 ((model.sizeX - 1) * model.pieceSize)) seed0

                                    ( y, seed2 ) =
                                        Random.step (Random.int 0 ((model.sizeY - 1) * model.pieceSize)) seed1
                                in
                                    ( List.concat
                                        [ passed
                                        , [ { piece | position = ( x, y ) } ]
                                        ]
                                    , seed2
                                    )
                            )
                            ( [], model.seed )
            in
                { model
                    | pieces = pieces
                    , seed = seed
                }


desiredPosition pieceSize (Piece { x, y } _) =
    ( x * pieceSize, y * pieceSize )


isCorrectDrop pieceSize { piece, position } =
    let
        ( desiredX, desiredY ) =
            desiredPosition pieceSize piece

        distance =
            case position of
                ( actualX, actualY ) ->
                    sqrt (toFloat (desiredX - actualX) ^ 2 + toFloat (desiredY - actualY) ^ 2)
    in
        distance < 10
