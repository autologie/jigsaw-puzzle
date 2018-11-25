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
                    , position = defaultPosition pieceSize piece
                    , isSettled = True
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
        maxDeviation =
            0.06

        withDeviation ( hook, seed00 ) =
            let
                ( positionDeviation, seed01 ) =
                    Random.step (Random.float -maxDeviation maxDeviation) seed00

                ( sizeDeviation, seed02 ) =
                    Random.step (Random.float -maxDeviation maxDeviation) seed01
            in
                case hook of
                    Positive _ _ ->
                        ( Positive positionDeviation sizeDeviation, seed02 )

                    Negative _ _ ->
                        ( Negative positionDeviation sizeDeviation, seed02 )

                    None ->
                        ( None, seed02 )

        generator =
            Random.uniform (Positive 0 0) [ (Negative 0 0) ]

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


withHooksAssigned : Position -> Hooks -> List Piece -> List Piece
withHooksAssigned { x, y } { north, east, south, west } pieces =
    let
        distanceTo position =
            ( x - position.x |> String.fromInt
            , y - position.y |> String.fromInt
            )

        assignHooks =
            \(Piece position hooks) ->
                let
                    updatedHooks =
                        case distanceTo position of
                            ( "0", "0" ) ->
                                { north = north
                                , east = east
                                , south = south
                                , west = west
                                }

                            ( "-1", "0" ) ->
                                { hooks | west = negate east }

                            ( "1", "0" ) ->
                                { hooks | east = negate west }

                            ( "0", "-1" ) ->
                                { hooks | north = negate south }

                            ( "0", "1" ) ->
                                { hooks | south = negate north }

                            _ ->
                                hooks
                in
                    Piece position updatedHooks
    in
        pieces |> List.map assignHooks


negate hook =
    case hook of
        None ->
            None

        Positive positionDeviation sizeDiviation ->
            Negative -positionDeviation sizeDiviation

        Negative positionDeviation sizeDiviation ->
            Positive -positionDeviation sizeDiviation


update msg model =
    case ( model.dragging, msg ) of
        ( Nothing, StartDragging targetPiece touchPosition ) ->
            { model
                | dragging = Just ( targetPiece, touchPosition )
                , pieces =
                    model.pieces
                        |> List.map (\piece -> { piece | isSettled = False })
                        |> List.sortBy (sortOrderOfPiece targetPiece)
            }

        ( Just ( targetPiece, _ ), EndDragging ) ->
            { model
                | dragging = Nothing
                , pieces =
                    model.pieces
                        |> List.map (updatePieceOnDrop model targetPiece)
                        |> List.sortBy (sortOrderOfPiece targetPiece)
            }

        ( Just ( draggingPiece, ( touchX, touchY ) ), MouseMove ( x, y ) ) ->
            let
                updatePosition piece =
                    if piece.piece == draggingPiece then
                        { piece | position = ( x - touchX, y - touchY ) }
                    else
                        piece
            in
                { model | pieces = model.pieces |> List.map updatePosition }

        ( _, Reset ) ->
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

        ( _, Scatter ) ->
            let
                reducePieces piece ( passed, seed0 ) =
                    let
                        ( x, seed1 ) =
                            Random.step (Random.int 0 ((model.sizeX - 1) * model.pieceSize)) seed0

                        ( y, seed2 ) =
                            Random.step (Random.int 0 ((model.sizeY - 1) * model.pieceSize)) seed1
                    in
                        ( List.concat
                            [ passed
                            , [ { piece | position = ( x, y ), isSettled = False } ]
                            ]
                        , seed2
                        )

                ( pieces, seed ) =
                    model.pieces |> List.foldl reducePieces ( [], model.seed )
            in
                { model
                    | pieces = pieces
                    , seed = seed
                }

        _ ->
            model


sortOrderOfPiece targetPiece piece =
    if piece.isSettled then
        -1
    else if piece.piece == targetPiece then
        1
    else
        0


defaultPosition pieceSize (Piece { x, y } _) =
    ( x * pieceSize, y * pieceSize )


isCorrectDrop pieceSize { piece, position } =
    let
        ( defaultX, defaultY ) =
            defaultPosition pieceSize piece

        distance =
            case position of
                ( actualX, actualY ) ->
                    sqrt (toFloat (defaultX - actualX) ^ 2 + toFloat (defaultY - actualY) ^ 2)
    in
        distance < 10


updatePieceOnDrop model targetPiece piece =
    if piece.piece /= targetPiece then
        piece
    else if isCorrectDrop model.pieceSize piece then
        { piece
            | position = defaultPosition model.pieceSize piece.piece
            , isSettled = True
        }
    else
        case ( piece.position, piece.piece ) of
            ( ( pX, pY ), Piece { x, y } _ ) ->
                let
                    withSnapApplied ( min, max ) position =
                        if position < min + 10 then
                            min
                        else if position > max - 10 then
                            max
                        else
                            position
                in
                    { piece
                        | position =
                            ( withSnapApplied ( 0, (model.sizeX - 1) * model.pieceSize ) pX
                            , withSnapApplied ( 0, (model.sizeY - 1) * model.pieceSize ) pY
                            )
                    }
