module JigsawPuzzle exposing (Piece(..), Hooks, Hook(..), generate)

import Random exposing (Seed)
import Point exposing (Point)
import Picture exposing (Picture)


type Piece
    = Piece Point Hooks


type alias Hooks =
    { north : Hook
    , east : Hook
    , south : Hook
    , west : Hook
    }


type Hook
    = Positive Float Float
    | Negative Float Float
    | None


generate : Picture -> Seed -> ( List Piece, Seed )
generate picture seed =
    let
        plain =
            plainPieces picture

        trimX ( x, y ) hooks =
            let
                isWestEnd =
                    Picture.isVoidAt ( x - 1, y ) picture

                isEastEnd =
                    Picture.isVoidAt ( x + 1, y ) picture
            in
                if isWestEnd && isEastEnd then
                    { hooks | west = None, east = None }
                else if isWestEnd then
                    { hooks | west = None }
                else if isEastEnd then
                    { hooks | east = None }
                else
                    hooks

        trimY ( x, y ) hooks =
            let
                isNorthEnd =
                    Picture.isVoidAt ( x, y - 1 ) picture

                isSouthEnd =
                    Picture.isVoidAt ( x, y + 1 ) picture
            in
                if isNorthEnd && isSouthEnd then
                    { hooks | north = None, south = None }
                else if isNorthEnd then
                    { hooks | north = None }
                else if isSouthEnd then
                    { hooks | south = None }
                else
                    hooks

        reducePieces (Piece (( indexX, indexY ) as index) hooks) ( pieces, mySeed ) =
            let
                ( generatedHooks, updatedSeed ) =
                    generateHooks mySeed

                trimmedHooks =
                    generatedHooks
                        |> trimX ( indexX, indexY )
                        |> trimY ( indexX, indexY )
            in
                ( withHooksAssigned
                    index
                    trimmedHooks
                    pieces
                , updatedSeed
                )
    in
        plain |> List.foldl reducePieces ( plain, seed )


plainPieces : Picture -> List Piece
plainPieces picture =
    List.range 0 (Picture.sizeX picture - 1)
        |> List.concatMap
            (\x ->
                List.range 0 (Picture.sizeY picture - 1)
                    |> List.filterMap
                        (\y ->
                            if Picture.isVoidAt ( x, y ) picture then
                                Nothing
                            else
                                Just
                                    (Piece
                                        ( x, y )
                                        { north = None
                                        , east = None
                                        , south = None
                                        , west = None
                                        }
                                    )
                        )
            )


generateHooks : Seed -> ( Hooks, Seed )
generateHooks seed0 =
    let
        maxDeviation =
            0.06

        deviationGenerator =
            Random.float -maxDeviation maxDeviation

        hookTypeGenerator =
            Random.uniform (Positive 0 0) [ (Negative 0 0) ]

        withDeviation ( hook, seed00 ) =
            let
                ( positionDeviation, seed01 ) =
                    Random.step deviationGenerator seed00

                ( sizeDeviation, seed02 ) =
                    Random.step deviationGenerator seed01
            in
                case hook of
                    Positive _ _ ->
                        ( Positive positionDeviation sizeDeviation, seed02 )

                    Negative _ _ ->
                        ( Negative positionDeviation sizeDeviation, seed02 )

                    None ->
                        ( None, seed02 )

        ( north, seed1 ) =
            Random.step hookTypeGenerator seed0 |> withDeviation

        ( east, seed2 ) =
            Random.step hookTypeGenerator seed1 |> withDeviation

        ( south, seed3 ) =
            Random.step hookTypeGenerator seed2 |> withDeviation

        ( west, seed4 ) =
            Random.step hookTypeGenerator seed3 |> withDeviation
    in
        ( { north = north
          , east = east
          , south = south
          , west = west
          }
        , seed4
        )


withHooksAssigned : Point -> Hooks -> List Piece -> List Piece
withHooksAssigned targetIndex { north, east, south, west } pieces =
    let
        assignHooks (Piece index hooks) =
            let
                diffOfIndex =
                    targetIndex |> Point.sub index

                updatedHooks =
                    case Point.toString diffOfIndex of
                        "(0,0)" ->
                            { north = north
                            , east = east
                            , south = south
                            , west = west
                            }

                        "(-1,0)" ->
                            { hooks | west = negate east }

                        "(1,0)" ->
                            { hooks | east = negate west }

                        "(0,-1)" ->
                            { hooks | north = negate south }

                        "(0,1)" ->
                            { hooks | south = negate north }

                        _ ->
                            hooks
            in
                Piece index updatedHooks
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
