module JigsawPuzzleSpec exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Random exposing (Seed)
import JigsawPuzzle exposing (Piece(..), Hook(..))
import Array
import Piece
import Picture exposing (Picture)
import Dict


seed : Seed
seed =
    Random.initialSeed 0


engages : Hook -> Hook -> Bool
engages aHook anotherHook =
    case ( aHook, anotherHook ) of
        ( Positive _ _, Negative _ _ ) ->
            aHook == JigsawPuzzle.negate anotherHook

        ( Negative _ _, Positive _ _ ) ->
            aHook == JigsawPuzzle.negate anotherHook

        _ ->
            False


suite : Test
suite =
    describe "JigsawPuzzle"
        [ describe "generate"
            [ test "if the picture is empty, returns empty list" <|
                \_ ->
                    JigsawPuzzle.generate Array.empty seed
                        |> Tuple.first
                        |> Expect.equal []
            , test "returns a list of pieces with positions as design" <|
                \_ ->
                    JigsawPuzzle.generate
                        (Picture.fromString '*' """**
                                                   **
                                                   **""")
                        seed
                        |> Tuple.first
                        |> List.map (\(Piece position _) -> position)
                        |> Expect.equal
                            [ ( 0, 0 )
                            , ( 0, 1 )
                            , ( 0, 2 )
                            , ( 1, 0 )
                            , ( 1, 1 )
                            , ( 1, 2 )
                            ]
            , test "returns an updated seed" <|
                \_ ->
                    JigsawPuzzle.generate (Picture.fromString '*' "****") seed
                        |> Tuple.second
                        |> Expect.notEqual seed
            , test "returns a list of pieces with hooks at all of their non-edge sides" <|
                \_ ->
                    JigsawPuzzle.generate
                        (Picture.fromString '*' """***
                                                   ***
                                                   ***""")
                        seed
                        |> Tuple.first
                        |> List.map
                            (\(Piece position { north, east, south, west }) ->
                                ( position
                                , [ None == north
                                  , None == east
                                  , None == south
                                  , None == west
                                  ]
                                )
                            )
                        |> Dict.fromList
                        |> Expect.equal
                            (Dict.fromList
                                [ ( ( 0, 0 ), [ True, False, False, True ] )
                                , ( ( 0, 1 ), [ False, False, False, True ] )
                                , ( ( 0, 2 ), [ False, False, True, True ] )
                                , ( ( 1, 0 ), [ True, False, False, False ] )
                                , ( ( 1, 1 ), [ False, False, False, False ] )
                                , ( ( 1, 2 ), [ False, False, True, False ] )
                                , ( ( 2, 0 ), [ True, True, False, False ] )
                                , ( ( 2, 1 ), [ False, True, False, False ] )
                                , ( ( 2, 2 ), [ False, True, True, False ] )
                                ]
                            )
            , test "adjacent pieces have consistent hook properties" <|
                \_ ->
                    JigsawPuzzle.generate
                        (Picture.fromString '*' """***
                                                   ***
                                                   ***""")
                        seed
                        |> Tuple.first
                        |> List.map (\(Piece position hooks) -> ( position, hooks ))
                        |> Dict.fromList
                        |> (\pieces ->
                                Maybe.map5
                                    (\north east south west center ->
                                        engages north.south center.north
                                            && engages east.west center.east
                                            && engages south.north center.south
                                            && engages west.east center.west
                                    )
                                    (Dict.get ( 1, 0 ) pieces)
                                    (Dict.get ( 2, 1 ) pieces)
                                    (Dict.get ( 1, 2 ) pieces)
                                    (Dict.get ( 0, 1 ) pieces)
                                    (Dict.get ( 1, 1 ) pieces)
                           )
                        |> Maybe.map (Expect.true "should be consistent")
                        |> Maybe.withDefault (Expect.fail "precondition fail")
            , test "may have one or more holes" <|
                \_ ->
                    JigsawPuzzle.generate
                        (Picture.fromString '*' """********
                                                   * ******
                                                   * *** **
                                                   ****  **
                                                   ********""")
                        seed
                        |> Tuple.first
                        |> List.map (\((Piece position _) as piece) -> ( position, piece ))
                        |> Dict.fromList
                        |> Expect.all
                            [ Dict.size >> Expect.equal 35
                            , Dict.get ( 4, 2 )
                                >> (Maybe.map
                                        (\(Piece _ { north, east, south, west }) ->
                                            Expect.all
                                                [ \_ -> Expect.notEqual north None
                                                , \_ -> Expect.equal east None
                                                , \_ -> Expect.equal south None
                                                , \_ -> Expect.notEqual west None
                                                ]
                                                ()
                                        )
                                        >> Maybe.withDefault (Expect.fail "shouldn't be nothing")
                                   )
                            , Dict.get ( 5, 2 ) >> Expect.equal Nothing
                            ]
            ]
        ]
