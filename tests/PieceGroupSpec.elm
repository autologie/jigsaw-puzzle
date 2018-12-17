module PieceGroupSpec exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import PieceGroup
import Dict
import Point
import JigsawPuzzle exposing (Piece(..), Hook(..), Hooks)


hooks : Hooks
hooks =
    { north = Positive 0 0
    , east = Positive 0 0
    , south = Positive 0 0
    , west = Positive 0 0
    }


group : PieceGroup.Model
group =
    { pieces =
        Dict.fromList
            [ ( ( 0, 0 ), Piece ( 0, 0 ) hooks )
            ]
    , position = Point.origin
    , zIndex = 0
    , dragHandle = Nothing
    , isSelected = False
    }


suite : Test
suite =
    describe "PieceGroup"
        [ describe "isMergeable"
            [ test "returns False if two groups are the same." <|
                \_ ->
                    PieceGroup.isMergeable
                        100
                        2
                        group
                        group
                        |> Expect.equal False
            , test "returns True if two groups contain mutually adjacent pieces" <|
                \_ ->
                    PieceGroup.isMergeable
                        100
                        2
                        { group
                            | position = ( 100, 0 )
                            , pieces =
                                Dict.fromList
                                    [ ( ( 0, 0 ), Piece ( 1, 0 ) hooks )
                                    ]
                        }
                        group
                        |> Expect.equal True
            , test "only the relative positions of the two groups matter" <|
                \_ ->
                    PieceGroup.isMergeable
                        100
                        2
                        { group
                            | position = ( 130, -70 )
                            , pieces =
                                Dict.fromList
                                    [ ( ( 0, 0 ), Piece ( 1, 0 ) hooks )
                                    ]
                        }
                        { group | position = ( 30, -70 ) }
                        |> Expect.equal True
            , test "returns False if two groups don't contain mutually adjacent pieces" <|
                \_ ->
                    PieceGroup.isMergeable
                        100
                        2
                        { group
                            | position = Point.origin
                            , pieces =
                                Dict.fromList
                                    [ ( ( 2, 0 ), Piece ( 2, 0 ) hooks )
                                    , ( ( 1, 1 ), Piece ( 1, 1 ) hooks )
                                    , ( ( 0, 2 ), Piece ( 0, 2 ) hooks )
                                    ]
                        }
                        group
                        |> Expect.equal False
            , test "returns True/False according to the tolerance and the amount of gap" <|
                \_ ->
                    [ ( 2, ( 1, 0 ) )
                    , ( 2, ( 2, 0 ) )
                    , ( 2, ( 3, 0 ) )
                    , ( 2, ( 0, 1 ) )
                    , ( 2, ( 0, 2 ) )
                    , ( 2, ( 0, 3 ) )
                    , ( 2, ( 0, -1 ) )
                    , ( 2, ( 0, -2 ) )
                    , ( 2, ( 0, -3 ) )
                    , ( 50, ( 30, 39 ) )
                    , ( 50, ( 30, 40 ) )
                    , ( 50, ( 30, 41 ) )
                    ]
                        |> List.map
                            (\( tolerance, gap ) ->
                                PieceGroup.isMergeable
                                    100
                                    tolerance
                                    { group
                                        | position = Point.add ( 100, 0 ) gap
                                        , pieces =
                                            Dict.fromList
                                                [ ( ( 0, 0 ), Piece ( 1, 0 ) hooks )
                                                ]
                                    }
                                    group
                            )
                        |> Expect.equal
                            [ True
                            , False
                            , False
                            , True
                            , False
                            , False
                            , True
                            , False
                            , False
                            , True
                            , False
                            , False
                            ]
            ]
        ]
