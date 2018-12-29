module Main exposing (main)

import Browser
import Browser.Events
import Random
import Html
import Html.Attributes
import Html.Events exposing (..)
import Board
import Point exposing (Point)
import JigsawPuzzle exposing (Piece(..))


type Msg
    = Scatter
    | Reset
    | ResizeWindow Point
    | BoardMsg Board.Msg


type alias Model =
    { windowSize : Point
    , seed : Random.Seed
    , board : Board.Model
    }


main =
    Browser.document
        { init = \flag -> ( initialModel flag, Cmd.none )
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = view
        , subscriptions = \_ -> Browser.Events.onResize (\x y -> ResizeWindow ( x, y ))
        }


initialModel : ( Int, Int ) -> Model
initialModel windowSize =
    let
        ( board, seed ) =
            Board.initialModel
                windowSize
                (Random.initialSeed 0)
    in
        { windowSize = windowSize
        , seed = seed
        , board = board
        }


view : Model -> Browser.Document Msg
view model =
    { title = ""
    , body =
        [ Html.div []
            [ Board.view model.board |> Html.map BoardMsg
            , Html.div [ Html.Attributes.class "control" ]
                [ Html.button [ onClick Scatter ] [ Html.text "Scatter" ]
                , Html.button [ onClick Reset ] [ Html.text "New Puzzle" ]
                ]
            ]
        ]
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        BoardMsg gamePlayMsg ->
            { model
                | board = Board.update gamePlayMsg model.board
            }

        Reset ->
            let
                ( board, seed ) =
                    Board.initialModel
                        model.windowSize
                        model.seed
            in
                { model
                    | seed = seed
                    , board = board
                }

        Scatter ->
            let
                ( board, seed ) =
                    model.board |> Board.scatterPieces model.seed
            in
                { model
                    | seed = seed
                    , board = board
                }

        ResizeWindow size ->
            let
                board =
                    model.board |> Board.withScreenSize size
            in
                { model
                    | board = board
                }
