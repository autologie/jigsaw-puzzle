module Main exposing (main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Random exposing (Seed)
import Html exposing (Html)
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
    , gamePlayModel : Board.Model
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
        ( gamePlayModel, seed ) =
            Board.initialModel
                windowSize
                (Random.initialSeed 0)
    in
        { windowSize = windowSize
        , seed = seed
        , gamePlayModel = gamePlayModel
        }


view : Model -> Browser.Document Msg
view model =
    { title = ""
    , body =
        [ Html.div []
            [ Board.view model.gamePlayModel |> Html.map BoardMsg
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
                | gamePlayModel = Board.update gamePlayMsg model.gamePlayModel
            }

        Reset ->
            let
                ( gamePlayModel, seed ) =
                    Board.initialModel
                        model.windowSize
                        model.seed
            in
                { model
                    | seed = seed
                    , gamePlayModel = gamePlayModel
                }

        Scatter ->
            let
                ( gamePlayModel, seed ) =
                    model.gamePlayModel |> Board.scatterPieces model.seed
            in
                { model
                    | seed = seed
                    , gamePlayModel = gamePlayModel
                }

        ResizeWindow size ->
            let
                gamePlayModel =
                    model.gamePlayModel |> Board.withScreenSize size
            in
                { model
                    | gamePlayModel = gamePlayModel
                }
