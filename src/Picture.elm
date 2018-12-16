module Picture exposing (Picture, sizeX, sizeY, isVoidAt, fromString)

import Point exposing (Point)
import Array exposing (Array)


type alias Picture =
    Array (Array Bool)


sizeX : Picture -> Int
sizeX picture =
    picture
        |> Array.map Array.length
        |> Array.toList
        |> List.maximum
        |> Maybe.withDefault 0


sizeY : Picture -> Int
sizeY picture =
    Array.length picture


isVoidAt : Point -> Picture -> Bool
isVoidAt ( x, y ) picture =
    (x < 0)
        || (y < 0)
        || (picture
                |> Array.get y
                |> Maybe.andThen
                    (\line ->
                        line
                            |> Array.get x
                            |> Maybe.map not
                    )
                |> Maybe.withDefault True
           )


fromString : Char -> String -> Picture
fromString filledChar source =
    source
        |> String.lines
        |> Array.fromList
        |> Array.map
            (String.trim
                >> String.toList
                >> Array.fromList
                >> (Array.map ((==) filledChar))
            )
