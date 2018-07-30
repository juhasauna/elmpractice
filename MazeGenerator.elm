module Main exposing (..)

import Array
import Html exposing (..)
import Random
import Svg exposing (..)
import Svg.Attributes as SvgA exposing (..)
import Time exposing (Time)


dim =
    10


cellCount =
    dim ^ 2


cellSize =
    20


gridSize =
    toString (dim * cellSize + 1)


strCS =
    toString cellSize


initWalls =
    Walls True True True True


type alias Walls =
    { top : Bool, right : Bool, bottom : Bool, left : Bool }


type alias Cell =
    { x : String, y : String, visited : Bool, walls : Walls }


type alias Model =
    { grid : Array.Array Cell
    , current : Cell
    , stack : List Cell
    }


init =
    ( Model initGrid (Cell "0" "0" False initWalls) [], Cmd.none )


initGrid =
    let
        xs =
            List.range 0 dim |> List.map ((*) cellSize) |> List.map toString

        makeCells lst i =
            if i < 0 then
                lst
            else
                let
                    ys =
                        toString (cellSize * i)

                    newList =
                        List.concat [ List.map (\x -> Cell x ys False initWalls) xs, lst ]
                in
                makeCells newList (i - 1)
    in
    Array.fromList (makeCells [] dim)


type Msg
    = Tick Time
    | NextCell Int


update msg model =
    case msg of
        Tick newTime ->
            ( model, Random.generate NextCell (Random.int 1 4) )

        NextCell dir ->
            ( model, Cmd.none )


subscriptions model =
    Sub.batch [ Time.every (10 * Time.millisecond) Tick ]


view model =
    Html.text "hello"


main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
