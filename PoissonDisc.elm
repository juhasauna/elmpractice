module Main exposing (..)

import Color exposing (Color, hsl)
import Color.Convert
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Math.Vector2 as V2 exposing (..)
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time)


-- main : Program Never (Dict.Dict Int Vec2) Msg


main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



--MODEL


type alias Model =
    { grid : Dict.Dict Int Vec2
    , active : Dict.Dict Int Vec2
    , order : List Int
    }


sizeFloat =
    600


size : String
size =
    toString sizeFloat


rr =
    2


w =
    6 * rr / sqrt 2


cols =
    floor (sizeFloat / w)


rows =
    floor (sizeFloat / w)


centerPoint =
    let
        center =
            sizeFloat / 2

        ( i, j ) =
            ( gridPos center, gridPos center )
    in
    { key = i + j * cols, point = V2.vec2 center center }


gridPos coordinate =
    floor (coordinate / w)



-- initGrid : Dict.Dict Int Vec2


initGrid =
    let
        makeDict =
            Dict.fromList [ ( centerPoint.key, centerPoint.point ) ]
    in
    Model makeDict makeDict [ centerPoint.key ]



-- init : ( ( Dict.Dict Int Vec2, Dict.Dict Int Vec2 ), Cmd msg )


init =
    ( initGrid, Cmd.none )



--UPDATE


type Msg
    = Tick Time
    | Roll (List Float)



-- update : Msg -> Dict.Dict Int Vec2 -> ( Dict.Dict Int Vec2, Cmd Msg )


update msg model =
    case msg of
        Tick newTime ->
            ( Model model.grid model.active model.order, Random.generate Roll (Random.list 3 (Random.float 0 1)) )

        Roll randomFloats ->
            let
                xoff =
                    rr
                        * 10
                        * ((List.head randomFloats
                                |> Maybe.withDefault 0
                           )
                            - 0.5
                          )

                yoff =
                    rr
                        * 10
                        * ((List.drop 1 randomFloats
                                |> List.head
                                |> Maybe.withDefault 0
                           )
                            - 0.5
                          )

                randomActiveKey =
                    getRandomActiveKey model.active
                        (List.drop 2 randomFloats
                            |> List.head
                            |> Maybe.withDefault 0
                        )

                oldPos =
                    Dict.get randomActiveKey model.active |> Maybe.withDefault (vec2 300 300)

                newPos =
                    vec2 (getX oldPos + xoff) (getY oldPos + yoff)

                gridIndex =
                    getGridIndex (getX newPos) (getY newPos)

                gridPosUsed =
                    Dict.member gridIndex model.grid

                grid =
                    if gridPosUsed then
                        model.grid
                    else
                        Dict.update gridIndex (\_ -> Just newPos) model.grid

                order =
                    if gridPosUsed then
                        model.order
                    else
                        gridIndex :: model.order

                active =
                    if gridPosUsed then
                        updateActive model.active model.grid gridIndex
                    else
                        Dict.insert gridIndex newPos model.active

                newModel =
                    Model grid active order
            in
            ( newModel, Cmd.none )


getRandomActiveKey active randFloat =
    let
        activeKeys =
            Dict.keys active

        activeCount =
            List.length activeKeys |> toFloat

        randIndex =
            activeCount * randFloat |> floor

        randomKey =
            List.drop randIndex activeKeys |> List.head |> Maybe.withDefault 0
    in
    randomKey


updateActive active grid i =
    let
        neighbors =
            [ checkNeighbor grid (i - cols)
            , checkNeighbor grid (i + 1)
            , checkNeighbor grid (i + cols)
            , checkNeighbor grid (i - 1)
            , checkNeighbor grid (i - cols - 1)
            , checkNeighbor grid (i - cols + 1)
            , checkNeighbor grid (i + cols - 1)
            , checkNeighbor grid (i + cols + 1)
            ]

        allNeighborsOccupied =
            List.all (\a -> a) neighbors

        -- log =
        --     toString neighbors ++ "___" ++ toString allNeighborsOccupied
        -- _ =
        --     Debug.log "test" log
        newActive =
            if allNeighborsOccupied then
                Dict.remove i active
            else
                active
    in
    newActive


isTrue bool =
    bool


checkNeighbor grid index =
    if Dict.member index grid then
        True
    else
        False


getGridIndex x y =
    gridPos y + gridPos x * cols



--SUBSCRIPTIONS


subscriptions model =
    Sub.batch [ Time.every (10 * Time.millisecond) Tick ]


view model =
    let
        parentStyle =
            Html.Attributes.style [ ( "margin", "0 auto" ), ( "display", "block" ) ]

        viewBoxDimensions =
            "0 0 " ++ size ++ " " ++ size

        -- grid =
        --     model.grid
        -- Tuple.second model
        -- gridToList =
        --     Dict.toList grid
        -- ( gridKeys, gridValues ) =
        --     List.unzip gridToList
        -- f =
        --     makeCircles grid |> toFloat)
        -- makeCircles (Dict.size (Tuple.first model) |> toFloat)
        order =
            model.order

        -- List.reverse model.order
        orderedValues =
            orderValues order model.grid []

        colors =
            List.range 0 (List.length orderedValues - 1)

        -- List.map2 getDots model.order gridValues
        dots =
            List.map2 makeCircles colors orderedValues

        dots2 =
            List.concat [ [ Svg.rect [ x "0", y "0", Svg.Attributes.width size, Svg.Attributes.height size, fill "black" ] [] ], dots ]
    in
    svg
        [ Svg.Attributes.width size
        , Svg.Attributes.height size
        , viewBox viewBoxDimensions
        , parentStyle
        ]
        dots2


renderBackground =
    rect [ x "0", y "0", Svg.Attributes.width size, Svg.Attributes.height size, fill "#333333" ] []


orderValues order grid orderedList =
    if List.length order == 0 then
        orderedList
    else
        let
            key =
                List.head order |> Maybe.withDefault 0

            newOrder =
                List.drop 1 order

            newList =
                (Dict.get key grid |> Maybe.withDefault (vec2 300 300)) :: orderedList
        in
        orderValues newOrder grid newList


makeCircles colour v =
    let
        hue =
            toFloat colour / 50

        --(colour % 360)
        ( strX, strY ) =
            ( toString (getX v), toString (getY v) )

        theColor =
            Color.Convert.colorToCssHsl (Color.hsl hue 1 0.5)
    in
    Svg.circle
        [ cx strX, cy strY, r (toString rr), fill theColor ]
        []
