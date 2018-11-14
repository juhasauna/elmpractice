-- module Main exposing (Model, Msg(..), centerPoint, checkNeighbor, cols, getGridIndex, getRandomActiveKey, gridPos, init, initGrid, isTrue, main, makeCircles, orderValues, renderBackground, rows, rr, size, sizeFloat, subscriptions, update, updateActive, view, w)
module Main exposing (..)

import Browser
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Math.Vector2 as V2 exposing (..)
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time


main =
    Browser.element
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
    String.fromInt sizeFloat


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


init : () -> ( Model, Cmd msg )
init _ =
    ( initGrid, Cmd.none )



--UPDATE


type Msg
    = Tick Time.Posix
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
    Sub.batch [ Time.every 1 Tick ]


view model =
    let
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
        , Html.Attributes.style "display" "block"
        , Html.Attributes.style "margin" "0 auto"
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

        ( strX, strY ) =
            ( String.fromFloat (getX v), String.fromFloat (getY v) )

        theColor =
            colorToCssHsl (hsl hue 1 0.5)
        -- _ = Debug.log "color" theColor
    in
    Svg.circle
        [ cx strX, cy strY, r (String.fromInt rr), fill theColor ]
        []


colorToCssHsl : Color -> String
colorToCssHsl cl =
    let
        { hue, saturation, lightness, alpha } =
            toHsl cl
    in
        cssColorString "hsl"
            [ hueToString hue
            , toPercentString saturation ++ "%"
            , toPercentString lightness ++ "%"
            ]


cssColorString : String -> List String -> String
cssColorString kind values =
    kind ++ "(" ++ String.join ", " values ++ ")"


{-| Extract the components of a color in the HSL format.
-}
toHsl : Color -> { hue:Float, saturation:Float, lightness:Float, alpha:Float }
toHsl color =
  case color of
    HSLA h s l a ->
      { hue=h, saturation=s, lightness=l, alpha=a }

    RGBA r g b a ->
      let
        (h,s,l) = rgbToHsl r g b
      in
        { hue=h, saturation=s, lightness=l, alpha=a }

hsl : Float -> Float -> Float -> Color
hsl hue saturation lightness =
  hsla hue saturation lightness 1


{-| Representation of colors.
-}
type Color
    = RGBA Int Int Int Float
    | HSLA Float Float Float Float

rgbToHsl : Int -> Int -> Int -> (Float,Float,Float)
rgbToHsl red green blue =
  let
    r = toFloat red   / 255
    g = toFloat green / 255
    b = toFloat blue  / 255

    cMax = Basics.max (Basics.max r g) b
    cMin = Basics.min (Basics.min r g) b

    c = cMax - cMin

    hue =
      degrees 60 *
        if cMax == r then
          fmod ((g - b) / c) 6
        else if cMax == g then
          ((b - r) / c) + 2
        else {- cMax == b -}
          ((r - g) / c) + 4

    lightness =
      (cMax + cMin) / 2

    saturation =
      if lightness == 0 then
        0
      else
        c / (1 - abs (2 * lightness - 1))
  in
    (hue, saturation, lightness)

{-| Create [HSL colors](http://en.wikipedia.org/wiki/HSL_and_HSV)
with an alpha component for transparency.
-}
hsla : Float -> Float -> Float -> Float -> Color
hsla hue saturation lightness alpha =
  HSLA (hue - turns (toFloat (floor (hue / (2*pi))))) saturation lightness alpha


fmod : Float -> Int -> Float
fmod f n =
  let
    integer = floor f
  in
    -- toFloat (integer % n) + f - toFloat integer
    toFloat (modBy n integer) + f - toFloat integer

hueToString : Float -> String
hueToString =
    (*) 180 >> (*) (1/pi) >> round >> String.fromInt
    -- (*) 180 >> flip (/) pi >> round >> String.fromFloat

toPercentString : Float -> String
toPercentString =
    (*) 100 >> round >> String.fromInt
    
    -- (*) 100 >> round >> String.fromFloat >> flip (++) "%"