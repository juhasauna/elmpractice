module Main exposing (..)

import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (style)
import Html.Events exposing (on, onInput)
import Math.Vector2 as V2 exposing (..)
import String
import Svg exposing (..)
import Svg.Attributes exposing (..)


size =
    "600"


svgLine : Float -> Float -> Float -> Float -> Svg msg
svgLine x1_ y1_ x2_ y2_ =
    Svg.line
        [ x1 (x1_ |> String.fromFloat), y1 (y1_ |> String.fromFloat), x2 (x2_ |> String.fromFloat), y2 (y2_ |> String.fromFloat), stroke "black" ]
        []


svgLine2 : Float -> Float -> Float -> Float -> Float -> Svg msg
svgLine2 x1_ y1_ x2_ y2_ w =
    Svg.line
        [ x1 (x1_ |> String.fromFloat), y1 (y1_ |> String.fromFloat), x2 (x2_ |> String.fromFloat), y2 (y2_ |> String.fromFloat), stroke "black", strokeWidth (String.fromFloat w) ]
        []


makeLines : Vec2 -> Vec2 -> Float -> Float -> Float -> List (Svg msg) -> List (Svg msg)
makeLines v0 v1 angle girth angleOff lst =
    let
        v11 =
            rotate v0 v1 (degrees angle)

        dist =
            V2.distance v0 v1

        nextList =
            svgLine2 (getX v0) (getY v0) (getX v11) (getY v11) girth :: lst

        newList =
            if dist < 2 then
                lst
            else
                let
                    vLeft =
                        V2.vec2 0 (0.7 * -dist) |> V2.add v11

                    vRight =
                        V2.vec2 0 (0.6 * -dist) |> V2.add v11

                    left_ =
                        makeLines v11 vLeft (angle - angleOff) (girth * 0.7) angleOff []

                    right_ =
                        makeLines v11 vRight (angle + angleOff) (girth * 0.7) angleOff nextList
                in
                List.concat [ left_, right_ ]

        -- log =
        --     String.fromFloat (List.length newList) ++ String.fromFloat dist
        -- _ =
        --     Debug.log "l" log
    in
    newList


type alias Model =
    Float


type Msg
    = UpdateSlider1 String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateSlider1 value ->
            String.toFloat value |> Maybe.withDefault 330


view : Model -> Html.Html Msg
view model =
    let


        viewBoxDimensions =
            "0 0 " ++ size ++ " " ++ size

        v0 =
            V2.vec2 300 500

        v1 =
            V2.vec2 300 600

        lines1 =
            makeLines v0 v1 -90 15 model []

        lines =
            List.concat [ lines1 ]
    in
    div []
        [ svg
            [ Svg.Attributes.width size
            , Svg.Attributes.height size
            , viewBox viewBoxDimensions
            , Html.Attributes.style "display" "block"
            , Html.Attributes.style "margin" "0 auto"
            ]
            lines
        , input
            [ type_ "range"
            , Html.Attributes.min "0"
            , Html.Attributes.max "360"
            -- , Html.Attributes.defaultValue "330"
            , Html.Attributes.value <| String.fromFloat model
            , onInput UpdateSlider1
            ]
            []
        , Html.text <| String.fromFloat model
        ]

init = 330


main =
    Browser.sandbox
        { view = view
        , init = init
        , update = update
        }


rotate : Vec2 -> Vec2 -> Float -> Vec2
rotate v1 v2 angle =
    let
        dist =
            V2.distance v1 v2

        x =
            cos angle * dist

        y =
            sin angle * dist

        newV =
            V2.vec2 x y |> V2.add v1
    in
    newV