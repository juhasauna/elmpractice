module Main exposing (..)

import Html exposing (Html, div, input, text)
import Html.Attributes exposing (style)
import Html.Events exposing (on, onInput)
import Math.Vector2 as V2 exposing (..)
import String
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Utils exposing (..)


size =
    "600"


svgLine : Float -> Float -> Float -> Float -> Svg msg
svgLine x1_ y1_ x2_ y2_ =
    Svg.line
        [ x1 (x1_ |> toString), y1 (y1_ |> toString), x2 (x2_ |> toString), y2 (y2_ |> toString), stroke "black" ]
        []


svgLine2 : Float -> Float -> Float -> Float -> Float -> Svg msg
svgLine2 x1_ y1_ x2_ y2_ w =
    Svg.line
        [ x1 (x1_ |> toString), y1 (y1_ |> toString), x2 (x2_ |> toString), y2 (y2_ |> toString), stroke "black", strokeWidth (toString w) ]
        []


makeLines : Vec2 -> Vec2 -> Float -> Float -> Float -> List (Svg msg) -> List (Svg msg)
makeLines v0 v1 angle girth angleOff lst =
    let
        v11 =
            Utils.rotate v0 v1 (degrees angle)

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
        --     toString (List.length newList) ++ toString dist
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
            String.toFloat value |> Result.withDefault 330


view : Model -> Html.Html Msg
view model =
    let
        parentStyle =
            Html.Attributes.style [ ( "margin", "0 auto" ), ( "display", "block" ) ]

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
            , parentStyle
            ]
            lines
        , input
            [ type_ "range"
            , Html.Attributes.min "0"
            , Html.Attributes.max "360"
            , Html.Attributes.defaultValue "330"
            , Html.Attributes.value <| toString model
            , onInput UpdateSlider1
            ]
            []
        , Html.text <| toString model
        ]


main : Program Never Float Msg
main =
    Html.beginnerProgram
        { model = 330
        , view = view
        , update = update
        }
