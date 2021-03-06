module Main exposing (..)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Math.Vector2 as V2 exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Math.Vector2 as V2 exposing (..)


rules : { a : String, b : String }
rules =
    { a = "F", b = "FF+[+F-F-F]-[-F+F+F]" }


getFirstListVectorTuple lst =
    List.head lst |> Maybe.withDefault ( V2.vec2 0 0, V2.vec2 0 0 )


size : String
size =
    "800"


svgLine : Float -> Float -> Float -> Float -> Svg msg
svgLine x1_ y1_ x2_ y2_ =
    Svg.line
        [ x1 (x1_ |> String.fromFloat), y1 (y1_ |> String.fromFloat), x2 (x2_ |> String.fromFloat), y2 (y2_ |> String.fromFloat), stroke "black" ]
        []


loopSentence sentence nextSentence i =
    if i == String.length sentence then
        nextSentence
    else
        let
            current =
                String.slice i (i + 1) sentence

            nextSentence2 =
                nextSentence
                    ++ (if current == rules.a then
                            rules.b
                        else
                            current
                       )
        in
        loopSentence sentence nextSentence2 (i + 1)


generate sentence i =
    if i == 4 then
        sentence
    else
        let
            newSentence =
                loopSentence sentence "" 0
        in
        generate newSentence (i + 1)


main : Html msg
main =
    let

        viewBoxDimensions =
            "0 0 " ++ size ++ " " ++ size

        generated =
            generate "F" 0

        l =
            20

        root =
            V2.vec2 300 600

        getSvgLines =
            turtle2 generated 0 [ ( root, V2.sub root (V2.vec2 0 l) ) ] (degrees -90) [] l

        lines =
            makeLines getSvgLines []
    in
    div []
        [ svg
            [ Svg.Attributes.width size
            , Svg.Attributes.height size
            , viewBox viewBoxDimensions
            , (Html.Attributes.style "display" "block")
            , (Html.Attributes.style "margin" "0 auto")
            ]
            lines
        ]


turtle2 sentence i lst angle base l =
    if i == String.length sentence then
        lst
    else
        let
            current =
                String.slice i (i + 1) sentence


            next =
                if current == "+" then
                    {lst = lst, angle = angle + degrees 22.5, base = base, l = l}
                else if current == "-" then
                    {lst = lst, angle = angle - degrees 22.5, base = base, l = l}
                else if current == "[" then
                    {lst = lst, angle = angle, base = getFirstListVectorTuple lst :: base, l = l * 0.9}
                else if current == "]" then
                    {lst = getFirstListVectorTuple base :: lst, angle = angle, base = List.drop 1 base, l = l}
                else if current == "F" then
                    {lst = makeNewVector lst angle base l, angle = angle, base = base, l = l}
                else
                    {lst = [], angle = 0, base = [], l = 0}


        in
        turtle2 sentence (i + 1) next.lst next.angle next.base next.l


makeNewVector lst angle base l =
    let
        baseCount =
            List.length base |> toFloat

        scaleFactor =
            0.5 ^ (baseCount - 1)

        ( v0, v1 ) =
            getFirstListVectorTuple lst

        rotated =
            rotate v1 v0 angle

        dist =
            20

        dir =
            V2.direction v1 rotated

        v11 =
            V2.add v1 (V2.scale -(dist * scaleFactor) dir)

        newList =
            ( v1, v11 ) :: lst
    in
    newList




makeLines vectorList lineList =
    if List.length vectorList == 0 then
        lineList
    else
        let
            ( v0, v2 ) =
                getFirstListVectorTuple vectorList


            xys = {x1 = V2.getX v0, y1 = V2.getY v0, x2 = V2.getX v2, y2 = V2.getY v2}


            newLine =
                svgLine xys.x1 xys.y1 xys.x2 xys.y2

            newList =
                newLine :: lineList
        in
        makeLines (List.drop 1 vectorList) newList




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