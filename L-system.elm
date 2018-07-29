module Main exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Math.Vector2 as V2 exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Utils exposing (..)


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
        [ x1 (x1_ |> toString), y1 (y1_ |> toString), x2 (x2_ |> toString), y2 (y2_ |> toString), stroke "black" ]
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
        parentStyle =
            Html.Attributes.style [ ( "margin", "0 auto" ), ( "display", "block" ) ]

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
            , parentStyle
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

            ( newList, newAngle, newBase, newL ) =
                if current == "+" then
                    changeAngle "+" lst angle base l
                else if current == "-" then
                    changeAngle "-" lst angle base l
                else if current == "[" then
                    push lst angle base l
                else if current == "]" then
                    pop lst angle base l
                else if current == "F" then
                    makeNewVector lst angle base l
                else
                    ( [], 0, [], 0 )
        in
        turtle2 sentence (i + 1) newList newAngle newBase newL


makeNewVector lst angle base l =
    let
        baseCount =
            List.length base |> toFloat

        scaleFactor =
            0.5 ^ (baseCount - 1)

        ( v0, v1 ) =
            getFirstListVectorTuple lst

        rotated =
            Utils.rotate v1 v0 angle

        dist =
            20

        dir =
            V2.direction v1 rotated

        v11 =
            V2.add v1 (V2.scale -(dist * scaleFactor) dir)

        log =
            toString dist ++ toString rotated ++ toString v11

        newList =
            ( v1, v11 ) :: lst
    in
    ( newList, angle, base, l )


pop lst angle base l =
    let
        newBase =
            List.drop 1 base

        newList =
            getFirstListVectorTuple base :: lst
    in
    ( newList, angle, newBase, l )


push lst angle base l =
    let
        newL =
            l * 0.9

        newBase =
            getFirstListVectorTuple lst :: base
    in
    ( lst, angle, newBase, newL )


changeAngle sign lst angle base l =
    let
        angleChange =
            if sign == "+" then
                degrees 22.5
            else
                degrees -22.5
    in
    ( lst, angle + angleChange, base, l )


makeLines vectorList lineList =
    if List.length vectorList == 0 then
        lineList
    else
        let
            ( v0, v2 ) =
                getFirstListVectorTuple vectorList

            ( x1, y1, x2, y2 ) =
                ( V2.getX v0, V2.getY v0, V2.getX v2, V2.getY v2 )

            newLine =
                svgLine x1 y1 x2 y2

            newList =
                newLine :: lineList
        in
        makeLines (List.drop 1 vectorList) newList
