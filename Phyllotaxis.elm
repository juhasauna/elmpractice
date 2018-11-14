module Main exposing (Block, Color(..), Msg(..), Snake, colorToCssHsl, cssColorString, fmod, getNewHead, getOldHead, hsl, hsla, hueToString, init, main, renderBlock, rgbToHsl, size, subscriptions, tick, toHsl, toPercentString, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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



--model


type alias Block =
    { x : Float
    , y : Float
    , n : Float
    , c : Float
    , l : Float
    }


type alias Snake =
    List Block


init : () -> ( Snake, Cmd Msg )
init _ =
    let
        snakeStart =
            [ Block 200 200 10 3 0
            ]
    in
    ( snakeStart
    , Cmd.none
    )



--update


type Msg
    = Tick Time.Posix
    | Roll
    | UpdateSlider String


update : Msg -> Snake -> ( Snake, Cmd Msg )
update msg snake =
    case msg of
        Tick time ->
            let
                newHead =
                    getOldHead snake |> getNewHead 0
            in
            ( newHead :: snake, Cmd.none )

        Roll ->
            let
                newHead =
                    getOldHead snake |> getNewHead 1
            in
            ( newHead :: snake, Cmd.none )

        UpdateSlider value ->
            let
                l =
                    -- case (String.toFloat value) of
                    --     Ok v ->
                    --         v / 100
                    --     Err e ->
                    --         0
                    (String.toFloat value |> Maybe.withDefault 0) / 100

                newHead =
                    getOldHead snake |> getNewHead l
            in
            ( newHead :: snake, Cmd.none )


getOldHead : Snake -> Block
getOldHead snake =
    let
        oldHead =
            List.head snake
                |> Maybe.withDefault { x = 1, y = 1, n = 1, c = 4, l = 0 }
    in
    oldHead


getNewHead : Float -> Block -> Block
getNewHead testVar oldHead =
    let
        n =
            oldHead.n

        l =
            oldHead.l + testVar

        a =
            n + logBase 2 n * l

        c =
            oldHead.c

        r =
            c * sqrt n

        center :
            { x : Float
            , y : Float
            }
        center =
            { x = 200
            , y = 200
            }

        newHead =
            { oldHead
                | y = r * sin a + center.x
                , x = r * cos a + center.y
                , n = n + 1
                , c = c
                , l = l
            }
    in
    newHead



--subscriptions


subscriptions : Snake -> Sub Msg
subscriptions model =
    Sub.batch [ tick ]


tick : Sub Msg
tick =
    Time.every 1 Tick



--view


size : String
size =
    "600"


view : Snake -> Html.Html Msg
view snake =
    let
        viewBoxDimensions =
            "0 0 " ++ size ++ " " ++ size

        dots =
            List.map renderBlock snake

        oldHead =
            getOldHead snake
    in
    div []
        [ h1 []
            [ Html.text "hello" ]
        , button [ onClick Roll ]
            [ Html.text "Roll" ]
        , svg
            [ Svg.Attributes.width
                size
            , Svg.Attributes.height size
            , viewBox viewBoxDimensions
            , Html.Attributes.style "display" "block"
            , Html.Attributes.style "margin" "0 auto"
            ]
            dots
        , input
            [ Html.Attributes.type_ "range"
            , Html.Attributes.min "1"
            , Html.Attributes.max "100"
            , value <| String.fromFloat oldHead.l
            , onInput UpdateSlider
            ]
            []
        , Html.text <| String.fromFloat oldHead.l
        ]


renderBlock : Block -> Svg Msg
renderBlock block =
    let
        ( strX, strY ) =
            ( String.fromFloat block.x, String.fromFloat block.y )

        hue =
            block.n / 50

        theColor =
            colorToCssHsl (hsl hue 1 0.5)
    in
    Svg.circle [ cx strX, cy strY, r "2", fill theColor ] []


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
toHsl : Color -> { hue : Float, saturation : Float, lightness : Float, alpha : Float }
toHsl color =
    case color of
        HSLA h s l a ->
            { hue = h, saturation = s, lightness = l, alpha = a }

        RGBA r g b a ->
            let
                ( h, s, l ) =
                    rgbToHsl r g b
            in
            { hue = h, saturation = s, lightness = l, alpha = a }


hsl : Float -> Float -> Float -> Color
hsl hue saturation lightness =
    hsla hue saturation lightness 1


{-| Representation of colors.
-}
type Color
    = RGBA Int Int Int Float
    | HSLA Float Float Float Float


rgbToHsl : Int -> Int -> Int -> ( Float, Float, Float )
rgbToHsl red green blue =
    let
        r =
            toFloat red / 255

        g =
            toFloat green / 255

        b =
            toFloat blue / 255

        cMax =
            Basics.max (Basics.max r g) b

        cMin =
            Basics.min (Basics.min r g) b

        c =
            cMax - cMin

        hue =
            degrees 60
                * (if cMax == r then
                    fmod ((g - b) / c) 6

                   else if cMax == g then
                    ((b - r) / c) + 2

                   else
                    {- cMax == b -}
                    ((r - g) / c) + 4
                  )

        lightness =
            (cMax + cMin) / 2

        saturation =
            if lightness == 0 then
                0

            else
                c / (1 - abs (2 * lightness - 1))
    in
    ( hue, saturation, lightness )


{-| Create [HSL colors](http://en.wikipedia.org/wiki/HSL_and_HSV)
with an alpha component for transparency.
-}
hsla : Float -> Float -> Float -> Float -> Color
hsla hue saturation lightness alpha =
    HSLA (hue - turns (toFloat (floor (hue / (2 * pi))))) saturation lightness alpha


fmod : Float -> Int -> Float
fmod f n =
    let
        integer =
            floor f
    in
    -- toFloat (integer % n) + f - toFloat integer
    toFloat (modBy n integer) + f - toFloat integer


hueToString : Float -> String
hueToString =
    (*) 180 >> (*) (1 / pi) >> round >> String.fromInt



-- (*) 180 >> flip (/) pi >> round >> String.fromFloat


toPercentString : Float -> String
toPercentString =
    (*) 100 >> round >> String.fromInt



-- (*) 100 >> round >> String.fromFloat >> flip (++) "%"
