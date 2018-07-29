module Main exposing (..)

import Color exposing (Color, hsl)
import Color.Convert
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time)


main : Program Never Snake Msg
main =
    Html.program
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


init : ( Snake, Cmd Msg )
init =
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
    = Tick Time
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
                    (String.toFloat value |> Result.withDefault 0) / 100

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
    Time.every (10 * Time.millisecond) Tick



--view


size : String
size =
    "600"


view : Snake -> Html.Html Msg
view snake =
    let
        parentStyle =
            Html.Attributes.style [ ( "margin", "0 auto" ), ( "display", "block" ) ]

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
            , parentStyle
            ]
            dots
        , input
            [ Html.Attributes.type_ "range"
            , Html.Attributes.min "1"
            , Html.Attributes.max "100"
            , value <| toString oldHead.l
            , onInput UpdateSlider
            ]
            []
        , Html.text <| toString oldHead.l
        ]


renderBlock : Block -> Svg Msg
renderBlock block =
    let
        ( strX, strY, hue ) =
            ( toString block.x, toString block.y, block.n / 50 )

        theColor =
            Color.Convert.colorToCssHsl (Color.hsl hue 1 0.5)
    in
    Svg.circle [ cx strX, cy strY, r "2", fill theColor ] []
