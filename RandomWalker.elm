module RandomWalker exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



--MODEL


size =
    "600"


type alias Walker =
    { x : Float
    , y : Float
    }


type alias Model =
    { trace : List Walker, stop : Bool }


init : ( Model, Cmd msg )
init =
    ( Model [ Walker 300 300 ] False, Cmd.none )



--UPDATE


type Msg
    = Tick Time
    | Roll (List Float)
    | ReleaseWalker


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReleaseWalker ->
            ( { model | stop = not model.stop }, Cmd.none )

        Tick newTime ->
            ( model, Random.generate Roll (Random.list 2 (Random.float -5 5)) )

        Roll off ->
            let
                xoff =
                    List.head off
                        |> Maybe.withDefault 0

                yoff =
                    List.reverse off
                        |> List.head
                        |> Maybe.withDefault 0

                oldPos =
                    List.head model.trace |> Maybe.withDefault { x = 1, y = 1 }

                newPos =
                    { oldPos
                        | x = oldPos.x + xoff
                        , y = oldPos.y + yoff
                    }
            in
            ( { model | trace = newPos :: model.trace }, Cmd.none )



--SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.stop then
        Sub.none
    else
        Sub.batch [ Time.every (10 * Time.millisecond) Tick ]



--VIEW


view : Model -> Html.Html Msg
view model =
    let
        parentStyle =
            Html.Attributes.style [ ( "margin", "0 auto" ), ( "display", "block" ) ]

        viewBoxDimensions =
            "0 0 " ++ size ++ " " ++ size

        traceLength =
            List.length model.trace

        oldTrace =
            List.drop 1 model.trace

        dots =
            List.map2 makeLines model.trace oldTrace
    in
    div []
        [ svg
            [ Svg.Attributes.width
                size
            , Svg.Attributes.height size
            , viewBox viewBoxDimensions
            , parentStyle
            ]
            dots
        , Html.button [ Html.Events.onClick ReleaseWalker ] [ Html.text "WALK!" ]
        ]


makeLines : Walker -> Walker -> Svg Msg
makeLines pos1 pos2 =
    let
        ( strX, strY, strX2, strY2 ) =
            ( toString pos1.x, toString pos1.y, toString pos2.x, toString pos2.y )
    in
    Svg.line
        [ x1 strX, y1 strY, x2 strX2, y2 strY2, stroke "black" ]
        []
