module RandomWalker exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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


size =
    "600"


type alias Walker =
    { x : Float
    , y : Float
    }


type alias Model =
    { trace : List Walker, stop : Bool }


init : () -> ( Model, Cmd msg )
init _ =
    ( Model [ Walker 300 300 ] False, Cmd.none )



--UPDATE


type Msg
    = Tick Time.Posix
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
        Sub.batch [ Time.every 1 Tick ]



--VIEW


view : Model -> Html.Html Msg
view model =
    let

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
            , (Html.Attributes.style "display" "block")
            , (Html.Attributes.style "margin" "0 auto")

            ]
            dots
        , Html.button [ Html.Events.onClick ReleaseWalker ] [ Html.text "WALK!" ]
        ]


makeLines : Walker -> Walker -> Svg Msg
makeLines pos1 pos2 =
    let

        xys = LineCoordinates (String.fromFloat pos1.x) (String.fromFloat pos1.y) (String.fromFloat pos2.x) (String.fromFloat pos2.y)
    in
    Svg.line
        [ x1 xys.x1, y1 xys.y1, x2 xys.x2, y2 xys.y2, stroke "black" ]
        []


type alias LineCoordinates =
    { x1 : String
    , y1 : String
    , x2 : String
    , y2 : String
    }