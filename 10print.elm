module Main exposing (..)

import Html exposing (..)
import Browser exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Math.Vector2 as V2 exposing (..)
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { dieFace : List Float
    }


type alias Line_ =
    { a : Vec2
    , b : Vec2
    }


init : () -> ( Model, Cmd Msg )
init _ =
        ( Model [], Random.generate NewFace (Random.list (2 * intSize) (Random.float 0 1)) )

-- UPDATE


type Msg
    = Roll
    | NewFace (List Float)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model, Random.generate NewFace (Random.list (2 * intSize) (Random.float 0 1)) )

        NewFace newFace ->
            ( Model newFace, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


intSize =
   800 


size =
    String.fromInt intSize


spacing =
    20


view : Model -> Html Msg
view model =
    let
        tenPrint =
            tenPrintLines model.dieFace []
    in
    div []
        [ button [ onClick Roll ]
            [ Html.text
                "Roll"
            ]
        , svg
            [ Svg.Attributes.width
                size
            , Svg.Attributes.height size
            ]
            tenPrint
        ]


tenPrintLines randFloats lines =
    if List.length randFloats == 0 then
        lines
    else
        let
            len =
                List.length randFloats

            x =
                remainderBy intSize (spacing * len) |> toFloat

            y =
                ((spacing * len) // intSize) * spacing |> toFloat

            rand =
                List.head randFloats |> Maybe.withDefault 1

            newLine =
                if rand < 0.5 then
                    makeLine (Line_ (vec2 x y) (vec2 (x + spacing) (y + spacing)))
                else
                    makeLine (Line_ (vec2 x (y + spacing)) (vec2 (x + spacing) y))

            newLines =
                newLine :: lines
        in
        tenPrintLines (List.drop 1 randFloats) newLines

type alias LineCoordinates =
    { x1 : String
    , y1 : String
    , x2 : String
    , y2 : String
    }



makeLine : Line_ -> Svg Msg
makeLine line_ =
    let
        xys = LineCoordinates (String.fromFloat (getX line_.a)) (String.fromFloat (getY line_.a)) (String.fromFloat (getX line_.b)) (String.fromFloat (getY line_.b))
    in
    Svg.line
        [ x1 xys.x1, y1 xys.y1, x2 xys.x2, y2 xys.y2, stroke "black" ]
        []
