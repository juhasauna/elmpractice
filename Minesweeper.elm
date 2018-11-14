module Main exposing (..)

import Browser
import Browser.Events
import Json.Decode
import Array
import Html exposing (..)
import Html.Attributes exposing (style, type_)
import Html.Events exposing (..)
import Random
import Svg exposing (..)
import Svg.Attributes as SvgA exposing (..)


dim : Int
dim =
    10


cellCount =
    dim ^ 2

cellSize : Int
cellSize =
    20

marg : Int
marg =
    cellSize


gridSize =
    String.fromInt (dim * cellSize + marg + 1)


strCS =
    String.fromInt cellSize


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Cell =
    { x : String, y : String, revealed : Bool, hasMine : Bool, closeMines : Int }


type alias GameOver =
    { win : Bool, lose : Bool }

type alias Position =
    { x : Int
    , y : Int
    }

type alias Model =
    { position : Position
    , grid : Array.Array Cell
    , gameOver : GameOver
    }

init : () -> (Model, Cmd Msg)
init _ =
    ( Model (Position 0 0) Array.empty (GameOver False False)
    , Random.generate InitModel (Random.list cellCount (Random.float 0 1))
    )



-- UPDATE


type Msg
    = MouseDowns Int Int
    | InitModel (List Float)
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitModel floatMines ->
            ( Model (Position 0 0) (initGrid floatMines) (GameOver False False), Cmd.none )

        Reset ->
            ( model, Random.generate InitModel (Random.list cellCount (Random.float 0 1)) )

        MouseDowns xx yy ->
            let
                (x,y) = (xx - cellSize // 2, yy- cellSize // 2)
                _ = Debug.log "asdf" <| (String.fromInt x ++ " " ++ String.fromInt y)
                getIndex =
                    let
                        inBounds val =
                            let
                                v =
                                    val - marg
                            in
                            v >= 0 && v < dim * cellSize

                        isInBounds =
                            inBounds x && inBounds y

                        f val =
                            let
                                v =
                                    val - marg
                                    
                            in
                            round (toFloat(v - dim) / (toFloat cellSize))

                        ( x_, y_ ) =
                            ( f x, f y )

                        ret =
                            if isInBounds then
                                x_ + y_ * dim
                            else
                                -1
                    in
                    ret

                arrIndex =
                    getIndex

                grid =
                    if arrIndex == -1 then
                        model.grid
                    else
                        setRevealed arrIndex model.grid

                lose =
                    if (getCell arrIndex grid).hasMine then
                        True
                    else
                        model.gameOver.lose

                win =
                    if lose then
                        False
                    else
                        checkIfDone grid (cellCount - 1)

                winGrid =
                    if win then
                        showMines grid (cellCount - 1)
                    else
                        grid
            in
            ( { model | position = Position x y, grid = winGrid, gameOver = GameOver win lose }, Cmd.none )


checkIfDone grid i =
    if i < 0 then
        True
    else
        let
            cell =
                getCell i grid
        in
        if cell.hasMine || cell.revealed then
            checkIfDone grid (i - 1)
        else
            False


setRevealed i grid =
    let
        cell =
            getCell i grid

        newGrid =
            Array.set i { cell | revealed = True } grid
    in
    if cell.closeMines > 0 then
        newGrid
    else
        revealNeighbors i newGrid


revealNeighbors i grid =
    let
        neighbors =
            removeRevealed (getNeighbors i) grid

        f l g =
            if List.length l == 0 then
                g
            else
                let
                    test =
                        List.head l |> Maybe.withDefault 0

                    newGrid =
                        setRevealed test g
                in
                f (List.drop 1 l) newGrid
    in
    f neighbors grid


removeRevealed neighbors grid =
    let
        areRevealed =
            List.map (\n -> (getCell n grid).revealed) neighbors

        revealedNeighbors =
            List.map2
                (\a b ->
                    if a then
                        -1
                    else
                        b
                )
                areRevealed
                neighbors
                |> List.filter (\n -> n > -1)
    in
    revealedNeighbors


getNeighbors i =
    let
        neighbors =
            [ i + 1, i - 1, i + dim, i - dim, i + dim + 1, i - dim + 1, i + dim - 1, i - dim - 1 ]
                |> List.filter (\n -> n >= 0)
                |> List.filter (\n -> n < cellCount)

        checkFirstAndLastCols =
            if remainderBy dim i == 0 then
                -- first col
                List.filter (\n -> n /= i - 1 && n /= i - 1 - dim && n /= i - 1 + dim) neighbors
            else if remainderBy dim i == (dim - 1) then
                -- last col
                List.filter (\n -> n /= i + 1 && n /= i + 1 - dim && n /= i + 1 + dim) neighbors
            else
                neighbors
    in
    checkFirstAndLastCols


getCell i grid =
    Array.get i grid
        |> Maybe.withDefault { x = "0", y = "0", revealed = False, hasMine = False, closeMines = 0 }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    
    Sub.batch [ Browser.Events.onClick (Json.Decode.map2 MouseDowns pageX pageY) ]

    -- Sub.batch [ Mouse.downs MouseDowns ]
pageX : Json.Decode.Decoder Int
pageX =
  Json.Decode.field "pageX" Json.Decode.int


pageY : Json.Decode.Decoder Int
pageY =
  Json.Decode.field "pageY" Json.Decode.int


-- VIEW


view : Model -> Html Msg
view model =
    let
        gameMsg =
            if model.gameOver.lose then
                "You have been killed by a mine :-("
            else if model.gameOver.win then
                "Mines cleared succesfully. You are a genious!"
            else
                ""

        makeSvgWin =
            makeSvg model.gameOver.win

        shapes =
            Array.map makeSvgWin model.grid |> Array.toList |> List.concat

        pad =
            String.fromInt marg ++ "px"
    in
    div
        []
        [ svg
            [ SvgA.width gridSize
            , SvgA.height gridSize
            ]
            shapes
        , div
            [ Html.Attributes.style "padding" pad]
            [ Html.text gameMsg, Html.br [] [], Html.button [ onClick Reset ] [ Html.text "Reset" ] ]
        ]


showMines grid i =
    if i < 0 then
        grid
    else
        let
            cell =
                getCell i grid

            newGrid =
                Array.set i { cell | revealed = True } grid
        in
        showMines newGrid (i - 1)


initGrid floatMines =
    let
        xPoints =
            List.range 0 dim |> List.map ((*) cellSize) |> List.map ((+) marg) |> List.map String.fromInt

        minePr =
            0.1

        boolMines =
            List.map (\n -> n < minePr) floatMines

        makeDims lst mines i =
            if i < 0 then
                List.concat lst
            else
                let
                    yPoints =
                        List.repeat dim (cellSize * i) |> List.map ((+) marg) |> List.map String.fromInt

                    showList =
                        List.repeat dim False

                    newLst =
                        List.map4 (\x y c d -> { x = x, y = y, revealed = c, hasMine = d, closeMines = 0 }) xPoints yPoints showList mines
                            :: lst

                    newMines =
                        List.drop dim mines
                in
                makeDims newLst newMines (i - 1)

        grid =
            makeDims [] boolMines (dim - 1)

        f_curried =
            countNeighborMines (Array.fromList (List.map .hasMine grid))

        mineCounts =
            List.map f_curried (List.range 0 cellCount)

        gridAddMineCounts =
            List.map2 (\a b -> { a | closeMines = b }) grid mineCounts
    in
    Array.fromList gridAddMineCounts


countNeighborMines hasMines i =
    let
        indexes =
            i :: getNeighbors i

        mineCount =
            List.map
                (\n ->
                    if Array.get n hasMines |> Maybe.withDefault False then
                        1
                    else
                        0
                )
                indexes
                |> List.sum
    in
    mineCount


makeSvg win { x, y, revealed, hasMine, closeMines } =
    let
        ( xx, yy ) =
            ( Maybe.withDefault 0 (String.toFloat x) + toFloat cellSize / 2 |> String.fromFloat
            , Maybe.withDefault 0 (String.toFloat y) + toFloat cellSize / 2 |> String.fromFloat
            )

        ( mineRadius, rectColor, mineCount ) =
            if hasMine && revealed then
                ( "3"
                , if win then
                    "gray"
                  else
                    "red"
                , ""
                )
            else if revealed then
                ( "0"
                , "lightgray"
                , if closeMines == 0 then
                    ""
                  else
                    String.fromInt closeMines
                )
            else
                ( "0"
                , "gray"
                , ""
                )
    in
    [ Svg.rect
        [ SvgA.x x
        , SvgA.y y
        , SvgA.fill rectColor
        , SvgA.strokeWidth "1"
        , SvgA.stroke "black"
        , SvgA.height strCS
        , SvgA.width strCS
        ]
        []
    , Svg.circle
        [ cx xx, cy yy, r mineRadius, fill "black" ]
        []
    , Svg.text_
        [ SvgA.x xx
        , SvgA.y (String.fromInt (Maybe.withDefault 0 (String.toInt y) + round (toFloat cellSize * 0.75)))
        , fontFamily "Verdana"
        , fontSize "12"
        , textAnchor "middle"
        ]
        [ Svg.text mineCount ]
    ]
