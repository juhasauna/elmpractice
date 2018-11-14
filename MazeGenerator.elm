module Main exposing (..)

import Browser
import Array
import Html exposing (..)
import Html.Events exposing (..)
import Random
import Svg exposing (..)
import Svg.Attributes as SvgA exposing (..)
import Time


dim =
    30


cellCount =
    dim ^ 2


cellSize =
    20


strCS =
    String.fromFloat cellSize


initWalls =
    Walls True True True True


type alias Walls =
    { top : Bool, right : Bool, down : Bool, left : Bool }


type alias Cell =
    { x : String, y : String, visited : Bool, walls : Walls }


defaultCell =
    { x = "0", y = "0", visited = False, walls = initWalls }


type alias Model =
    { grid : Array.Array Cell
    , current : Int
    , stack : List Int
    }

init : () -> (Model, Cmd Msg)
init _ =
    ( Model initGrid 0 [], Cmd.none )


initGrid =
    let
        xs =
            List.range 0 (dim - 1) |> List.map ((*) cellSize) |> List.map String.fromInt

        makeCells lst i =
            if i < 0 then
                lst
            else
                let
                    ys =
                        String.fromFloat (cellSize * i)

                    newList =
                        List.concat [ List.map (\x -> Cell x ys False initWalls) xs, lst ]
                in
                makeCells newList (i - 1)
    in
    Array.fromList (makeCells [] dim)


type Msg
    = Tick Time.Posix
    | NextCell Float


update msg model =
    case msg of
        Tick newTime ->
            ( model, Random.generate NextCell (Random.float 0 1) )

        NextCell nextDir ->
            let
                visitCell grid i =
                    let
                        cell =
                            getCell i grid
                    in
                    Array.set i { cell | visited = True } grid

                newGrid =
                    visitCell model.grid model.current

                next =
                    getNextCell model.current model.grid nextDir

                newGrid2 =
                    removeWalls newGrid model.current next

                ( newStack, newCurrent ) =
                    updateStack model next

                newModel =
                    { model | grid = newGrid2, current = newCurrent, stack = newStack }
            in
            ( newModel, Cmd.none )


removeWalls grid current next =
    let
        getXY index =
            let
                y_ =
                    remainderBy dim index

                x_ =
                    toFloat (index - y_) / dim
            in
            ( x_, y_ )

        ( cx, cy ) =
            getXY current

        ( nx, ny ) =
            getXY next

        ( x, y ) =
            ( nx - cx, ny - cy )

        ( cCell, nCell ) =
            ( getCell current grid, getCell next grid )

        getWalls { top, right, down, left } t0 r0 d0 l0 =
            let
                t1 =
                    if top then
                        t0
                    else
                        top

                r1 =
                    if right then
                        r0
                    else
                        right

                d1 =
                    if down then
                        d0
                    else
                        down

                l1 =
                    if left then
                        l0
                    else
                        left
            in
            Walls t1 r1 d1 l1

        cWalls =
            getWalls cCell.walls (x /= -1) (y /= 1) (x /= 1) (y /= -1)

        nWalls =
            getWalls nCell.walls (x /= 1) (y /= -1) (x /= -1) (y /= 1)

        updateCurrentCell =
            Array.set current { cCell | walls = cWalls } grid

        updateNextCell =
            Array.set next { nCell | walls = nWalls } updateCurrentCell
    in
    updateNextCell


updateStack model next =
    let
        stackLen =
            List.length model.stack

        newStack =
            if next == model.current then
                if stackLen == 0 then
                    []
                else
                    List.drop 1 model.stack
            else
                model.current :: model.stack

        current =
            if List.length newStack > stackLen then
                next
            else if stackLen > 0 then
                List.head model.stack |> Maybe.withDefault 0
            else
                model.current
    in
    ( newStack, current )


getCell i grid =
    Array.get i grid
        |> Maybe.withDefault defaultCell


getNextCell i grid dir =
    let
        neighbors =
            getNeighbors i

        notVisited =
            List.filter (\n -> not (getCell n grid).visited) neighbors

        randomNeighborIndex =
            let
                len =
                    List.length notVisited

                neighborIndex =
                    floor (toFloat len * dir)

                chosenNeighbor =
                    Array.get neighborIndex (Array.fromList notVisited) |> Maybe.withDefault -1
            in
            if chosenNeighbor == -1 then
                i
            else
                chosenNeighbor
    in
    randomNeighborIndex


-- notVisited i grid =
--     let
--         cell =
--             getCell i grid
--     in
--     not cell.visited


getNeighbors i =
    let
        neighbors =
            [ i - dim, i + 1, i + dim, i - 1 ]
                |> List.filter (\n -> n >= 0)
                |> List.filter (\n -> n < cellCount)

        checkFirstAndLastCols =
            if remainderBy dim i == 0 then
                -- first col
                List.filter (\n -> n /= i - 1) neighbors
            else if remainderBy dim i == (dim - 1) then
                -- last col
                List.filter (\n -> n /= i + 1) neighbors
            else
                neighbors
    in
    checkFirstAndLastCols


subscriptions model =
    -- Sub.none
    Sub.batch [ Time.every 1 Tick ]


view model =
    let
        gridSize =
            String.fromFloat (dim * cellSize + 1)

        f =
            makeSvg model.current

        rects =
            Array.toList (Array.map f model.grid)

        linesErased =
            List.concat (Array.toList (Array.map eraseLines model.grid))

        shapes =
            List.concat [ rects, linesErased ]
    in
    div
        []
        [ svg
            [ SvgA.width gridSize
            , SvgA.height gridSize
            ]
            shapes
        -- , Html.button [ Html.Events.onClick (Tick Time.millisecond) ] [ Html.text "Step" ]
        , Html.button [ Html.Events.onClick (Tick <| Time.millisToPosix 10) ] [ Html.text "Step" ]
        ]


getIndex sx sy =
    let
        x =
            (String.toFloat sx |> Maybe.withDefault 0) / cellSize

        y =
            (String.toFloat sy |> Maybe.withDefault 0) / cellSize

        ret =
            x + dim * y
    in
    ret


makeSvg current { x, y, visited } =
    let
        c =
            if toFloat current == getIndex x y then
                "yellow"
            else if visited then
                "purple"
            else
                "darkgrey"
    in
    Svg.rect
        [ SvgA.x x
        , SvgA.y y
        , SvgA.fill c
        , SvgA.strokeWidth "1"
        , SvgA.stroke "white"
        , SvgA.height strCS
        , SvgA.width strCS
        , SvgA.strokeOpacity "0"
        ]
        []


eraseLines cell =
    let
        { top, right, down, left } =
            cell.walls

        sx1 =
            (String.toInt cell.x |> Maybe.withDefault 0) + cellSize |> String.fromInt

        sy1 =
            (String.toInt cell.y |> Maybe.withDefault 0) + cellSize |> String.fromInt

        color =
            "black"

        sOpacity =
            "999"

        topLine =
            if not top then
                Svg.line
                    []
                    []
            else
                Svg.line
                    [ x1 sx1, y1 cell.y, x2 cell.x, y2 cell.y, stroke color, SvgA.strokeOpacity sOpacity ]
                    []

        downLine =
            if not down then
                Svg.line
                    []
                    []
            else
                Svg.line
                    [ x1 sx1, y1 sy1, x2 cell.x, y2 sy1, stroke color, SvgA.strokeOpacity sOpacity ]
                    []

        leftLine =
            if not left then
                Svg.line
                    []
                    []
            else
                Svg.line
                    [ x1 cell.x, y1 sy1, x2 cell.x, y2 cell.y, stroke color, SvgA.strokeOpacity sOpacity ]
                    []

        rightLine =
            if not right then
                Svg.line
                    []
                    []
            else
                Svg.line
                    [ x1 sx1, y1 sy1, x2 sx1, y2 cell.y, stroke color, SvgA.strokeOpacity sOpacity ]
                    []
    in
    [ topLine, rightLine, downLine, leftLine ]


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
