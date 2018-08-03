module Main exposing (..)

import Array
import Html exposing (..)
import Html.Events exposing (..)
import Random
import Svg exposing (..)
import Svg.Attributes as SvgA exposing (..)
import Time exposing (Time)


dim : Int
dim =
    40


cellCount : Int
cellCount =
    dim ^ 2


obstacleFrequency : Float
obstacleFrequency =
    0.3


cellSize : Int
cellSize =
    10


strCS : String
strCS =
    toString cellSize


start =
    let
        mid =
            cellCount // 2

        leftMidEdge =
            mid - rem mid dim
    in
    leftMidEdge


goal =
    -- cellCount - 1
    start + dim - 1


type alias Fgh =
    { f : Int, g : Int, h : Int }


initFgh =
    Fgh 0 0 0


type alias Cell =
    { i : Int, obstacle : Bool, previous : Int, fgh : Fgh }


defaultCell : Cell
defaultCell =
    { i = start, obstacle = False, previous = -1, fgh = initFgh }


type alias Model =
    { grid : Array.Array Cell
    , current : Int
    , done : Bool
    , openSet : List Int
    , closedSet : List Int
    }


init : ( Model, Cmd Msg )
init =
    ( Model initGrid start False [ start ] [], Random.generate AddObstacles (Random.list (cellCount - 1) (Random.float 0 1)) )


initGrid : Array.Array Cell
initGrid =
    let
        cells =
            List.range 0 (cellCount - 1)
                |> List.map (\n -> { defaultCell | i = n })
                |> Array.fromList

        startCell =
            getCell start cells

        h =
            dist start goal

        f =
            h * 4 + 1

        initStartCell =
            Array.set start { startCell | fgh = Fgh f 0 h } cells
    in
    initStartCell


type Msg
    = Tick Time
    | AddObstacles (List Float)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            let
                next =
                    getNextCell start model.grid model.openSet

                newModel =
                    let
                        done =
                            if model.current == goal then
                                True
                            else if List.length model.openSet == 0 then
                                True
                            else
                                model.done

                        updateModel =
                            if done then
                                { model | done = done }
                            else
                                { model
                                    | current = next
                                }
                                    |> updateCurrentNeighbors
                    in
                    updateModel
            in
            ( newModel, Cmd.none )

        AddObstacles randFloats ->
            let
                addObstacles grid floats =
                    let
                        floatsLeft =
                            List.length floats
                    in
                    if floatsLeft == 0 then
                        grid
                    else
                        let
                            newGrid =
                                if (List.head floats |> Maybe.withDefault 0) < obstacleFrequency then
                                    let
                                        remainder =
                                            rem floatsLeft dim

                                        notInFirstOrLastColumn =
                                            remainder /= 0 && remainder /= (dim - 1)

                                        cell =
                                            getCell floatsLeft grid
                                    in
                                    Array.set floatsLeft { cell | obstacle = notInFirstOrLastColumn } grid
                                else
                                    grid
                        in
                        addObstacles newGrid (List.drop 1 floats)

                newGrid =
                    addObstacles model.grid randFloats
            in
            ( { model | grid = newGrid }, Cmd.none )


getRowAndCol : Int -> { row : Int, col : Int }
getRowAndCol i =
    { row = i // dim, col = rem i dim }


dist : Int -> Int -> Int
dist i j =
    let
        ( a, b ) =
            ( getRowAndCol i, getRowAndCol j )
    in
    abs (b.row - a.row) + abs (b.col - a.col)


getCell : Int -> Array.Array Cell -> Cell
getCell i arr =
    Array.get i arr |> Maybe.withDefault defaultCell


filterListFromList : List Int -> List Int -> List Int
filterListFromList lst filt =
    if List.length lst == 0 || List.length filt == 0 then
        lst
    else
        let
            head =
                List.head filt |> Maybe.withDefault -1

            newLst =
                List.filter (\n -> not (n == head)) lst
        in
        filterListFromList newLst (List.drop 1 filt)


updateCurrentNeighbors : Model -> Model
updateCurrentNeighbors model =
    let
        neighbors =
            getNeighbors model.current |> List.filter (\n -> not (getCell n model.grid).obstacle)

        active =
            filterListFromList neighbors model.closedSet

        newActive =
            filterListFromList active model.openSet

        notNewActive =
            filterListFromList active newActive

        newOpenSet =
            List.concat [ model.openSet, newActive ] |> List.filter (\n -> not (n == model.current))

        newClosedSet =
            model.current :: model.closedSet

        updateGrid grid activeLst current =
            if List.length activeLst == 0 then
                grid
            else
                let
                    head =
                        List.head activeLst |> Maybe.withDefault -1

                    ( currentCell, neighborCell ) =
                        ( getCell current grid, getCell head grid )

                    g =
                        currentCell.fgh.g + 1

                    newNeighborCell =
                        if List.member head model.openSet then
                            if g < neighborCell.fgh.g then
                                let
                                    oldFgh =
                                        neighborCell.fgh

                                    newFgh =
                                        { oldFgh | g = g }
                                in
                                { neighborCell | fgh = newFgh }
                            else
                                neighborCell
                        else
                            let
                                h =
                                    dist head goal

                                f =
                                    g + h
                            in
                            { neighborCell | previous = current, fgh = Fgh f g h }

                    newGrid =
                        Array.set head newNeighborCell grid
                in
                updateGrid newGrid (List.drop 1 activeLst) current

        newGrid =
            updateGrid model.grid active model.current
    in
    { model | openSet = newOpenSet, closedSet = newClosedSet, grid = newGrid }


getNextCell winner grid openLst =
    if List.length openLst == 0 then
        winner
    else
        let
            nextCandidate =
                List.head openLst |> Maybe.withDefault start

            ( winnerF, testF ) =
                ( (getCell winner grid).fgh.f, (getCell nextCandidate grid).fgh.f )

            newWinner =
                if winnerF > testF then
                    nextCandidate
                else
                    winner
        in
        getNextCell newWinner grid (List.drop 1 openLst)


getNeighbors : Int -> List Int
getNeighbors i =
    let
        neighbors =
            [ i - dim, i + 1, i + dim, i - 1 ]
                |> List.filter (\n -> n >= 0)
                |> List.filter (\n -> n < cellCount)

        removeEdges =
            if rem i dim == 0 then
                List.filter (\n -> n /= i - 1) neighbors
            else if rem i dim == (dim - 1) then
                List.filter (\n -> n /= i + 1) neighbors
            else
                neighbors
    in
    removeEdges


subscriptions : Model -> Sub Msg
subscriptions model =
    -- Sub.none
    if model.done then
        Sub.none
    else
        Sub.batch [ Time.every (10 * Time.millisecond) Tick ]


view : Model -> Html Msg
view model =
    let
        gridSize =
            toString (dim * cellSize + 1)

        rects =
            Array.toList (Array.map (makeGridSvg model) model.grid)

        shapes =
            List.concat [ rects, makePathSvg model ]
    in
    div
        []
        [ svg
            [ SvgA.width gridSize
            , SvgA.height gridSize
            ]
            shapes
        , Html.button [ Html.Events.onClick (Tick Time.millisecond) ] [ Html.text "Step" ]
        ]


makePathSvg : Model -> List (Svg msg)
makePathSvg model =
    let
        makePath i lst grid =
            if i == start then
                i :: lst
            else
                let
                    newLst =
                        i :: lst

                    next =
                        (getCell i grid).previous
                in
                makePath next newLst grid

        points =
            makePath model.current [] model.grid

        makePathDot p =
            let
                getMiddle index =
                    (index * cellSize + cellSize // 2) |> toString

                point =
                    getRowAndCol p

                ( x, y ) =
                    ( getMiddle point.col, getMiddle point.row )
            in
            Svg.circle
                [ cx x, cy y, r "2", fill "white" ]
                []

        getSvgPath =
            List.map makePathDot points
    in
    getSvgPath


makeGridSvg : Model -> Cell -> Svg msg
makeGridSvg model cell =
    let
        ( visited, open ) =
            ( List.member cell.i model.closedSet, List.member cell.i model.openSet )

        point =
            getRowAndCol cell.i

        makeScaledString index =
            (index * cellSize) |> toString

        ( x, y ) =
            ( makeScaledString point.col, makeScaledString point.row )

        c =
            if cell.obstacle then
                "black"
            else if cell.i == model.current then
                "yellow"
            else if cell.i == goal then
                "blue"
            else if open then
                "teal"
            else if visited then
                "green"
            else
                "white"
    in
    Svg.rect
        [ SvgA.x x
        , SvgA.y y
        , SvgA.fill c
        , SvgA.strokeWidth "1"
        , SvgA.stroke "black"
        , SvgA.height strCS
        , SvgA.width strCS
        ]
        []


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
