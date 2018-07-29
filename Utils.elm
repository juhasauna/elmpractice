module Utils exposing (..)

import Math.Vector2 as V2 exposing (..)


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
