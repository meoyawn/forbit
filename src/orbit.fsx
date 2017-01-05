#r "../node_modules/fable-core/Fable.Core.dll"

open Fable.Core
open Fable.Import.Browser

[<Struct; StructuralEquality; StructuralComparison>]
type Vec2 = 
  Vec2 of float * float

    static member zero = Vec2 (0., 0.)

    static member ( + ) (Vec2 (x1, y1), Vec2 (x2, y2)) = 
      Vec2 (x1 + x2, y1 + y2)

    static member ( - ) (Vec2 (x1, y1), Vec2 (x2, y2)) = 
      Vec2 (x1 - x2, y1 - y2)

    static member ( * ) (Vec2 (x, y), n) = 
      Vec2 (x * n, y * n)

    static member ( / ) (Vec2 (x, y), n) = 
      Vec2 (x / n, y / n)

    static member ( * ) (n, Vec2 (x, y)) = 
      Vec2 (x * n, y * n)

[<Struct; StructuralEquality; StructuralComparison>]
type Body = {
    mass:     float
    position: Vec2
    velocity: Vec2
}

[<Struct; StructuralEquality; StructuralComparison>]
type Universe = {
    time: float
    bodies: Body list
}

let norm (Vec2 (x, y)) = 
  sqrt (x * x + y * y)

let distance a b = 
  norm (b - a)

let normalize v = 
  v * (1. / norm v)

let setLength n v = 
  n * normalize v

let gConst = 6.67384808080e-11

let gravity { mass = m1; position = p1 } { mass = m2; position = p2 } = 
  if p1 = p2 then
    Vec2.zero
  else
    let diff = p2 - p1
    let r    = norm diff
    let len  = gConst * m1 * m2 / (r * r)
    setLength len diff

let acceleration (force: Vec2) mass =
  force / mass

let canvas =  document.getElementsByTagName_canvas().[0]
canvas.width <- 1000.
canvas.height <- 800.

let ctx = canvas.getContext_2d()

ctx.fillStyle <- U3.Case1 "rgb(200,0,0)"
ctx.fillRect (10., 10., 55., 50.)

ctx.fillStyle <- U3.Case1 "rgba(0, 0, 200, 0.5)"
ctx.fillRect (30., 30., 55., 50.)

let x = Vec2 (1., 2.) + Vec2 (3., 4.)
