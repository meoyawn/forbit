#r "node_modules/fable-core/Fable.Core.dll"

open System
open Fable.Core
open Fable.Import

type Vec2 =
  | Vec2 of float * float
  static member (+) (Vec2 (x1, y1), Vec2 (x2, y2)) = Vec2 (x1 + x2, y1 + y2)
  static member (-) (Vec2 (x1, y1), Vec2 (x2, y2)) = Vec2 (x1 - x2, y1 - y2)
  static member (*) (Vec2 (x, y), n) = Vec2 (x * n, y * n)
  static member (*) (n, v) = v * n
  static member (/) (v: Vec2, n) = (1. / n) * v

let ZEROVEC = Vec2 (0., 0.)

type Body = {
    mass:     float
    position: Vec2
    velocity: Vec2
    path:     Vec2 list
}

type Universe = {
    time: float
    bodies: Body list
}

let norm (Vec2 (x, y)) = sqrt (x * x + y * y)

let normalize v = v / norm v

let setLength n v = n * normalize v

let gConst = 6.673e-11

let scale = 1.5e-9
let speedUp = 2e6

let sunEarth = 149597870700.
let earthMoon = sunEarth + 384400000.
let earthSpeed = 29.78e3
let moonSpeed = earthSpeed + (1023.056 * 0.94)

let bodies = [
  {mass = 1.989e30; position = Vec2 (0., 0.);        velocity = Vec2 (0., 0.);         path = []}
  {mass = 5.972e24; position = Vec2 (sunEarth, 0.);  velocity = Vec2 (0., earthSpeed); path = []}    
  {mass = 7.348e22; position = Vec2 (earthMoon, 0.); velocity = Vec2 (0., moonSpeed);  path = []}
  {mass = 6.4171e23; position = Vec2 (1.524 * sunEarth, 0.); velocity = Vec2 (0., 24.077e3);  path = []}
]

let gravity { mass = m1; position = p1 } { mass = m2; position = p2 } = 
  let diff = p2 - p1
  let r = norm diff  
  let len = gConst * m1 * m2 / (r * r)
  setLength len diff

let acceleration mass (force: Vec2) = force / mass

let allForces b bs =
  List.filter (fun x -> x.position <> b.position) bs
  |> List.map (gravity b) 
  |> List.fold (+) ZEROVEC

let nextBody {mass = m; position = p; velocity = v; path = oldPath} dt (accel: Vec2) = 
  let pos = p + v * dt
  let newPath = pos :: oldPath |> List.truncate 10000
  {mass = m; position = pos; velocity = v + accel * dt; path = newPath}

let nextBodies bs dt =
  List.map (fun b -> allForces b bs |> acceleration b.mass |> nextBody b dt) bs

let canvas =  Browser.document.getElementsByTagName_canvas().[0]
canvas.width <- canvas.offsetWidth
canvas.height <- canvas.offsetHeight

let ctx = canvas.getContext_2d()

let canvasPos (Vec2 (x, y)) =
  let cw = canvas.width
  let ch = canvas.height

  let cx = x * scale + cw / 2.
  let cy = ch / 2. - y * scale

  Vec2 (cx, cy)

let draw (ctx: Browser.CanvasRenderingContext2D) {mass = m; position = pos; path = path} =
  ctx.beginPath()

  let cPath = List.map canvasPos path

  match cPath with
    | Vec2(startX, startY) :: _ -> ctx.moveTo(startX, startY)
    | _                         -> ()

  for Vec2(lx, ly) in List.skip 1 cPath do
    ctx.lineTo(lx, ly)

  ctx.stroke()

  let (Vec2 (cx, cy)) = canvasPos pos
  let rad = m * 5e-29
  let start = 0.
  let finish = 2. * System.Math.PI

  ctx.beginPath()
  ctx.arc (cx, cy, rad, start, finish, false)
  ctx.fillStyle <- U3.Case1 "green"
  ctx.fill()

let rec loop {time = prev; bodies = bs} (next: float) =
  let diff = (next - prev) * 1e-3 * speedUp
  let newBodies = nextBodies bs diff
  let step = {time = next; bodies = newBodies }
  Browser.window.requestAnimationFrame (Func<_, _> (loop step)) |> ignore

  ctx.clearRect (0., 0., canvas.width, canvas.height)
  for b in bs do
    draw ctx b

loop {time = 0.; bodies = bodies} 0.
