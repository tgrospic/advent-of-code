(*
  AoC - Day 01

  http://adventofcode.com/2016/day/1
*)
module AoC.Day01

open Util
open FSharpx.Text.Regex
open System.Text.RegularExpressions

// PART 1

type LR   = L of int | R of int
type Face = N | S | E | W

let walker (x, y, face) lr =
  match face, lr with
  | N, L n
  | S, R n -> x - n, y    , W
  | S, L n
  | N, R n -> x + n, y    , E
  | E, L n
  | W, R n -> x    , y + n, N
  | W, L n
  | E, R n -> x    , y - n, S

let patternLR = @"([LR])([\d]+)"

let parseLR = function
  | Match RegexOptions.None patternLR { GroupValues = [dir; num] } when dir = "L" -> L <| int num
  | Match RegexOptions.None patternLR { GroupValues = [dir; num] } when dir = "R" -> R <| int num
  | _ -> failwith "Hey! This is not fair!"

let parser (str : string) =
  str.Split(',')
  |> Seq.map parseLR
  |> Seq.toList

let doWalking f s = parser >> f s

let toXY (x, y, _) = (x, y)

let calcDistance (x, y) = abs x + abs y

let start = 0, 0, N

// PART 1 answer: distance at the end
let distance input = doWalking (List.fold walker) start input |> toXY |> calcDistance

// PART 2

type Point = int * int
type Line  = Point * Point

//E = B-A = ( Bx-Ax, By-Ay )
//F = D-C = ( Dx-Cx, Dy-Cy )
//P = ( -Ey, Ex )
//h = ( (A-C) * P ) / ( F * P )

let (.*) f (x, y) = f (y, x)
let (<|>) x y =
  match x with
  | Some _ -> x
  | None -> y

let bothPlanes f (x1, x2) (y1, y2) =
  f x1 x2 y1 y2 <|> f .* x1 .* x2 .* y1 .* y2

let checkX x1 _ y1 y2 =
  let ch z1 z2 = fst z1 < fst x1 && fst x1 < fst z2
  ch y1 y2 || ch y2 y1

let intersectX x1 x2 y1 y2 =
  if
    fst x1 = fst x2
    && fst y1 <> fst y2
    && checkX x1 x2 y1 y2
    // Flip x and y
    && checkX .* y1 .* y2 .* x1 .* x2
  then Some (fst x1, snd y1)
  else None

let intersectLineLine = bothPlanes intersectX

let intersect (pastLines: Line list, points: Point list) a =
  if List.isEmpty pastLines
  then a :: pastLines, points
  else
    let intersectedPoints =
      pastLines
      |> List.rev
      |> List.map (intersectLineLine a)
      |> List.choose id
    a :: pastLines, points @ intersectedPoints

// PART 2 answer: first location visited twice
let firstIntersectionDistance input =
  doWalking (List.scan walker) start input
  |> (List.map toXY >> List.pairwise)
  |> List.fold intersect ([], [])
  |> snd
  |> List.head
  |> calcDistance

