module Util

open System.IO
open System.Text.RegularExpressions
open FSharpx.TimeMeasurement

module FParsecOp =
  open FParsec

  let (<*>) f x = f >>= fun f' -> x >>= fun x' -> preturn (f' x')
  let (<!>) f x = preturn f <*> x
  let (<**>) = (.>>.)
  let ( *>) = (>>.)
  let (<* ) = (.>>)

module Arrow =
  let inline split x = (x, x)
  let inline combine f (x, y) = f x y
  let inline first f (a, b) = (f a, b)
  let inline second f (a, b) = (a, f b)
  let inline onTuple f g = first f >> second g
  let inline onSingle f g = split >> (onTuple f g)
  let inline onSingleCombine op f g = (onSingle f g) >> combine op
  let (.***.) = onTuple
  let (.&&&.) = onSingle

let readFile p = File.ReadAllText(p)

let time f = stopTime f |> Arrow.second (System.TimeSpan.FromMilliseconds >> sprintf "%A")
