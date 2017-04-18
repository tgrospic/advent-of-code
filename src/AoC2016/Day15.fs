(*
  AoC - Day 15

  http://adventofcode.com/2016/day/15
*)
module AoC.Day15

open Library

let isSlot time disks =
  let isDiskSlot i (max, pos) = (pos + 1 + i + time) % max = 0
  disks
  |> List.mapi isDiskSlot
  |> List.forall id

let scanSlots disks =
  let f x = (x, isSlot x disks)
  (Seq.initInfinite id)
  |> Seq.map f

let findFirstSlot input =
  scanSlots input
  |> Seq.filter snd
  |> Seq.head
  |> fst
