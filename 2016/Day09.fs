(*
  AoC - Day 9

  http://adventofcode.com/2016/day/9
*)
module AoC.Day09

open Library
open FParsec
open FParsecOp

let charsToString (chs : char list) =
  chs |> List.fold (fun a x -> a + (x.ToString())) ""
let strJoin (xs : string list) =
  xs |> List.fold (+) ""

// PART 1

let pIntPair = pint32 <* pchar 'x' <**> pint32

let subCode (x, y) =
  (fun dup ->
    let aa = List.init y (fun _ -> dup)
    strJoin aa
  ) <!> anyString x

let pCode = pchar '(' *> pIntPair <* pchar ')'

let pCodeExpr = pCode >>= subCode

let pTxt  = charsToString <!> many letter
let pTxt1 = charsToString <!> many1 letter

let expr =
  pTxt1 <|> ((fun (x, y) -> x + y) <!> (pCodeExpr <**> pTxt))

let grammar : Parser<string list, unit> = many expr

let example = run grammar "A(2x2)BCD(2x2)EFG"
