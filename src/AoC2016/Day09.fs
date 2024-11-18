(*
  AoC - Day 9

  http://adventofcode.com/2016/day/9
*)
module AoC.Day09

open Util
open FParsec
open FParsecOp

let strJoin (xs : string list) =
  xs |> List.fold (+) ""
let strLen64 = String.length >> int64

let result = function
  | Success (x, _, _) -> x
  | Failure (s, _, _) -> failwith s

// PART 1

let pword  = manySatisfy System.Char.IsLetter
let pword1 = many1Satisfy System.Char.IsLetter

let pIntPair = pint32 <* pchar 'x' <**> pint32

let pCode = pchar '(' *> pIntPair <* pchar ')'

let mkSubP (x, y) = List.replicate y >> strJoin <!> anyString x

let pCodeExpr = pCode >>= mkSubP

let expr = pword1 <|> ((+) <!> pCodeExpr <*> pword)

let grammar = strJoin >> strLen64 <!> many expr

// PART 2

let pStrLen  = strLen64 <!> pword
let pStr1Len = strLen64 <!> pword1

let rec mkSubPLen (x, y) =
  let subLength str =
    match run grammarLen str with
    | Success (n, _, _) when n > 0L -> int64 y * n
    | _ -> int64 y * strLen64 str
  // Parse subexpression
  subLength <!> anyString x

and pCodeLen = pCode >>= mkSubPLen

and exprLen = pStr1Len <|> ((+) <!> pCodeLen <*> pStrLen)

and grammarLen = List.sum <!> many exprLen

// Example run parser
let example    = run grammar "A(2x2)BCD(2x2)EFG"
let exampleLen = run grammarLen "A(2x2)BCD(2x2)EFG"
