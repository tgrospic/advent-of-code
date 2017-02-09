#load "Library.fs"
#r "../packages/FParsec/lib/net40-client/FParsecCS.dll"
#r "../packages/FParsec/lib/net40-client/FParsec.dll"

open FParsec
open FParsec.Primitives
open Library

let case = [
  "A(1x5)BC", "ABBBBBC"
  "(3x3)XYZ", "XYZXYZXYZ"
  "A(2x2)BCD(2x2)EFG", "ABCBCDEFEFG"
  "(6x1)(1x3)A", "(1x3)A"
]

let charsToString (chs : char list) =
  chs |> List.fold (fun a x -> a + (x.ToString())) ""
let strJoin (xs : string list) =
  xs |> List.fold (+) ""

let (<*>) f x = f >>= fun f' -> x >>= fun x' -> preturn (f' x')
let (<!>) f x = preturn f <*> x
let (<**>) = (.>>.)
let ( *>) = (>>.)
let (<* ) = (.>>)

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

let grammar = many expr

let r =
  case
  |> List.map (fst >> run grammar)
  
let read () =
  let inputPath = __SOURCE_DIRECTORY__ + @"./puzzles/input-2016-day09.txt"
  let input = readFile inputPath
  run (strJoin <!> grammar) input

let res =
  match read() with
  | Success (x, _, _) -> "Result", String.length x
  | Failure (s, e, _) -> sprintf "ERROR: %s - %A" s e, 0

module Test =
  run (pTxt <**> pCode) "ABC(2x2)BCD(2x2)EFG"
  run (many1 expr) ""
  run (((+) "") <!> expr) "D(3x3)ABCD"
  run expr "(3x3)ABCD"
  run (many expr) "GG(2x2)BCD(3x2)URD"
