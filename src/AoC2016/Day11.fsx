#r "../../bin/Debug/net8.0/FParsecCS.dll"
#r "../../bin/Debug/net8.0/FParsec.dll"
#r "../../bin/Debug/net8.0/FSharpx.Collections.dll"
#r "../../bin/Debug/net8.0/FSharpx.Extras.dll"
#load "Library.fs"

open FParsec
open FParsec.Primitives
open Library
open FParsecOp
open FSharpx.Collections
open FSharpx.TimeMeasurement
open FSharpx.Functional

#load "Day11.fs"
open AoC.Day11

let example = """
The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.
The second floor contains a hydrogen generator.
The third floor contains a lithium generator.
The fourth floor contains nothing relevant.
"""

let inputPuzzleStr1 = """
The first floor contains a polonium generator, a thulium generator, a thulium-compatible microchip, a promethium generator, a ruthenium generator, a ruthenium-compatible microchip, a cobalt generator, and a cobalt-compatible microchip.
The second floor contains a polonium-compatible microchip and a promethium-compatible microchip.
The third floor contains nothing relevant.
The fourth floor contains nothing relevant.
"""

let inputPuzzleStr2 = """
The first floor contains an elerium generator, an elerium-compatible microchip, a dilithium generator, a dilithium-compatible microchip, a polonium generator, a thulium generator, a thulium-compatible microchip, a promethium generator, a ruthenium generator, a ruthenium-compatible microchip, a cobalt generator, and a cobalt-compatible microchip.
The second floor contains a polonium-compatible microchip and a promethium-compatible microchip.
The third floor contains nothing relevant.
The fourth floor contains nothing relevant.
"""

// Vedran's puzzles
let vedranPuzzleStr1 = """
The first floor contains a promethium generator and a promethium-compatible microchip.
The second floor contains a cobalt generator, a curium generator, a ruthenium generator, and a plutonium generator.
The third floor contains a cobalt-compatible microchip, a curium-compatible microchip, a ruthenium-compatible microchip, and a plutonium-compatible microchip.
The fourth floor contains nothing relevant.
"""

let vedranPuzzleStr2 = """
The first floor contains a promethium generator, a promethium-compatible microchip, an elerium generator, an elerium-compatible microchip, a dilithium generator, and a dilithium-compatible microchip.
The second floor contains a cobalt generator, a curium generator, a ruthenium generator, and a plutonium generator.
The third floor contains a cobalt-compatible microchip, a curium-compatible microchip, a ruthenium-compatible microchip, and a plutonium-compatible microchip.
The fourth floor contains nothing relevant.
"""

time (fun () -> computeSteps example)
time (fun () -> computeSteps inputPuzzleStr1)
time (fun () -> computeSteps inputPuzzleStr2)

time (fun () -> computeSteps vedranPuzzleStr1)
// 00:00:00.2460000
time (fun () -> computeSteps vedranPuzzleStr2)
// 00:00:02.0450000

// Slower Path hash
let computeStepHash1 = parser >> mkPath >> computeSteps' hash1
time (fun () -> computeStepHash1 vedranPuzzleStr2)
// 00:00:03.5660000

// Part1: 33
// Elapsed: 00:00:00.7342373

// Part2: 57
// Elapsed: 00:00:03.4661911
