// include Fake lib
#r @"packages/build/FAKE/tools/FakeLib.dll"
open Fake

// Properties
let buildDir = "./build/"

// Targets
Target "Clean" (fun _ ->
  CleanDirs [buildDir]
)

Target "Build" (fun _ ->
  !! "AdventOfCode.sln"
  |> MSBuildDebug "build" "Build"
  |> Log "AppBuild-Output: "
)

Target "Default" (fun _ ->
  Run "Build"
)

// Dependencies
"Clean"
  ==> "Build"
  ==> "Default"

// start build
RunTargetOrDefault "Default"
