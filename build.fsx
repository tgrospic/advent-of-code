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

Target "Test" (fun _ ->
  if Shell.Exec "./build/AoC.2016.exe" <> 0
  then failwith "Test failed!"
)

Target "Default" (fun _ ->
  Run "Build"
)

// Dependencies
"Build"
  ==> "Test"

"Clean"
  ==> "Build"
  ==> "Default"

// start build
RunTargetOrDefault "Default"
