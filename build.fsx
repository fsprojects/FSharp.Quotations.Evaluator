// --------------------------------------------------------------------------------------
// FAKE build script 
// --------------------------------------------------------------------------------------

//#r @"packages/build/FAKE/tools/NuGet.Core.dll"
#r @"packages/build/FAKE/tools/FakeLib.dll"
open Fake
open Fake.Git
open Fake.AssemblyInfoFile
open Fake.ReleaseNotesHelper
open System
open System.IO


// --------------------------------------------------------------------------------------
// START TODO: Provide project-specific details below
// --------------------------------------------------------------------------------------

// Information about the project are used
//  - for version and project name in generated AssemblyInfo file
//  - by the generated NuGet package 
//  - to run tests and to publish documentation on GitHub gh-pages
//  - for documentation, you also need to edit info in "docs/tools/generate.fsx"

// The name of the project 
// (used by attributes in AssemblyInfo, name of a NuGet package and directory in 'src')
let project = "FSharp.Quotations.Evaluator"

// Short summary of the project
// (used as description in AssemblyInfo and as a short summary for NuGet package)
let summary = "A quotations evaluator for F# based on LINQ expression tree compilation"

// Longer description of the project
// (used as a description for NuGet package; line breaks are automatically cleaned up)
let description = "A quotations evaluator for F# based on LINQ expression tree compilation. Some constructs are not supported and performance may be slower than F# compiled code.  Based on the old F# 2.0 PowerPack code."

// List of author names (for NuGet package)
let authors = [ "F# Software Foundation" ]

// Tags for your project (for NuGet package)
let tags = "FSharp F# Quotations Evaluator"

// File system information 
let solutionFile  = "FSharp.Quotations.Evaluator.sln"

// Pattern specifying assemblies to be tested using NUnit
let testProjects = "tests/**/*.??proj"

// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted 
let gitHome = "https://github.com/fsprojects"

// The name of the project on GitHub
let gitName = "FSharp.Quotations.Evaluator"

// The url for the raw files hosted
let gitRaw = environVarOrDefault "gitRaw" "https://raw.github.com/fsprojects"

// --------------------------------------------------------------------------------------
// END TODO: The rest of the file includes standard build steps 
// --------------------------------------------------------------------------------------

// Read additional information from the release notes document
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let configuration = environVarOrDefault "Configuration" "Release"
let artifactsDir = "artifacts"
let release = parseReleaseNotes (IO.File.ReadAllLines "RELEASE_NOTES.md")

// --------------------------------------------------------------------------------------
// Clean build results & restore NuGet packages

Target "Clean" (fun _ ->
    CleanDirs ["artifactsDir"; "docs/output"]
)

// --------------------------------------------------------------------------------------
// Build library & test project

Target "Build" (fun _ ->
    DotNetCli.Build(fun c ->
        { c with
            Project = __SOURCE_DIRECTORY__
            Configuration = configuration
            AdditionalArgs = [ sprintf "-p:Version=%s" release.NugetVersion ]
        })
)

// --------------------------------------------------------------------------------------
// Run the unit tests using test runner

Target "RunTests" (fun _ ->
    DotNetCli.Test (fun c ->
        { c with
            Project = __SOURCE_DIRECTORY__
            Configuration = configuration
            AdditionalArgs = [ "--no-build" ] })
)

// --------------------------------------------------------------------------------------
// Build a NuGet package

Target "NuGet" (fun _ ->
    DotNetCli.Pack (fun c ->
        { c with
            Project = __SOURCE_DIRECTORY__
            OutputPath = artifactsDir
            Configuration = configuration })
)

Target "NuGetPush" (fun _ ->
    Paket.Push (fun p -> 
        { p with WorkingDir = "nuget" })
)

// --------------------------------------------------------------------------------------
// Generate the documentation

Target "GenerateDocs" (fun _ ->
    // ugh, still need to use mono and legacy fsi for FSharp.Formatting
    let path = __SOURCE_DIRECTORY__ @@ "packages/build/FSharp.Compiler.Tools/tools/fsi.exe"
    let workingDir = "docs/tools"
    let args = "--define:RELEASE generate.fsx"
    let command, args = 
        if EnvironmentHelper.isWindows then path, args
        else "mono", sprintf "%s %s" path args

    if Shell.Exec(command, args, workingDir) <> 0 then
        failwith "failed to generate docs"
)

// --------------------------------------------------------------------------------------
// Release Scripts

Target "ReleaseDocs" (fun _ ->
    let tempDocsDir = "temp/gh-pages"
    //CleanDir tempDocsDir
    if not (System.IO.Directory.Exists tempDocsDir) then 
        Repository.cloneSingleBranch "" (gitHome + "/" + gitName + ".git") "gh-pages" tempDocsDir

    fullclean tempDocsDir
    CopyRecursive "docs/output" tempDocsDir true |> tracefn "%A"
    StageAll tempDocsDir
    Commit tempDocsDir (sprintf "Update generated documentation for version %s" release.NugetVersion)
    Branches.push tempDocsDir
)

Target "ReleaseTag" (fun _ ->
    StageAll ""
    Commit "" (sprintf "Bump version to %s" release.NugetVersion)
    Branches.push ""

    Branches.tag "" release.NugetVersion
    Branches.pushTag "" "origin" release.NugetVersion
)

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target "Default" DoNothing
Target "Bundle" DoNothing
Target "Release" DoNothing

"Clean"
  ==> "Build"
  ==> "RunTests"
  ==> "Default"

"Default"
  ==> "GenerateDocs"
  ==> "NuGet"
  ==> "Bundle"

"Bundle"
  ==> "ReleaseDocs"
  ==> "NuGetPush"
  ==> "ReleaseTag"
  ==> "Release"

RunTargetOrDefault "Default"