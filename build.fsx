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
let release = parseReleaseNotes (IO.File.ReadAllLines "RELEASE_NOTES.md")

// Generate assembly info files with the right version & up-to-date information
Target "AssemblyInfo" (fun _ ->
    let getAssemblyInfoAttributes projectName =
      [ Attribute.Title (projectName)
        Attribute.Product project
        Attribute.Description summary
        Attribute.Version release.AssemblyVersion
        Attribute.FileVersion release.AssemblyVersion ] 

    let getProjectDetails projectPath =
        let projectName = System.IO.Path.GetFileNameWithoutExtension projectPath
        ( projectPath,
          projectName,
          System.IO.Path.GetDirectoryName projectPath,
          getAssemblyInfoAttributes projectName
        )

    !! "src/**/*.??proj"
    |> Seq.map getProjectDetails
    |> Seq.iter (fun (projFileName, projectName, folderName, attributes) ->
        match projFileName with
        | Fsproj -> CreateFSharpAssemblyInfo (folderName </> "AssemblyInfo.fs") attributes
        | Csproj -> CreateCSharpAssemblyInfo ((folderName </> "Properties") </> "AssemblyInfo.cs") attributes
        | Vbproj -> CreateVisualBasicAssemblyInfo ((folderName </> "My Project") </> "AssemblyInfo.vb") attributes
        | Shproj -> ()
        )
)

// --------------------------------------------------------------------------------------
// Clean build results & restore NuGet packages

Target "Clean" (fun _ ->
    CleanDirs ["bin"; "temp"]
)

Target "CleanDocs" (fun _ ->
    CleanDirs ["docs/output"]
)

// --------------------------------------------------------------------------------------
// Build library & test project

Target "DotNetRestore" (fun _ -> DotNetCli.Restore id)

Target "Build" (fun _ ->
    !! solutionFile
    |> MSBuild "" "Build" ["Configuration", "Release"; "SourceLinkCreate", "true"]
    |> ignore
)

// --------------------------------------------------------------------------------------
// Run the unit tests using test runner

Target "RunTests" (fun _ ->
    for proj in !! testProjects do
        DotNetCli.Test (fun c ->
            { c with
                Project = proj
                Configuration = "Release"
                Framework =
                    if EnvironmentHelper.isWindows then c.Framework
                    else "netcoreapp2.0" })
)

// --------------------------------------------------------------------------------------
// Build a NuGet package

Target "NuGet" (fun _ ->
    Paket.Pack (fun p -> 
        { p with
            OutputPath = "nuget"
            Version = release.NugetVersion
            ReleaseNotes = toLines release.Notes })
)

Target "NuGetPush" (fun _ ->
    Paket.Push (fun p -> 
        { p with WorkingDir = "nuget" })
)

// --------------------------------------------------------------------------------------
// Generate the documentation

Target "GenerateDocs" (fun _ ->
    executeFSIWithArgs "docs/tools" "generate.fsx" ["--define:RELEASE"] [] |> ignore
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

Target "All" DoNothing
Target "BuildPackage" DoNothing
Target "Release" DoNothing

"Clean"
  ==> "AssemblyInfo"
  ==> "DotNetRestore"
  ==> "Build"
  ==> "RunTests"
  ==> "All"

"All"
  ==> "CleanDocs"
  ==> "GenerateDocs"
  ==> "NuGet"
  ==> "BuildPackage"

"BuildPackage"
  ==> "ReleaseDocs"
  ==> "NuGetPush"
  ==> "ReleaseTag"
  ==> "Release"

RunTargetOrDefault "All"