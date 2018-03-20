// --------------------------------------------------------------------------------------
// FAKE build script 
// --------------------------------------------------------------------------------------

#r @"packages/FAKE/tools/NuGet.Core.dll"
#r @"packages/FAKE/tools/FakeLib.dll"
open Fake
open Fake.Git
open Fake.AssemblyInfoFile
open Fake.ReleaseNotesHelper
open System
open System.IO
#if MONO
#else
#load "packages/SourceLink.Fake/tools/Fake.fsx"
open SourceLink
#endif


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
let testAssemblies = "tests/**/bin/Release/*Tests*.dll"

// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted 
let gitHome = "https://github.com/fsprojects"

// The name of the project on GitHub
let gitName = "FSharp.Quotations.Evaluator"

// The url for the raw files hosted
let gitRaw = environVarOrDefault "gitRaw" "https://raw.github.com/fsprojects"

let dotnetcliVersion = "1.0.4"

let dotnetSDKPath = System.Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData) </> "dotnetcore" |> FullName

let mutable dotnetExePath = "dotnet"

// --------------------------------------------------------------------------------------
// END TODO: The rest of the file includes standard build steps 
// --------------------------------------------------------------------------------------

// Read additional information from the release notes document
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let release = parseReleaseNotes (IO.File.ReadAllLines "RELEASE_NOTES.md")

let genFSAssemblyInfo (projectPath) = 
    let projectName = System.IO.Path.GetFileNameWithoutExtension(projectPath) 
    let basePath = "src/" + projectName 
    let fileName = basePath + "/AssemblyInfo.fs"
    CreateFSharpAssemblyInfo fileName
      [ Attribute.Title (projectName)
        Attribute.Product project
        Attribute.Description summary
        Attribute.Version release.AssemblyVersion
        Attribute.FileVersion release.AssemblyVersion ] 

let genCSAssemblyInfo (projectPath) = 
    let projectName = System.IO.Path.GetFileNameWithoutExtension(projectPath) 
    let basePath = "src/" + projectName + "/Properties"
    let fileName = basePath + "/AssemblyInfo.cs"
    CreateCSharpAssemblyInfo fileName
      [ Attribute.Title (projectName)
        Attribute.Product project
        Attribute.Description summary
        Attribute.Version release.AssemblyVersion
        Attribute.FileVersion release.AssemblyVersion ] 

Target "InstallDotNetCore" (fun _ ->
    dotnetExePath <- DotNetCli.InstallDotNetSDK dotnetcliVersion
)

let assertExitCodeZero x = 
    if x = 0 then () else 
    failwithf "Command failed with exit code %i" x

let runCmdIn workDir exe = 
    Printf.ksprintf (fun args -> 
        tracefn "%s %s" exe args
        Shell.Exec(exe, args, workDir) |> assertExitCodeZero)

// Generate assembly info files with the right version & up-to-date information
Target "AssemblyInfo" (fun _ ->
  let fsProjs =  !! "src/**/*.fsproj"
  let csProjs = !! "src/**/*.csproj"
  fsProjs |> Seq.iter genFSAssemblyInfo
  csProjs |> Seq.iter genCSAssemblyInfo
)

// --------------------------------------------------------------------------------------
// Clean build results & restore NuGet packages

Target "RestorePackages" RestorePackages

Target "Clean" (fun _ ->
    CleanDirs ["bin"; "temp"]
)

Target "CleanDocs" (fun _ ->
    CleanDirs ["docs/output"]
)

// --------------------------------------------------------------------------------------
// Build library & test project

Target "Build" (fun _ ->
    !! solutionFile
    |> MSBuildRelease "" "Build"
    |> ignore
)

// --------------------------------------------------------------------------------------
// Run the unit tests using test runner

Target "RunTests" (fun _ ->
    !! testAssemblies 
    |> NUnit (fun p ->
        { p with
            DisableShadowCopy = true
            TimeOut = TimeSpan.FromMinutes 20.
            OutputFile = "TestResults.xml" })
)

#if MONO
#else
// --------------------------------------------------------------------------------------
// SourceLink allows Source Indexing on the PDB generated by the compiler, this allows
// the ability to step through the source code of external libraies https://github.com/ctaggart/SourceLink

Target "SourceLink" (fun _ ->
    let baseUrl = sprintf "%s/%s/{0}/%%var2%%" gitRaw (project.ToLower())
    use repo = new GitRepo(__SOURCE_DIRECTORY__)
    !! "src/**/*.fsproj"
    -- "src/FSharp.Quotations.Evaluator.NetStandard/*.fsproj"
    |> Seq.iter (fun f ->
        let proj = VsProj.LoadRelease f
        logfn "source linking %s" proj.OutputFilePdb
        let files = proj.Compiles -- "**/AssemblyInfo.fs"
        repo.VerifyChecksums files
        proj.VerifyPdbChecksums files
        proj.CreateSrcSrv baseUrl repo.Revision (repo.Paths files)
        Pdbstr.exec proj.OutputFilePdb proj.OutputFilePdbSrcSrv
    )
)
#endif

// --------------------------------------------------------------------------------------
// Build a NuGet package

Target "NuGet" (fun _ ->
    NuGet (fun p -> 
        { p with   
            Authors = authors
            Project = project
            Summary = summary
            Description = description
            Version = release.NugetVersion
            ReleaseNotes = String.Join(Environment.NewLine, release.Notes)
            Tags = tags
            OutputPath = "bin"
            WorkingDir = "nuget"
            AccessKey = getBuildParamOrDefault "nugetkey" ""
            Publish = hasBuildParam "nugetkey"
            Dependencies = [] })
        ("nuget/" + project + ".nuspec")
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

Target "Release" (fun _ ->
    StageAll ""
    Commit "" (sprintf "Bump version to %s" release.NugetVersion)
    Branches.push ""

    Branches.tag "" release.NugetVersion
    Branches.pushTag "" "origin" release.NugetVersion
)

// --------------------------------------------------------------------------------------
// dotnet core part

Target "DotnetRestore" (fun _ ->
    DotNetCli.Restore (fun c ->
        { c with
            Project = "FSharp.Quotations.Evaluator.NetStandard.sln"
        })
)

Target "DotnetBuild" (fun _ ->
    DotNetCli.Build (fun c ->
        { c with
            Project = "FSharp.Quotations.Evaluator.NetStandard.sln"
            Configuration = "Release"
            Output = "..\\..\\bin\\netstandard"
            AdditionalArgs = [ "/v:n" ]
        })
)

Target "DotnetTest" (fun _ ->
    DotNetCli.Test (fun c ->
        { c with
            Project = "tests/FSharp.Quotations.Evaluator.NetStandard.Tests/FSharp.Quotations.Evaluator.NetStandard.Tests.fsproj"
            Configuration = "Release"
        })
)

// --------------------------------------------------------------------------------------
// Merge .NET Core package in the current NuGet package
Target "MergeDotnetCoreIntoNuget" (fun _ ->

    let nupkg = "bin" </> sprintf "FSharp.Quotations.Evaluator.%s.nupkg" (release.NugetVersion) |> Path.GetFullPath
    // let netcoreNupkg =  "src" </> "FSharp.Quotations.Evaluator.NetStandard" </> "bin" </> "Release" </> sprintf "FSharp.Quotations.Evaluator.%s.nupkg" (release.NugetVersion) |> Path.GetFullPath
    let netcoreNupkg =  "bin" </> "netstandard" </> sprintf "FSharp.Quotations.Evaluator.%s.nupkg" (release.NugetVersion) |> Path.GetFullPath

    let runTool = runCmdIn "src/FSharp.Quotations.Evaluator.NetStandard" "dotnet"
    
    runTool """mergenupkg --source "%s" --other "%s" --framework netstandard1.6 """ nupkg netcoreNupkg
)

Target "DotnetPack" (fun _ ->
    NuGet (fun p -> 
        { p with   
            Authors = authors
            Project = project
            Summary = summary
            Description = description
            Version = release.NugetVersion
            ReleaseNotes = String.Join(Environment.NewLine, release.Notes)
            Tags = tags
            OutputPath = "bin\\netstandard"
            WorkingDir = "nuget"
            AccessKey = getBuildParamOrDefault "nugetkey" ""
            Publish = hasBuildParam "nugetkey"
            Dependencies = [] })
        ("nuget/FSharp.Quotations.Evaluator.NetStandard.nuspec")

    // DotNetCli.Pack (fun c ->
    //     { c with
    //         Project = "src/FSharp.Quotations.Evaluator.NetStandard/FSharp.Quotations.Evaluator.NetStandard.fsproj"
    //         Configuration = "Release"
    //         AdditionalArgs =
    //             [
    //                 sprintf "/p:PackageVersion=%s" release.NugetVersion
    //             ]
    //     })
)

Target "BuildPackage" DoNothing

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target "All" DoNothing

//"Clean"
"RestorePackages"
  ==> "AssemblyInfo"
  ==> "Build"
  ==> "RunTests"
  ==> "CleanDocs"
  ==> "GenerateDocs"
  ==> "All"

"All"
  ==> "ReleaseDocs"

"All" 
#if MONO
#else
  =?> ("SourceLink", Pdbstr.tryFind().IsSome )
#endif
  ==> "NuGet"
  ==> "DotnetRestore"
  ==> "DotnetBuild"
  ==> "DotnetTest"
  ==> "DotnetPack"
  ==> "MergeDotnetCoreIntoNuget"
  ==> "BuildPackage"

"ReleaseDocs"
  ==> "Release"

"BuildPackage"
  ==> "Release"

RunTargetOrDefault "All"