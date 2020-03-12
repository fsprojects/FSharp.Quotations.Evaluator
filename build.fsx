// --------------------------------------------------------------------------------------
// FAKE build script 
// --------------------------------------------------------------------------------------

#r "paket: groupref build //"
#load "./.fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Tools
open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let gitRepoUrl = "https://github.com/fsprojects/FSharp.Quotations.Evaluator"
let artifacts = __SOURCE_DIRECTORY__ @@ "artifacts"

// Read additional information from the release notes document
let release = ReleaseNotes.load "RELEASE_NOTES.md"
let configuration = Environment.environVarOrDefault "Configuration" "Release"

// --------------------------------------------------------------------------------------
// Clean build results

Target.create "Clean" (fun _ ->
    Shell.cleanDirs [artifacts; "docs/output"]
)

// --------------------------------------------------------------------------------------
// Clean build results

Target.create "Build" (fun _ ->
    DotNet.build (fun c ->
        { c with
            Configuration = DotNet.BuildConfiguration.fromString configuration

            MSBuildParams =
            { c.MSBuildParams with
                Properties = [("Version", release.NugetVersion)] }

        }) __SOURCE_DIRECTORY__
)

// --------------------------------------------------------------------------------------
// Run the unit tests

Target.create "RunTests" (fun _ ->
    DotNet.test (fun c ->
        { c with
            Configuration = DotNet.BuildConfiguration.fromString configuration
            NoBuild = true
            Blame = true

            MSBuildParams =
                { c.MSBuildParams with
                    Properties = [("ParallelizeAssemblies", "true"); ("ParallelizeTestCollections", "true")] }
        }) __SOURCE_DIRECTORY__
)

// --------------------------------------------------------------------------------------
// Build a NuGet package

Target.create "NuGet.Pack" (fun _ ->
    let releaseNotes = String.toLines release.Notes |> System.Net.WebUtility.HtmlEncode
    DotNet.pack (fun pack ->
        { pack with
            OutputPath = Some artifacts
            Configuration = DotNet.BuildConfiguration.Release
            MSBuildParams =
                { pack.MSBuildParams with
                    Properties = 
                        [("Version", release.NugetVersion)
                         ("PackageReleaseNotes", releaseNotes)] }
        }) __SOURCE_DIRECTORY__
)

Target.create "NuGet.ValidateSourceLink" (fun _ ->
    for nupkg in !! (artifacts @@ "*.nupkg") do
        let p = DotNet.exec id "sourcelink" (sprintf "test %s" nupkg)
        if not p.OK then failwithf "failed to validate sourcelink for %s" nupkg
)

Target.create "NuGet.Push" (fun _ ->
    let source = "https://api.nuget.org/v3/index.json"
    let apikey =  Environment.environVarOrDefault "NUGET_KEY" ""
    for artifact in !! (artifacts + "/*nupkg") do
        let result = DotNet.exec id "nuget" (sprintf "push -s %s -k %s %s" source apikey artifact)
        if not result.OK then failwith "failed to push packages"  
)

// --------------------------------------------------------------------------------------
// Generate the documentation

Target.create "GenerateDocs" (fun _ ->
    let res = DotNet.exec id "fsi" "--define:RELEASE docs/tools/generate.fsx"
    if not res.OK then failwith "failed to generate docs"
)

// --------------------------------------------------------------------------------------
// Release Scripts

Target.create "ReleaseDocs" (fun _ ->
    let tempDocsDir = "temp/gh-pages"
    let outputDocsDir = "docs/output"

    Directory.ensure outputDocsDir

    Shell.cleanDir tempDocsDir
    Git.Repository.cloneSingleBranch "" (gitRepoUrl + ".git") "gh-pages" tempDocsDir
    Shell.copyRecursive outputDocsDir tempDocsDir true |> Trace.tracefn "%A"
    Git.Staging.stageAll tempDocsDir
    Git.Commit.exec tempDocsDir (sprintf "Update generated documentation for version %s" release.NugetVersion)
    Git.Branches.push tempDocsDir
)

Target.create "ReleaseTag" (fun _ ->
    Git.Staging.stageAll ""
    Git.Commit.exec "" (sprintf "Bump version to %s" release.NugetVersion)
    Git.Branches.push ""

    Git.Branches.tag "" release.NugetVersion
    Git.Branches.pushTag "" "origin" release.NugetVersion
)

// --------------------------------------------------------------------------------------
// Run all Target.creates by default. Invoke 'build <Target.create>' to override

Target.create "Default" ignore
Target.create "Bundle" ignore
Target.create "Release" ignore

"Clean"
  ==> "Build"
  ==> "RunTests"
  ==> "Default"

"Default"
  ==> "GenerateDocs"
  ==> "NuGet.Pack"
  ==> "NuGet.ValidateSourceLink"
  ==> "Bundle"

"Bundle"
  ==> "ReleaseDocs"
  ==> "NuGet.Push"
  ==> "ReleaseTag"
  ==> "Release"

Target.runOrDefault "Default"
