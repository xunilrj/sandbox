#r @"packages/FAKE/tools/FakeLib.dll"
open Fake
open Fake.Testing 

let buildDir = currentDirectory @@ "./.builds/"
let packagesDir = buildDir @@ "packages"

let testsDir = buildDir @@ "tests"
let xunit2Path = currentDirectory @@ "packages/xunit.runner.console/tools/xunit.console.exe"
let xunit2ReportPath = testsDir @@ "xunit.html"

RestorePackages()

let compileCSPROJ a =    
    a |> MSBuild buildDir "Build" 
         [ 
            "Configuration", "Release"
            "Platform", "AnyCPU"
            "DeployOnBuild", "True"
            "DeployTarget", "Package"
            "PackageLocation", packagesDir
        ]
        |> Log "MSBUILD: "

let runUnitTests a =    
    ensureDirectory testsDir
    a |> xUnit2 (fun p -> { p with HtmlOutputPath = Some xunit2ReportPath; ToolPath = xunit2Path;ExcludeTraits=[("Type","Integration");("Type","Automated")]})

let runAutomatedTests a =    
    ensureDirectory testsDir
    a |> xUnit2 (fun p -> { p with HtmlOutputPath = Some xunit2ReportPath; ToolPath = xunit2Path;IncludeTraits = [("Type","Automated")]})

Target "Clean" (fun _ ->
    CleanDir buildDir
)

Target "Compile" (fun _ ->
    !! "sources/**/*.csproj" |> compileCSPROJ
)

Target "Test" (fun _ ->     
    
    !! (buildDir @@ "**/*.Tests.dll") |> runUnitTests    
)

Target "Default" (fun _ ->
    trace "Hello World from FAKE"
)

"Clean"
    ==> "Compile"
    ==> "Test"
    ==> "Default"

RunTargetOrDefault "Default"