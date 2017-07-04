function Out-FileUtf8NoBom {

  [CmdletBinding()]
  param(
    [Parameter(Mandatory, Position=0)] [string] $LiteralPath,
    [switch] $Append,
    [switch] $NoClobber,
    [AllowNull()] [int] $Width,
    [switch]$NoNewLine,
    [Parameter(ValueFromPipeline)] $InputObject
  )
  [Environment]::CurrentDirectory = $PWD
  $LiteralPath = [IO.Path]::GetFullPath($LiteralPath)
  if ($NoClobber -and (Test-Path $LiteralPath)) { 
    Throw [IO.IOException] "The file '$LiteralPath' already exists."
  }
  $sw = New-Object IO.StreamWriter $LiteralPath, $Append

  $htOutStringArgs = @{}
  if ($Width) {
    $htOutStringArgs += @{ Width = $Width }
  }
   try {
    $Input | Out-String -Stream @htOutStringArgs | % {
        if($NoNewLine.IsPresent){
            $sw.Write($_)
        }else{
            $sw.WriteLine($_)
        }
    }    
  } finally {
    $sw.Dispose()
  }
}

function New-CMakeEnv
{
    [CmdletBinding()]
    param([Parameter(Position=0,Mandatory=$true)]$Name)
    if([System.IO.File]::Exists("CMakeLists.txt")){
        Write-Verbose "Environment already configured"
        return
    }

    New-Item -Path CMakeLists.txt
    "cmake_minimum_required (VERSION 2.6)
project ($Name)

set($($Name)_VERSION_MAJOR 0)
set($($Name)_VERSION_MINOR 1)

" | Out-FileUtf8NoBom  CMakeLists.txt | Out-Null

    New-Item -Path build.ps1
    "param([switch]`$SkipInstall, [switch]`$SkipGenerate, [string]`$Build = ""Debug"", [string]`$Run = ""Debug"")

Push-Location
cd (Split-Path `$PSCommandPath -Parent)

if(`$SkipInstall.IsPresent -eq `$false){
    Write-Information ""Installing...""
    `$choco = command choco -EA SilentlyContinue
    if(`$choco -eq `$null){
        iwr https://chocolatey.org/install.ps1 -UseBasicParsing -Verbose | iex -Verbose
    }

    `$cmake = command cmake -EA SilentlyContinue
    if(`$cmake -eq `$null){
        choco install cmake -y -v
    }
    Write-Information ""Finished!""
}

if(`$SkipGenerate.IsPresent -eq `$false){
    Write-Information ""Generating...""
    rm .\.build -Recurse -Force -EA SilentlyContinue
    mkdir .build | cd
    mkdir msvc | cd
    cmake ..\.. -G ""Visual Studio 14 2015 Win64""
    Write-Information ""Finished!""
}

if([System.String]::IsNullOrEmpty(`$Build) -eq `$false){
    Write-Information ""Building...""
    cmake --build .\.build\msvc --config `$Build
    Write-Information ""Finished!""
}

if([System.String]::IsNullOrEmpty(`$Run) -eq `$false){
    .\.build\msvc\`$Run\$Name.exe
}

Pop-Location" | Out-FileUtf8NoBom  build.ps1 | Out-Null
}

function New-CMakeCppFile
{
    [CmdletBinding()]
    param([Parameter(Position=0,Mandatory=$true)]$Name)

    mkdir sources
    New-Item "sources/$Name.cpp"
    "add_executable(input sources/main.cpp)" | Out-FileUtf8NoBom CMakeLists.txt -Append
}

$ToolChainFile = $null
function Set-DCMakeToolchainFile($File)
{
    $global:ToolChainFile = $File
}

function Invoke-CMakeBuild
{
    param([switch]$ForceGeneration)
    $exist = Test-Path .\.build\msvc -EA SilentlyContinue
    if($exist -ne $true -or $ForceGeneration.IsPresent){
        ri .\.build\msvc -EA SilentlyContinue -Recurse -Force
        mkdir .\.build -Force | Out-Null
        mkdir .\.build\msvc -Force | Out-Null
        pushd .\.build\msvc

        if($global:ToolChainFile -eq $null){
        cmake ..\.. -DCMAKE_TOOLCHAIN_FILE=$($global:ToolChainFile)
        }
        else{
        cmake ..\.. -G "Visual Studio 15 Win64"
        }
        
        popd
    }
    
    cmake --build .\.build\msvc --config Debug 
}