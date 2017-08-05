param($Files, [switch]$RunTLC)

function Out-FileUtf8NoBom {

  [CmdletBinding()]
  param(
    [Parameter(Mandatory, Position=0)] [string] $LiteralPath,
    [switch] $Append,
    [switch] $NoClobber,
    [AllowNull()] [int] $Width,
    [Parameter(ValueFromPipeline)] $InputObject
  )

  #requires -version 3

  # Make sure that the .NET framework sees the same working dir. as PS
  # and resolve the input path to a full path.
  [Environment]::CurrentDirectory = $PWD
  $LiteralPath = [IO.Path]::GetFullPath($LiteralPath)

  # If -NoClobber was specified, throw an exception if the target file already
  # exists.
  if ($NoClobber -and (Test-Path $LiteralPath)) { 
    Throw [IO.IOException] "The file '$LiteralPath' already exists."
  }

  # Create a StreamWriter object.
  # Note that we take advantage of the fact that the StreamWriter class by default:
  # - uses UTF-8 encoding
  # - without a BOM.
  $sw = New-Object IO.StreamWriter $LiteralPath, $Append

  $htOutStringArgs = @{}
  if ($Width) {
    $htOutStringArgs += @{ Width = $Width }
  }

  # Note: By not using begin / process / end blocks, we're effectively running
  #       in the end block, which means that all pipeline input has already
  #       been collected in automatic variable $Input.
  #       We must use this approach, because using | Out-String individually
  #       in each iteration of a process block would format each input object
  #       with an indvidual header.
  try {
    $Input | Out-String -Stream @htOutStringArgs | % { $sw.WriteLine($_) }
  } finally {
    $sw.Dispose()
  }

}

if($Files -eq $null)
{
    $Files = Get-ChildItem . -Filter *.tla
}

ri .check -Force -Recurse -EA SilentlyContinue
mkdir .check

if($RunTLC.IsPresent)
{
    $Files | % {
        Write-Host "Running $($_.FullName)"
        Write-Host "Removing log..."
        ri .check/.temp -Force -Recurse -EA SilentlyContinue

        Write-Host "Transpiling..."
        java -cp C:/Users/xunil/Downloads/tla pcal.trans "$_"
        ri "$($_.Name.Replace($_.Extension, '')).cfg" -Force
        ri "$($_.Name.Replace($_.Extension, '')).old" -Force
        
        Write-Host "Creating .check folder..."
        mkdir .check/.temp | Out-Null

        Write-Host "Creating Model TLA..."
        "---- MODULE MAIN ----
EXTENDS $($_.Name.Replace($_.Extension,'')), TLC
const_147855430910980000 == 10
----
spec_147855430914082000 == Spec
----
=============================================================================
" | Out-FileUtf8NoBom .check/.temp/MAIN.tla

        Write-Host "Creating config file..."
        "CONSTANT defaultInitValue = defaultInitValue
CONSTANT
N <- const_147855430910980000
SPECIFICATION
spec_147855430914082000
" | Out-FileUtf8NoBom .check/.temp/MAIN.cfg
        
        copy $_ ".check/.temp/$($_.Name)" -Force
        Push-Location
        cd .check/.temp

        java -cp C:/Users/xunil/Downloads/tla tla2sany.SANY quickfind
        java -cp C:/Users/xunil/Downloads/tla tla2sany.SANY MC

        $modelPath = [System.IO.Path]::Combine($_.Directory, ".check/.temp")
        $mcCFG = [System.IO.Path]::Combine($modelPath, "MAIN.cfg")

        Write-Host $mcCFG
        Write-Host "Checking..."
        
        java -cp C:/Users/xunil/Downloads/tla tlc2.TLC "MAIN" "-config" "$mcCFG" "-dump" "states" > result.txt

        Move-Item result.txt "../$($_.Name).result.txt"
        
        Write-Host "Done!"

        Pop-Location
    }    
}

ri .check/.temp -Force -Recurse -EA SilentlyContinue