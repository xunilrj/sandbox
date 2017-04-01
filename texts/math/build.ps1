Push-Location
cd (Split-Path $PSCommandPath -Parent)

function Resolve-PathSafe
{
    param
    (
        [string] $Path
    )
     
    $ExecutionContext.SessionState.Path.GetUnresolvedProviderPathFromPSPath($Path)
}

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

set-alias pandoc C:\Users\xunil\AppData\Local\Pandoc\pandoc.exe

#ri .\builds -Force -Recurse -EA SilentlyContinue
mkdir .\builds -EA SilentlyContinue
dir -Filter *.tex | %{
    Write-Progress -Activity $_ -CurrentOperation "Building $($_.FullName)..."
    $fileInfo = [System.IO.FileInfo]::new($_.FullName)
    $source = $fileInfo.FullName    
    $temp = [System.IO.Path]::GetTempFileName()
    $name = $fileInfo.Name
    Write-Progress -Activity $_ -CurrentOperation "Converting to PDF"
    gc $source | %{$_.Replace('\plotSamples}{200}','\plotSamples}{5000}')} | Out-FileUtf8NoBom $temp
    latex $temp --output-format=pdf --output-directory=builds --job-name=$name *>&1 | % {Write-Verbose $_}   
}

Pop-Location