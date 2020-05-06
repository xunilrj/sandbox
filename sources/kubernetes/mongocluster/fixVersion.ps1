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
    [System.IO.Directory]::SetCurrentDirectory($PWD) # Caveat: .NET Core doesn't support [Environment]::CurrentDirectory
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

$version = [System.Environment]::OSVersion.Version.ToString()
$dockerFrom = "FROM mcr.microsoft.com/windows/servercore:1909:1803"
$imageName = "mongo-1903"

if($version.StartsWith("10.0.18363"))
{
    $dockerFrom = "FROM mcr.microsoft.com/windows/servercore:1909:1909"
    $imageName = "mongo-1909"
}

$lines = cat .\Dockerfile | Select -Skip 1

$dockerFrom | Out-FileUtf8NoBom .\Dockerfile
$lines | Out-FileUtf8NoBom  .\Dockerfile -Append

return $imageName