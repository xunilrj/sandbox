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


function get($Url)
{
    Invoke-RestMethod -Method Get -Uri $Url
}

function Get-Chapters($TOC)
{
    rm .build -Force -Recurse -EA SilentlyContinue
    mkdir .build
    pushd .build

    $sections = $toc.chapters.sections
    $sections |% {
        Write-Progress -Activity $_.id -CurrentOperation "a"
        ##Write-Output $_.ToString()
        #ni -Path $_.id
        $i = 1;
        if($_.id -match "^ch(?<N>\d+)"){
            $i = [int]::Parse($Matches['N'])
        }
        $chapter = get https://services.packtpub.com/products-v1/products/9781787123663/$i/$($_.id)
        $chapterhtml = wget $chapter.data

        $chapterhtml.Content | Out-FileUtf8NoBom "$($_.id).html"
    }

    popd
}
Get-Chapters $toc

cat .\.build\*.html | Out-FileUtf8NoBom "./.build/book.html"
pushd .build
(cat .\book.html)|% {
    $_|sls '"(?<NAME>[^"]*\.jpg)"' -AllMatches|% {$_.Matches.Value}|%{$_.Trim('"')}|Sort -Unique
}|% {
    $new_ = [System.Text.RegularExpressions.Regex]::Replace($_, "^/graphics", "")
    $url = "https://static.packt-cdn.com/products$($new_)"
    Write-Output $url
    Write-Output $fi.Directory.FullName
    
    $fi = [System.IO.FileInfo]::new($_)
    mkdir $fi.Directory.FullName -Force

    wget $url -OutFile $fi.FullName
}
popd
#iex "pandoc -o book.epub $((ls *.html|%{ "./" + $_.Name}) -join " ")"

start book.epub