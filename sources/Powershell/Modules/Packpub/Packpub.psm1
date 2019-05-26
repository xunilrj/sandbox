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

$token = "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJ1c2VySWQiOiI0ODljZjk2MS1hZjJlLTRkYTEtOWViZi0xMDM1YmZkZmJlY2YiLCJ1c2VybmFtZSI6IjE5ODUuZGFuaWVsQGdtYWlsLmNvbSIsInBlcm1pc3Npb25zIjpbXSwiaWF0IjoxNTU3MzE1NzE0LCJleHAiOjE1NTczMTkzMTR9.u4iKXgmdEDAKxenacUak_hWP1T0uuhGqljKO2YFuLLkLVndomGb95p1h3fTN0wGrxZzj7m2FFJa1lkk9lRKhhfvEx6EufRYUbMoa-nT0SeRqo0szLN_BZLRl-9zLNxlRJ2XF0rXrP_u403GB4MuTj5qF55wbOjr6W_cGj_1rYqXuAsm7_2DWoWeXxtZ9LF8_dr6nIDrolrfqHDJWTdCNdR3jvi1EmrUxH-riB1UW0XfFIYfqtsDFkwypVMJj1vOW7ypIcyrdk9OvDPxqxmbe4br6lTjWPyTN4GwdWyemGkzOXwmxkr7vgxA1qi1DQNfIYWSbGaq3_4gvn57sml83nA"
function get($Url)
{
    Invoke-RestMethod -Method Get -Uri $Url -Headers @{"Authorization"="Bearer $token"}
}

function Get-PackpubBook($isbn, [switch]$Force)
{
    $summary = get https://static.packt-cdn.com/products/$isbn/summary
    $toc = get https://static.packt-cdn.com/products/$isbn/toc
    

    if($Force)
    {
        rm $ISBN -Force -Recurse -EA SilentlyContinue
    }
    if((Test-Path $isbn) -eq $false)
    {
        mkdir $ISBN
    }
    
    pushd $ISBN
       

    if($Force)
    {
        rm .build -Force -Recurse -EA SilentlyContinue
    }

    if((Test-Path .build) -eq $false)
    {
        mkdir .build
        pushd .build

        $summary | ConvertTo-Json -Depth 10 | Out-FileUtf8NoBom "summary.json"
        wget $summary.coverImage -OutFile "cover.png"
        $authors = $summary.authors|% {
            get https://static.packt-cdn.com/authors/$_
        }
        $toc | ConvertTo-Json -Depth 10 | Out-FileUtf8NoBom "toc.json"
        ConvertTo-Json -InputObject @($authors) -Depth 10 | Out-FileUtf8NoBom "authors.json"

        $sections = $toc.chapters.sections

        $toc.chapters|%{
            $chapterid = $_.id
            if($_.id -match "(?<N>\d+)"){
                $chapterid= [int]::Parse($Matches['N'])
            }
            $sections |% {
                Write-Progress -Activity $isbn -CurrentOperation "Chapter $($_.id)"
            
                $i = 1;
                if($_.id -match "(?<N>\d+)"){
                    $i = [int]::Parse($Matches['N'])
                }
                $chapter = get https://services.packtpub.com/products-v1/products/$isbn/$chapterid/$($_.id)
                $chapter | ConvertTo-Json -Depth 10 | Out-FileUtf8NoBom "chapter_$($_.id).json"
                $fullPath = Join-Path (gl).ProviderPath "$($_.id).original"
                Write-Host $fullPath
            
                #$chapterhtml = wget $chapter.data
                #[System.IO.File]::WriteAllText($fullPath, $chapterhtml.Content, [System.Text.Encoding]::UTF8)
                wget $chapter.data -OutFile $fullPath
                $url = [System.Uri]::new( $chapter.data)
                if($url.LocalPath.EndsWith(".html")) {
                    Copy-Item "$($_.id).original" "$($_.id).html"
                } elseif($url.LocalPath.EndsWith(".mp4")) {
                    Move-Item "$($_.id).original" "$($_.id).mp4"
                }
            }
        }
        popd
    }
    else 
    {
        Write-Host "Skiping download of originals"
    }

    pushd .build

    rm *.html
    ls *.original | % {Copy-Item $_.FullName "$($_.FullName).html"}
    $count = (ls *.html | Measure-Object).Count
    $i = 0
    (ls .\*.html)|% {
        cat $_|
            sls '"(?<NAME>[^"]*\.jpg)"' -AllMatches|
            % {$_.Matches.Value}|
            % {$_.Trim('"')}|
            Sort -Unique
    }|% {
        Write-Progress -Activity $isbn -CurrentOperation "Resource $($_)"
        $new_ = [System.Text.RegularExpressions.Regex]::Replace($_, "^/graphics", "")
            
        $fi = [System.IO.FileInfo]::new($_)
        mkdir $fi.Directory.FullName -Force | Out-Null
        
        $url = "https://static.packt-cdn.com/products$($new_)"
        if((Test-Path $fi.FullName) -eq $false) {
            Write-Host "Downloading $($fi.FullName)"
            wget $url -OutFile $fi.FullName
        }
        else 
        {
            Write-Host "Skiping download of $url"
        }
    }

    #(ls .\*.html)|% {
        #$fullName = $_.FullName
        #$str = [System.IO.File]::ReadAllText($fullname, [System.Text.Encoding]::UTF8)
        #$str = $str.Replace("<h2","<h1").Replace("</h2","</h1")
        #$str = $str.Replace("<h3","<h2").Replace("</h3","</h2")
        #$str = $str.Replace("<h4","<h3").Replace("</h4","</h3")
        #[System.IO.File]::WriteAllText($fullName, $str, [System.Text.Encoding]::UTF8)                          
    #}

    iex "pandoc $((ls *.html|%{$_.Name}) -join ' ') -o ../$ISBN.pdf --toc --top-level-division=chapter -V documentclass=book --template=c:\github\pandoc-latex-template\eisvogel.tex --base-header-level=2 -s"

    popd
    popd

    #rm c:\graphics -Recurse -Force -EA SilentlyContinue
}

#Get-PackpubBook 9781787123663
#Get-PackpubBook 9781784391874
#start book.epub