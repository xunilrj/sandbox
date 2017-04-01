function Replace-Content
{
    param([Parameter(Position = 0)]$Path,[Parameter(Position=1,ValueFromPipeline=$true)][byte[]]$Item)
    end{
        [System.IO.File]::WriteAllBytes($Path,$Item)
    }
}

function ConvertFrom-PDF
{
    param([Parameter(Position = 0, Mandatory =$true)]$Path)

    $psm1Path = Split-Path $PSCommandPath -Parent
    $absolutePath = Resolve-Path (Join-Path $psm1Path "itextsharp.dll")
    [System.Reflection.Assembly]::LoadFile($absolutePath) | Out-Null

    $pdfReader = [iTextSharp.text.pdf.PdfReader]::new($Path)
    $pdfReader
}

function Get-PDFText
{
    param([Parameter(Position = 0, Mandatory = $true, ValueFromPipeline = $true)]$Item)

    if($Item -is [System.String]){
        $Item = [iTextSharp.text.pdf.PdfReader](ConvertFrom-PDF $Item)
    }

    $Item = [iTextSharp.text.pdf.PdfReader]$Item

    $builder = [System.Text.StringBuilder]::new()
    1..$Item.NumberOfPages | %{
        $strategy = [iTextSharp.text.pdf.parser.SimpleTextExtractionStrategy]::new()
        $text = ([iTextSharp.text.pdf.parser.PdfTextExtractor]::GetTextFromPage($Item, $_, $strategy))
        $builder.Append($text) | Out-Null
    }

    $builder.ToString()
}

function Watch-Item
{
    param($Path,$Filter,$Expression)
    
    if([string]::IsNullOrEmpty($Filter)) { $Filter = "*.*" }

    $watcher = New-Object IO.FileSystemWatcher $folder, $filter -Property @{ 
        IncludeSubdirectories = $false
        EnableRaisingEvents = $true
    }

    if($Expression -eq $null){
        $Expression = [scriptblock]::Create('')
    }

    Register-ObjectEvent $Watcher "Changed" -Action $Expression
    while($true){
        Start-Sleep -Seconds 5
    }
}
