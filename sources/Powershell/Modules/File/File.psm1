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

function ConvertFrom-Xml
{
    param([Parameter(ValueFromPipeline=$true)]$Item)
    process{
        if($Item -is [System.IO.FileInfo]){
            [xml][System.IO.File]::ReadAllText($Item.FullName)
        }else{
            [xml]$Item 
        }
    }
}

Set-Alias xml ConvertFrom-xml
