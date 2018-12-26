# Some helper functions for Chocolatey


    function Install-ChocolateyIfNeeded
    {
        [CmdletBinding()]
        param([Parameter(ValueFromPipeline=$true)]$Packages)
        process{
            $_|
                %{$pkg=$_;choco list $pkg -l|sls $pkg|Measure|? Count -eq 0|%{choco list $pkg|%{$_}|select -Skip 1|select -SkipLast 1}|
                ConvertFrom-Csv -Delimiter ' ' -Header 'Name'|
                ogv -PassThru|
                %{choco install $_.Name -y}
                }
        }
    }