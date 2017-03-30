filter ConvertFrom-Xml{
    process{
        [xml]$_
    }
}

function %% {
    param([Parameter(Position=0)]$Name,[Parameter(ValueFromPipeline=$true)]$Item)
    begin{
        $properties = $Name.Split('.')
        $properties | % {Write-Verbose $_}
    }
    process{
        $currentValue = $_
        $properties | % {
            $currentValue = $currentValue | % $_
        } | Out-Null
        $currentValue
    }
}

function ??? {
    [CmdletBinding()]
    param([Parameter(Mandatory=$true,Position=0)][Alias("e")]$regexp, [Parameter()][Alias("v")][switch]$NotMatch, [Parameter(ValueFromPipeline = $true)]$PSItem)
    begin{
        $Regex = [System.Text.RegularExpressions.Regex]::new($regexp,"ExplicitCapture")
    }
    process{
        $result = $Regex.Match($Regex)
        if($result.Success -eq $true){
            $newobj = New-Object PSCustomObject
            $Regex.GetGroupNames() | ? {$_ -notmatch "\d+"} | % { 
                $newobj = $newobj | Add-Member -Name $_ -Value ($result[$_].Value) -MemberType NoteProperty
            }
            $newobj 
        }
    }
}
cat "C:\github\xunilrj-mardocs\MarDocs\MarDocs.Core\Models\DrySpotFixture\DrySpotFixture.cs" | ??? "public string (?<NAME>.*?)"