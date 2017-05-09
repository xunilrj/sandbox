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

function ...
{
    param([Parameter(Position=0)]$Expression, [Parameter(Position = 1)]$Count = 999, [Parameter(Position = 2)]$SleepMs = 1000)
    do
    {
        try{
            $result = $Expression.Invoke()
            $result = $result[0]
        }catch
        {
            $result = $null
        }

        if($result -eq $null)
        {
            Start-Sleep -Milliseconds $SleepMs
        }
        --$Count
    }while($result -eq $null -and $Count -gt 0)
    $result
}

function trim
{
    [CmdletBinding()]
    param([Parameter(ValueFromPipeline=$true)]$PSItem)
    process{
        if([System.String]::IsNullOrEmpty($PSItem) -eq $false){
            $PSItem.Trim()
        }else{
            $PSItem
        }
    }
}