function grep
{
    [CmdletBinding()]
    param([Parameter(Mandatory=$true,Position=0)][Alias("e")]$regexp, [Parameter()][Alias("v")][switch]$NotMatch, [Parameter(ValueFromPipeline = $true)]$PSItem)
    process{
        $_ | Select-String -Pattern $regexp -NotMatch:$NotMatch | % {$_.ToString()}
    }
}

function cut
{
    [CmdletBinding()]
    param([Parameter(Mandatory=$true,Position=0)][Alias("d")]$Delimiter, [Parameter()][Alias("f")]$Index, [Parameter(ValueFromPipeline = $true)]$PSItem)
    process{
        $PSItem | % {$_.Split($Delimiter)[$Index]}
    }
}

function pwd
{
    (gl).Path
}

Remove-Item alias:/pwd -EA SilentlyContinue

function touch
{
    [CmdletBinding()]
    param([Parameter(Position=0)]$Path)
    $item = Get-Item $Path
    if($item -eq $null)
    {
        New-Item $Path
    }
}