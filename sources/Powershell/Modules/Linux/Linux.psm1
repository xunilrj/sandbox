function grep
{
    [CmdletBinding()]
    param([Parameter(Mandatory=$true,Position=0)][Alias("e")]$regexp, [Parameter()][Alias("v")][switch]$NotMatch, [Parameter(ValueFromPipeline = $true)]$PSItem)
    process{
        $_ | Select-String -Pattern $regexp -NotMatch:$NotMatch
    }
}