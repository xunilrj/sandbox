function Get-Handle($Path)
{
    E:\OneDrive\Apps\SysInternals\handle.exe $Path /accepteula | Select-String -Pattern "pid:" | %{
        $result = $_.ToString().Split(" ", [System.StringSplitOptions]::RemoveEmptyEntries)
        New-Object PsCustomObject -Property @{
            ProcessName = $result[0];
            ProcessID = $result[2];
            Type = $result[4];
            ID = $result[5].Trim(":");
            Path = $result[6];
        }
    }
}

function Close-Handle
{
    [CmdletBinding()]
    param([Parameter(ValueFromPipelineByPropertyName=$true)]$ID, [Parameter(ValueFromPipelineByPropertyName=$true)]$ProcessID)
    process{
        start "E:\OneDrive\Apps\SysInternals\handle.exe" "-c $ID -y /accepteula" -Verb runas -Wait *>&1
    }
}