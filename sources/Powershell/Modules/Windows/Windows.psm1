function Get-Product
{
    Get-WmiObject -Class Win32_Product
}

function Uninstall-Product
{
    [CmdletBinding()]
    param([Parameter(ValueFromPipeline=$true)]$Product)
    process{
        if($Product -is [System.Management.ManagementBaseObject]){
            $wmiobj = [System.Management.ManagementBaseObject]$Product
        }

        $wmiobj.Uninstall()
    }
}