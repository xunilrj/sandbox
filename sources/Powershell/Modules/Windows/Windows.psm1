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

function Show-BalloonTip {            
[cmdletbinding()]            
param(            
 [parameter(Mandatory=$true,Position=0)]            
 [string]$Title,            
 [Parameter(Position=1)]
 [string]$Message,      
 [ValidateSet("Info","Warning","Error")]
 [string]$MessageType = "Info",          
 [parameter()]            
 [int]$Duration=10000
)            
    [system.Reflection.Assembly]::LoadWithPartialName('System.Windows.Forms') | Out-Null            
    $balloon = New-Object System.Windows.Forms.NotifyIcon            
    $path = Get-Process -id $pid | Select-Object -ExpandProperty Path            
    $icon = [System.Drawing.Icon]::ExtractAssociatedIcon($path)            
    $balloon.Icon = $icon            
    $balloon.BalloonTipIcon = $MessageType            
    $balloon.BalloonTipText = $Message            
    $balloon.BalloonTipTitle = $Title    
    $balloon.Visible = $true            
    $balloon.ShowBalloonTip($Duration)            
}