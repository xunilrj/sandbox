Import-Module Linux
Import-Module OneLiners

function Get-Wlan()
{
    netsh.exe wlan show profiles | grep "User Profile" | cut ":" -f 1 | trim | % {
        $result = netsh.exe wlan show profiles name="$_" key=clear
        $key = $result | grep "Key Content" |cut ":" -f 1 | trim
        New-Object PSCustomObject -Property @{
            Password = $key;
            Name = $_            
        }
    }
}

function Add-CurrentLocationToPath
{
	$oldPath=(Get-ItemProperty -Path 'Registry::HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\Session Manager\Environment' -Name PATH).Path
    $currentdir = ((gl).Path)
    if($oldPath.Contains($currentdir) -eq $false){
	    $newPath = $oldPath + ";" + $currentdir
        Set-ItemProperty -Path 'Registry::HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\Session Manager\Environment' -Name PATH –Value $newPath
    }
	
    if((gi Env:\Path).Value.Contains($currentdir) -eq $false){
        Set-Item -Path Env:\Path -Value ((gi Env:\Path).Value + ";" + ((gl).Path))
    }
}