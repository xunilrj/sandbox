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