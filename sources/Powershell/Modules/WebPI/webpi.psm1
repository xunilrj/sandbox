function Get-Product
{
    [CmdletBinding()]
    param($Name)

    Write-Verbose "Testing ifwebpicmd is installed..."

    $webpicmd = command webpicmd
    if($webpicmd -eq $null){
        Write-Error "NO!"
    }
    else{
        Write-Verbose "OK"
    }
     
    webpicmd /List /ListOption:Available *>&1 | % {Write-Verbose $_}
}

function Install-Product
{
    [CmdletBinding()]
    param($Name)

    Write-Verbose "Testing ifwebpicmd is installed..."

    $webpicmd = command webpicmd
    if($webpicmd -eq $null){
        Write-Error "NO!"
    }
    else{
        Write-Verbose "OK"
    }
     
    webpicmd /Install /Products:$Name /AcceptEula *>&1 | % {Write-Verbose $_}

    if($Name -eq "WDeploy36"){
        if($env:Path.Contains("%PROGRAMFILES%/IIS/Microsoft Web Deploy V3")){
            setx path "%path%;%PROGRAMFILES%/IIS/Microsoft Web Deploy V3;"
        }
    }
}