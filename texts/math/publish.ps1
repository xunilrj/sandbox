push-location
cd (Split-Path $PSCommandPath -Parent)

.\build.ps1
cd .\builds

dir -Filter *.pdf | % {
    Write-Verbose "Copying $_"
    copy-item $_.FullName "..\..\..\Público\Math\$($_.Name)"
}


pop-location
