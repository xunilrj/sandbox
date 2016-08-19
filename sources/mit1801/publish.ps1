push-location
cd (Split-Path $PSCommandPath -Parent)

.\build.ps1
copy-item .\builds\handout.Log.pdf "..\..\Público\Math\Handout.log.pdf"

pop-location
