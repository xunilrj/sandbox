pushd (Split-Path $PSCommandPath -Parent)

. ./Remove-Utf8BOM.ps1
. ./Out-FileUtf8NoBom.ps1

#q1 0 2 90
#q2 0 0 90
#0 q3 0 0
.\sketch.robot.ps1 "q1 0 2 90 q2 0 0 90 0 q3 0 0" 0,0,0.5 | Out-FileUtf8NoBom ./robot.asy;
asy ./robot.asy

popd