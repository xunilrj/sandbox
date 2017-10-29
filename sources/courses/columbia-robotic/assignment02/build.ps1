pushd (Split-Path $PSCommandPath -Parent)

. ./Remove-Utf8BOM.ps1
. ./Out-FileUtf8NoBom.ps1

#example at
#https://youtu.be/8bty4buvOVs
#q1 0 0.5 0
#q2 0 0.3 0
$DH = "q1 0 0.5 0 q2 0 0.3 0"
.\sketch.robot.ps1 $DH  0,0,0.5 | Out-FileUtf8NoBom ./robot.001.asy;
.\foward.kinematics.robot.ps1 $DH | Out-FileUtf8NoBom ./foward.kinematics.001.m;


#https://youtu.be/vlusOKOffas
# 0 q1 0 0
# q2 0 0.7 0
# q3 0 0.7 0
# 0 q4 0 0
# q5 0 0 0
$DH = "0 q1 0 0 q2 0 0.7 0 q3 0 0.7 0 0 q4 0 0 q5 0 0 0"
.\sketch.robot.ps1 $DH  0.5,45,45,-0.2,45 | Out-FileUtf8NoBom ./scara.asy;
.\foward.kinematics.robot.ps1 $DH | Out-FileUtf8NoBom ./foward.kinematics.scara.m;

#q1 0 2 90
#q2 0 0 90
#0 q3 0 0
$DH = "q1 0 2 90 q2 0 0 90 0 q3 0 0"
.\sketch.robot.ps1 $DH  0,0,0.5 | Out-FileUtf8NoBom ./robot.asy;
.\foward.kinematics.robot.ps1 $DH | Out-FileUtf8NoBom ./foward.kinematics.m;

popd