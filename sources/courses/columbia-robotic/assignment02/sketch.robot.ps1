param($DH, $QS)

function ToArray
{
    begin
    {
        $output = @(); 
    }
    process
    {
        $output += $_; 
    }
    end
    {
        return ,$output; 
    }
}

Write-Output "import three;"
Write-Output "import solids;"
Write-Output "unitsize(1cm);"
Write-Output "size(200);"

Write-Output "triple xx = (1,0,0);"
Write-Output "triple yy = (0,1,0);"
Write-Output "triple zz = (0,0,1);"
Write-Output "triple zero = (0,0,0);"
Write-Output "transform3 tcurrent = shift(0,0,0);"
Write-Output "triple currentjoint = zero;"

$parameters = $DH -split " "
$i = 0
$ji = 1
1..100 | % {
    if($i -lt $parameters.Length){
        $qr = 0
        if($parameters[$i+0].StartsWith("q")) { $drawcircle = $true; $qr = $QS[$ji-1]; }
        $qt = 0
        if($parameters[$i+1].StartsWith("q")) { $drawcircle = $false; $qt = $QS[$ji-1]; }

        Write-Output "currentjoint = tcurrent*zero;"
        Write-Output "xx = tcurrent*(X*0.1);"
        Write-Output "yy = tcurrent*(Y*0.1);"
        Write-Output "zz = tcurrent*(Z*0.1);"
        Write-Output "draw(currentjoint--xx,red);"
        #Write-Output "draw(currentjoint--yy,green);"
        Write-Output "draw(currentjoint--zz,blue);"
        if($drawcircle){
            Write-Output "draw(circle(currentjoint, 0.1, (zz.x-currentjoint.x,zz.y-currentjoint.y,zz.z-currentjoint.z)),dotted+0.5bp+blue);"
        }else{
            Write-Output "draw(tcurrent*scale(0.1,0.1,$qt)*shift(-0.5,-0.5,0)*unitbox, dashed+blue);"            
        }
        Write-Output "triple j$ji = currentjoint;"
        Write-Output "transform3 j$($ji)rz = rotate($qr,(0,0,1));"
        Write-Output "transform3 j$($ji)tz = shift(0,0,$qt);"
        Write-Output "transform3 j$($ji)tx = shift($($parameters[$i+2]),0,0);"
        Write-Output "transform3 j$($ji)rx = rotate($($parameters[$i+3]),(1,0,0));"
        #Write-Output "currentjoint = j$($ji)rx*j$($ji)tx*j$($ji)tz*j$($ji)rz*currentjoint;"
        Write-Output "tcurrent = tcurrent*j$($ji)rz*j$($ji)tz*j$($ji)tx*j$($ji)rx;"
        
        $ji = $ji + 1
        $i = $i + 4
    }
}

Write-Output "currentjoint = tcurrent*zero;"
Write-Output "xx = tcurrent*(X*0.1);"
#Write-Output "yy = tcurrent*(Y*0.1);"
Write-Output "zz = tcurrent*(Z*0.1);"
Write-Output "triple j$ji = tcurrent*zero;"
Write-Output "draw(currentjoint--xx,red);"
#Write-Output "draw(currentjoint--yy,green);"
Write-Output "draw(currentjoint--zz,blue);"

$joints = 1..($ji) | % { "j$_" } | ToArray
Write-Output "draw((0,0,0)--$([System.String]::Join("--", $joints)));"

Write-Output "limits(O,X+Y+Z);"
Write-Output "xaxis3(Label(""`$x$"",1),Arrow3);"
Write-Output "yaxis3(Label(""`$y$"",1),Arrow3);"
Write-Output "zaxis3(Label(""`$z$"",1),Arrow3);"
