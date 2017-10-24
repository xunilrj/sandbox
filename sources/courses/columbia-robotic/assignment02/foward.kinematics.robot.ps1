param ($DH)


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

Write-Output "pkg load symbolic"

$parameters = $DH -split " "
$i = 0
$ji = 1
1..100 | % {
    if($i -lt $parameters.Length){
        $qi = "q$ji"
        $qr = 0
        if($parameters[$i+0].StartsWith("q")) { $qr = $qi }        
        $qt = 0
        if($parameters[$i+1].StartsWith("q")) { $drawcircle = $false; }

        
        $tx = $parameters[$i+2]
        $rz = $parameters[$i+3]
        $qxradians = 0
        if($rz -ne 0) { $qxradians = (2.0*[System.Math]::PI)/($rz) }

        Write-Output "syms q$ji"        
        Write-Output "j$($ji)rz = [[cos($qr), -sin($qr), 0, 0]; [sin($qr), cos($qr), 0, 0]; [0, 0, 1, 0]; [0,0,0,1]]";
        Write-Output "j$($ji)tz = [[1, 0, 0, 0]; [0, 1, 0, 0]; [0, 0, 1, q1]; [0,0,$qt,1]]";
        Write-Output "j$($ji)tx = [[1, 0, 0, 0]; [0, 1, 0, 0]; [0, 0, 1, q1]; [$tx,0,0,1]]";
        Write-Output "j$($ji)rx = [[1, 0, 0, 0]; [0, cos($qxradians), -sin($qxradians), 0]; [0, sin($qxradians), cos($qxradians), 0]; [0,0,0,1]]";        
        Write-Output "line$ji = j$($ji)rz*j$($ji)tz*j$($ji)tx*j$($ji)rx"

        $ji = $ji + 1
        $i = $i + 4
    }   
}

$joints = 1..($ji-1) | % { "line$_" } | ToArray
Write-Output "T = $([System.String]::Join("*", $joints))"