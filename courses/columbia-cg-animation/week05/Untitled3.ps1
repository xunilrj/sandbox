$map = @"
0 0 0
0 1 0
0 1 0
"@;
$x = 0
$y = 0
$particleDic = @{}
$map.Split(" `r`n", [System.StringSplitOptions]::RemoveEmptyEntries) | % {
    $tile = $_.Trim()
    if($tile -eq "1"){
        $particleDic["$x$y"]
        "<particle px=`"$x`" py=`"$y`" /> "
    }
    $x=$x+1
    if($x -eq 3) {$y=$y+1;$x=0;}
}