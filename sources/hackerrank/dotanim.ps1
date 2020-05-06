param($Path, $Name)
set-alias dot "C:\Program Files (x86)\Graphviz2.38\bin\dot.exe"

[System.Environment]::CurrentDirectory = (gl).Path
$lines = [System.IO.File]::ReadAllLines($Path)

rm $Name -Force -Recurse -EA SilentlyContinue
mkdir $Name -EA SilentlyContinue | Out-Null
pushd $Name

$accum = "";
$i = 0;
$lines | % -Process {
    Write-Progress -Activity "Generating animation" -CurrentOperation $i -PercentComplete (($i/[float]$lines.length)*100.0)
    $accum += $_ + [System.Environment]::NewLine;
    $dotfile = @"
digraph graphname {
rank1 [style=invisible];
rank2 [style=invisible];
rank1 -> rank2 [color=white];
rank2 -> 1 [ style=invis ];
"@;
    $dotfile += $accum;
    $dotfile += "}";

    [System.Environment]::CurrentDirectory = (gl).Path
    [System.IO.File]::WriteAllText("$i.txt", $dotfile);
    iex "dot -Tpng $i.txt -o $i.png"
    $i += 1
}
popd
