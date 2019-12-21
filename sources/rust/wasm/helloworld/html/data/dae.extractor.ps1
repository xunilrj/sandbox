Import-Module Powershell.Chunks

$path = "C:\Users\xunil\Downloads\models\ty.walking.001\Walking.dae"
$model = [xml](cat $path)

#$scenes = $model.COLLADA.scene.instance_visual_scene
#$scenes|% {
#    $url = $scenes.url.replace("#","")#
#
#    $scene = $model.COLLADA.library_visual_scenes.visual_scene|? id -eq $url
#    $scene.node
#}

cd "C:\Users\xunil\Downloads\models\ty.walking.001"

$model.COLLADA.library_geometries.geometry|% {
    $geom = $_
    
    $m = ($geom.mesh.polylist.input | Measure-Object).Count
    $p = $geom.mesh.polylist.p.Split(@(' '), "RemoveEmptyEntries")|
        %{[int]::Parse($_)}|
        Get-Chunk $m
    
    $i = 0
    $index = @()
    $indexi = -1
    $dic = New-Object 'System.Collections.Generic.Dictionary[String,Int32]'
    $vi = New-Object 'System.Collections.Generic.Dictionary[Int32,Int32]'
    $geom.mesh.polylist.vcount.Split(@(' '), "RemoveEmptyEntries")|% {
        $count = [int]::Parse($_);
        if($count -eq 3) {
            $a = $p[$i];$i=$i+1; 

            $key = (($a|%{$_}) -join "-");
            if($dic.ContainsKey($key)) { $indexi = $dic[$key]; }
            else {
                $indexi = $indexi + 1;
                $dic.Add($key, $indexi);
            }
            if($vi.ContainsKey($indexi) -eq $false) { $vi.Add($indexi, $a[0]); }
            $index += $indexi;
        }    
    }
    $dic

    Write-Host "Index: $($index.Count) $($vi.Count)"

    $dir = "C:\github\sandbox\sources\rust\wasm\helloworld\html\data\ty\"
    mkdir $dir -Force | Out-Null
    $path = $dir + ($geom.id) + ".index";
    rm $path -Force -EA SilentlyContinue; rm ($path+".txt") -Force -EA SilentlyContinue;
    Write-Host $path
    $s = [System.IO.File]::Open($path, "Create");
    $w = [System.IO.BinaryWriter]::new($s);
    $ss = [System.IO.File]::Open($path+".txt", "Create");
    $ws = [System.IO.StreamWriter]::new($ss);
    $index |% { $w.Write($_); $ws.Write($_.ToString() + " "); }
    $w.Dispose(); $ws.Dispose();

    $verticeId = ($geom.mesh.polylist.input|? semantic -eq "VERTEX").source.replace("#","")
    $verticesEl = $geom.mesh.vertices|? id -eq $verticeId
    $sourceId = $verticesEl.input.source.replace("#","")
    $source = $geom.mesh.source|? id -eq $sourceId
    $vertices = $source.float_array.'#text'.Split(@(' '), "RemoveEmptyEntries")|% { [float]::Parse($_) };
    if($vertices -eq $null) {
        continue;
    }
    $path = $dir + ($geom.id) + ".vertices";
    rm $path -Force -EA SilentlyContinue; rm ($path+".txt") -Force -EA SilentlyContinue;
    Write-Host $path;
    $s = [System.IO.File]::Open($path, "Create");
    $w = [System.IO.BinaryWriter]::new($s);
    $ss = [System.IO.File]::Open($path + ".txt", "Create");
    $ws = [System.IO.StreamWriter]::new($ss);
    $vi.Values|% {
        $w.Write([float]$vertices[$_*3+0]);
        $w.Write([float]$vertices[$_*3+1]);
        $w.Write([float]$vertices[$_*3+2]);

        $ws.Write($vertices[$_*3+0].ToString() + " ");
        $ws.Write($vertices[$_*3+1].ToString() + " ");
        $ws.Write($vertices[$_*3+2].ToString() + " ");
    }
    $w.Dispose(); $ws.Dispose();
}