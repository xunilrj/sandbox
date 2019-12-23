Import-Module Powershell.Chunks

$Assem = (
    'System.Drawing, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a'   
 )
$Source = @”
using System;
using System.Drawing;
using System.Drawing.Imaging;
using System.Runtime.InteropServices;
public class LockBitmap
{
    Bitmap source = null;
    IntPtr Iptr = IntPtr.Zero;
    BitmapData bitmapData = null;
 
    public byte[] Pixels { get; set; }
    public int Depth { get; private set; }
    public int Width { get; private set; }
    public int Height { get; private set; }
 
    public LockBitmap(Bitmap source)
    {
        this.source = source;
    }
 
    /// <summary>
    /// Lock bitmap data
    /// </summary>
    public void LockBits()
    {
        try
        {
            // Get width and height of bitmap
            Width = source.Width;
            Height = source.Height;
 
            // get total locked pixels count
            int PixelCount = Width * Height;
 
            // Create rectangle to lock
            Rectangle rect = new Rectangle(0, 0, Width, Height);
 
            // get source bitmap pixel format size
            Depth = System.Drawing.Bitmap.GetPixelFormatSize(source.PixelFormat);
 
            // Check if bpp (Bits Per Pixel) is 8, 24, or 32
            if (Depth != 8 && Depth != 24 && Depth != 32)
            {
                throw new ArgumentException("Only 8, 24 and 32 bpp images are supported.");
            }
 
            // Lock bitmap and return bitmap data
            bitmapData = source.LockBits(rect, ImageLockMode.ReadWrite, 
                                         source.PixelFormat);
 
            // create byte array to copy pixel values
            int step = Depth / 8;
            Pixels = new byte[PixelCount * step];
            Iptr = bitmapData.Scan0;
 
            // Copy data from pointer to array
            Marshal.Copy(Iptr, Pixels, 0, Pixels.Length);
        }
        catch (Exception ex)
        {
            throw ex;
        }
    }
 
    /// <summary>
    /// Unlock bitmap data
    /// </summary>
    public void UnlockBits()
    {
        try
        {
            // Copy data from byte array to pointer
            Marshal.Copy(Pixels, 0, Iptr, Pixels.Length);
 
            // Unlock bitmap data
            source.UnlockBits(bitmapData);
        }
        catch (Exception ex)
        {
            throw ex;
        }
    }
 
    /// <summary>
    /// Get the color of the specified pixel
    /// </summary>
    /// <param name="x"></param>
    /// <param name="y"></param>
    /// <returns></returns>
    public Color GetPixel(int x, int y)
    {
        Color clr = Color.Empty;
 
        // Get color components count
        int cCount = Depth / 8;
 
        // Get start index of the specified pixel
        int i = ((y * Width) + x) * cCount;
 
        if (i > Pixels.Length - cCount)
            throw new IndexOutOfRangeException();
 
        if (Depth == 32) // For 32 bpp get Red, Green, Blue and Alpha
        {
            byte b = Pixels[i];
            byte g = Pixels[i + 1];
            byte r = Pixels[i + 2];
            byte a = Pixels[i + 3]; // a
            clr = Color.FromArgb(a, r, g, b);
        }
        if (Depth == 24) // For 24 bpp get Red, Green and Blue
        {
            byte b = Pixels[i];
            byte g = Pixels[i + 1];
            byte r = Pixels[i + 2];
            clr = Color.FromArgb(r, g, b);
        }
        if (Depth == 8)
        // For 8 bpp get color value (Red, Green and Blue values are the same)
        {
            byte c = Pixels[i];
            clr = Color.FromArgb(c, c, c);
        }
        return clr;
    }
 
    /// <summary>
    /// Set the color of the specified pixel
    /// </summary>
    /// <param name="x"></param>
    /// <param name="y"></param>
    /// <param name="color"></param>
    public void SetPixel(int x, int y, Color color)
    {
        // Get color components count
        int cCount = Depth / 8;
 
        // Get start index of the specified pixel
        int i = ((y * Width) + x) * cCount;
 
        if (Depth == 32) // For 32 bpp set Red, Green, Blue and Alpha
        {
            Pixels[i] = color.B;
            Pixels[i + 1] = color.G;
            Pixels[i + 2] = color.R;
            Pixels[i + 3] = color.A;
        }
        if (Depth == 24) // For 24 bpp set Red, Green and Blue
        {
            Pixels[i] = color.B;
            Pixels[i + 1] = color.G;
            Pixels[i + 2] = color.R;
        }
        if (Depth == 8)
        // For 8 bpp set color value (Red, Green and Blue values are the same)
        {
            Pixels[i] = color.B;
        }
    }
}
“@
Add-Type -ReferencedAssemblies $Assem -TypeDefinition $Source -Language CSharp 


$daeDir = "C:\Users\xunil\OneDrive\Desktop\models\ty.walking.001\"
$path = "$($daeDir)Walking.dae"
$model = [xml](cat $path)

#$scenes = $model.COLLADA.scene.instance_visual_scene
#$scenes|% {
#    $url = $scenes.url.replace("#","")#
#
#    $scene = $model.COLLADA.library_visual_scenes.visual_scene|? id -eq $url
#    $scene.node
#}

cd "C:\Users\xunil\OneDrive\Desktop\models\ty.walking.001"

function writeFloat($w, $v)
{
    #$arr = [BitConverter]::GetBytes([float]$v)
    #[Array]::Reverse($arr)
    #$w.Write($arr);
    $w.Write([float]$v);
}

function writeUint16($w, $v)
{
    #$arr = [BitConverter]::GetBytes([uint16]$v)
    #[Array]::Reverse($arr)
    #$w.Write($arr);
    $w.Write([uint16]$v);
}

function writeUint8($w, $v)
{
    #$arr = [BitConverter]::GetBytes([uint16]$v)
    #[Array]::Reverse($arr)
    #$w.Write($arr);
    $w.Write([byte]$v);
}

$dir = "D:\github\sandbox\sources\rust\wasm\helloworld\html\data\ty\"


#$model.COLLADA.library_images.image|% {
#    $_.init_from
#    $path = $daeDir + $_.init_from
#    $img = [System.Drawing.Bitmap]::new($path)
#    $lockBitmap = [LockBitmap]::new($img);
#    $lockBitmap.LockBits();
#    
#
#    $fileInfo = [System.IO.FileInfo]::new($path)
#    $texPath = $dir + $fileInfo.Name + ".tex";
#    $texPath
#    $s = [System.IO.File]::Open($texPath, "Create");
#    $w = [System.IO.BinaryWriter]::new($s);
#
#    $i = 0
#    for($x = 0;$x -lt $img.Width; $x++) {
#        for($y = 0;$y -lt $img.Width; $y++) {
#            $color = $lockBitmap.GetPixel($x,$y);
#
#            $w.Write([byte]$color.R);
#            $w.Write([byte]$color.G);
#            $w.Write([byte]$color.B);
#            $w.Write([byte]$color.A);
#            $i = $i + 1
#        }
#
#        $per = (100*[float]$i / [float]($img.Width*$img.Height))
#        Write-Progress -Activity "." -CurrentOperation "." -PercentComplete $per 
#    }
#
#    $lockBitmap.UnlockBits();    
#    $w.Dispose();
#}

$model.COLLADA.library_geometries.geometry|% {
    $geom = $_

    if($geom.id.Contains("Head") -eq $false) { continue; }
    
    $m = ($geom.mesh.polylist.input | Measure-Object).Count
    $p = $geom.mesh.polylist.p.Split(@(' '), "RemoveEmptyEntries")|
        %{[int]::Parse($_)}|
        Get-Chunk $m
    
    $i = 0
    $index = @()
    $indexi = -1
    $dic = New-Object 'System.Collections.Generic.Dictionary[String,Int32]'
    $vi = New-Object 'System.Collections.Generic.Dictionary[Int32,Int32]'
    $ti = New-Object 'System.Collections.Generic.Dictionary[Int32,Int32]'
    $geom.mesh.polylist.vcount.Split(@(' '), "RemoveEmptyEntries")|% {
        $count = [int]::Parse($_);
        if($count -eq 3) {
            1..3|%{
                $a = $p[$i];$i=$i+1; 

                $key = (($a|%{$_}) -join "-");
                if($dic.ContainsKey($key)) { $indexi = $dic[$key]; }
                else {
                    $indexi = $dic.Count;
                    $dic.Add($key, $indexi);
                }
                if($vi.ContainsKey($indexi) -eq $false) { $vi.Add($indexi, $a[0]); }
                if($ti.ContainsKey($indexi) -eq $false) { $ti.Add($indexi, $a[2]); }
                $index += $indexi;
            }
        }    
    }
    #Write-Host "p: $($p.Count) $($i)"
    #$dic.Keys
    #$vi|ft Key,Value

    Write-Host "Index: $($dic.Keys.Count) $($index.Count) $($vi.Count)"

    # INDICES

    
    mkdir $dir -Force | Out-Null
    $path = $dir + ($geom.id) + ".index";
    rm $path -Force -EA SilentlyContinue; rm ($path+".txt") -Force -EA SilentlyContinue;
    Write-Host $path
    $s = [System.IO.File]::Open($path, "Create");
    $w = [System.IO.BinaryWriter]::new($s);
    $ss = [System.IO.File]::Open($path+".txt", "Create");
    $ws = [System.IO.StreamWriter]::new($ss);
    $index |% { (writeUint16 $w $_); $ws.Write($_.ToString() + " "); }
    $w.Dispose(); $ws.Dispose();

    # VERTICES

    $verticeId = ($geom.mesh.polylist.input|? semantic -eq "VERTEX").source.replace("#","")
    $verticesEl = $geom.mesh.vertices|? id -eq $verticeId
    $sourceId = $verticesEl.input.source.replace("#","")
    $source = $geom.mesh.source|? id -eq $sourceId
    $vertices = $source.float_array.'#text'.Split(@(' '), "RemoveEmptyEntries")|% { [float]::Parse($_) };
    if($vertices -eq $null) { continue; }
    $path = $dir + ($geom.id) + ".vertices";
    rm $path -Force -EA SilentlyContinue; rm ($path+".txt") -Force -EA SilentlyContinue;
    Write-Host $path;
    $s = [System.IO.File]::Open($path, "Create");
    $w = [System.IO.BinaryWriter]::new($s);    
    $ss = [System.IO.File]::Open($path + ".txt", "Create");
    $ws = [System.IO.StreamWriter]::new($ss);
    $maxx = -999999; $minx = 999999;
    $maxy = -999999; $miny = 999999;
    $maxz = -999999; $minz = 999999;
    $vi.Values|% {
        $x = [float]$vertices[$_*3+0];
        $y = [float]$vertices[$_*3+1];
        $z = [float]$vertices[$_*3+2];

        if($x -gt $maxx) { $maxx = $x; }
        if($x -lt $minx) { $minx = $x; }
        if($y -gt $maxy) { $maxy = $y; }
        if($y -lt $miny) { $miny = $y; }
        if($z -gt $maxz) { $maxz = $z; }
        if($z -lt $minz) { $minz = $z; }
                
        (writeFloat $w $x)
        (writeFloat $w $y)
        (writeFloat $w $z)

        $ws.Write($vertices[$_*3+0].ToString() + " ");
        $ws.Write($vertices[$_*3+1].ToString() + " ");
        $ws.Write($vertices[$_*3+2].ToString() + " ");
        $ws.Write("`n");
    }
    $ws.Write("Min X: $($minx)`n"); $ws.Write("Max X: $($maxx)`n");
    $ws.Write("Min Y: $($miny)`n"); $ws.Write("Max Y: $($maxy)`n");
    $ws.Write("Min Z: $($minz)`n"); $ws.Write("Max Z: $($maxz)`n");
    $w.Dispose(); $ws.Dispose();

    # TEXCOORDS

    $sourceId = ($geom.mesh.polylist.input|? semantic -eq "TEXCOORD").source.replace("#","")
    $source = $geom.mesh.source|? id -eq $sourceId
    $texcoords = $source.float_array.'#text'.Split(@(' '), "RemoveEmptyEntries")|% { [float]::Parse($_) };
    if($texcoords -eq $null) { continue; }
    $path = $dir + ($geom.id) + ".texcoords";
    rm $path -Force -EA SilentlyContinue; rm ($path+".txt") -Force -EA SilentlyContinue;
    Write-Host $path;
    $s = [System.IO.File]::Open($path, "Create");
    $w = [System.IO.BinaryWriter]::new($s);    
    $ss = [System.IO.File]::Open($path + ".txt", "Create");
    $ws = [System.IO.StreamWriter]::new($ss);
    $ti.Values|% {
        $u = [float]$texcoords[$_*2+0];
        $v = [float]$texcoords[$_*2+1];

        (writeFloat $w $u)
        (writeFloat $w $v)

        $ws.Write($u.ToString() + " ");
        $ws.Write($v.ToString() + " ");
        $ws.Write("`n");
    }
    $w.Dispose(); $ws.Dispose();
}
