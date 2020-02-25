param($Command, $Path)

set-alias glslc D:\tools\VulkanSDK\1.1.121.2\Bin\glslc.exe

$fi = [System.IO.FileInfo]::new($path)
$spvPath = $Path.replace($fi.Extension, ".spv");

if($Command -eq "vert")
{
    glslc -fshader-stage=vertex $Path -o $spvPath
}
elseif ($Command -eq "frag")
{
    glslc -fshader-stage=fragment $Path -o $spvPath
}
else {
    Write-Error "Unknown command";
}