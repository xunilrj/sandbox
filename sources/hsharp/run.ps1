param([string]$Run = $null)

Push-Location
Set-Location (Split-Path $PSCommandPath -Parent)
[System.Environment]::CurrentDirectory = (gl)

if([System.String]::IsNullOrEmpty($Run)){
    if([System.IO.File]::Exists(".\.build\msvc\Release\hsharp.exe")){
        Write-Verbose "Found Release Build"
        $Run = "Release"
    }elseif([System.IO.File]::Exists(".\.build\msvc\Debug\hsharp.exe")){
        Write-Verbose "Found Debug Build"
        $Run = "Debug"
    }else{
        Write-Warning "No build found!"
    }
}

if($Run -eq "Debug"){
    .\.build\msvc\Debug\hsharp.exe
}elseif($Run -eq "Release"){
    .\.build\msvc\Release\hsharp.exe
}

Pop-Location