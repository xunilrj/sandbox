Push-Location
cd (Split-Path $PSCommandPath -Parent)

try
{
    .\.paket\paket.bootstrapper.exe
    if ($LASTEXITCODE -ne 0){
        Write-Error "Paket bootstrapper failed"
        return
    }

    $result = Test-Path .\paket.lock
    if($result -eq $false){
        .\.paket\paket.exe install 
    }
    
    .\.paket\paket.exe restore
    if ($LASTEXITCODE -ne 0){
        Write-Error "Paket restore failed"
        return
    }    

    .\packages\FAKE\tools\FAKE.exe build.fsx $args
}
catch
{
}
finally
{
    Pop-Location
}