function Install-NPMPackage
{
    [CmdletBinding()]
    param([string[]]$Name,[Switch]$Global, [Switch]$Development)
    $Name | %{
        $param = ""
        if($Global.IsPresent){
            $param = "--global"
        }
        elseif($Development.IsPresent){
            $param = "--save-dev"
        }
        else{
            $param = "--save"
        }

        Write-Verbose "Installing NPM package $_ with param $param"
        npm install $_ $param *>&1 | %{Write-Verbose $_}
    }
}

function Start-NPM
{
    $name = (New-Object System.IO.FileInfo ((gl).Path)).Name        
    '{"name":"' + $name + '","version":"1.0.0","dependencies":{},"devDependencies":{}}' | Out-File ".\package.json" -Encoding ascii -Force
}

function Restore-NPM
{
    $out = [System.IO.Path]::GetTempFileName()
    $err = [System.IO.Path]::GetTempFileName()

    try
    {
        start npm "install" -Wait -NoNewWindow -RedirectStandardOutput $out -RedirectStandardError $err
        gc $out | % { Write-Verbose $_ }
        gc $err | % { Write-Verbose $_ }
    }
    finally
    {
        ri $out -Force
        ri $err -Force
    }
}