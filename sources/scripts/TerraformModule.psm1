function Update-TerraformScript
{
    [CmdletBinding(SupportsShouldProcess=$true,ConfirmImpact="High")]
    param([Parameter(Position=0)][string[]]$Environment,[Switch]$Force)
    begin{
        Push-Location
    }
    end{
        Pop-Location
    }
    process{
        $Environment | ForEach-Object {
            cd $_

            Write-Verbose "Planning: $(gl)"

            $lastId = Get-ChildItem . -Filter *.tfplan -Recurse | 
                Select-Object Name | 
                Sort -Descending | 
                Select-String "\.(?<NAME>\d+)\." | 
                ForEach-Object {$_.Matches[0].Groups["NAME"].Value} | 
                Select-Object -first 1

            if($null -eq $lastId){
                $lastId = "00000001"
            }else{
                $lastId = ([System.Int32]::Parse($lastId) + 1).ToString("d8")
            }

            $tempPlanName = [System.IO.Path]::GetTempFileName()
            $planName = ".\$Environment\$Environment.$lastId.tfplan"
            terraform plan -detailed-exitcode -out $tempPlanName *>&1 | ForEach-Object {Write-Verbose $_}

            if($LASTEXITCODE -eq 2){
                Write-Information "Change detected"
                Write-Verbose "See $tempPlanName"

                if ($Force -or $pscmdlet.ShouldProcess("See $tempPlanName","Can terraform apply plan $tempPlanName","Terraform will apply")) {
                    Write-Verbose "Applying: $Environment"
                    Copy-Item $tempPlanName $planName
                    terraform apply -state $_ $planName  *>&1 | ForEach-Object {Write-Verbose $_}                
                }else{
                    Write-Verbose "Apply canceled"
                }

            }elseif($LASTEXITCODE -eq 0){
                Write-Information "No Changes detected"
            }else{
                Write-Error "Terraform found an error"
            }
        }
    }
}