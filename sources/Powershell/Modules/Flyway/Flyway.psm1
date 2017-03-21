function Invoke-Application
{
    [CmdletBinding()]
    param([Parameter(Position=0)]$Command,
    [Parameter(ValueFromPipelineByPropertyName=$true)]$ServerInstance,
    [Parameter(ValueFromPipelineByPropertyName=$true)]$Database,
    [Parameter(ValueFromPipelineByPropertyName=$true)]$Username,
    [Parameter(ValueFromPipelineByPropertyName=$true)]$Password,
    [Parameter(ValueFromPipeline=$true)]$PSItem,
    [switch]$PassThru
    )
    process{

        if($ServerInstance -eq "."){
            $ServerInstance = "localhost"
        }

        $exe = Join-Path (Split-Path $PSCommandPath -Parent) "flyway/flyway.cmd"
        $jarDirs = Join-Path (Split-Path $PSCommandPath -Parent) "flyway/flyway.cmd"

        $stringBuilder = [System.Text.StringBuilder]::new()
        . $exe $Command -url=jdbc:jtds:sqlserver://$($ServerInstance):1433/$Database -user="$Username" -password="$Password" -locations="filesystem:$Path" *>&1 | %{
            $stringBuilder.AppendLine($_) | Out-Null
        }

        if($PassThru.IsPresent){
            $PSItem
        }else{
            $stringBuilder.ToString()
        }
    }
}

function Get-Database
{
    [CmdletBinding()]
    param(
    [Parameter(ValueFromPipelineByPropertyName=$true)]$Path, 
    [Parameter(ValueFromPipelineByPropertyName=$true)]$ServerInstance,
    [Parameter(ValueFromPipelineByPropertyName=$true)]$Database,
    [Parameter(ValueFromPipelineByPropertyName=$true)]$Username,
    [Parameter(ValueFromPipelineByPropertyName=$true)]$Password,
    [Parameter(ValueFromPipeline=$true)]$PSItem,
    [switch]$PassThru
    )
    process{
        $PSItem | Invoke-Application info -PassThru:$PassThru
    }
}

function Checkpoint-Database
{
    [CmdletBinding()]
    param(
    [Parameter(ValueFromPipelineByPropertyName=$true)]$Path, 
    [Parameter(ValueFromPipelineByPropertyName=$true)]$ServerInstance,
    [Parameter(ValueFromPipelineByPropertyName=$true)]$Database,
    [Parameter(ValueFromPipelineByPropertyName=$true)]$Username,
    [Parameter(ValueFromPipelineByPropertyName=$true)]$Password,
    [Parameter(ValueFromPipeline=$true)]$PSItem,
    [switch]$PassThru
    )
    process{
        $PSItem | Invoke-Application baseline -PassThru:$PassThru
    }
}

function Update-Database
{
    [CmdletBinding()]
    param(
    [Parameter(ValueFromPipelineByPropertyName=$true)]$Path, 
    [Parameter(ValueFromPipelineByPropertyName=$true)]$ServerInstance,
    [Parameter(ValueFromPipelineByPropertyName=$true)]$Database,
    [Parameter(ValueFromPipelineByPropertyName=$true)]$Username,
    [Parameter(ValueFromPipelineByPropertyName=$true)]$Password,
    [Parameter(ValueFromPipeline=$true)]$PSItem,
    [switch]$PassThru
    )
    process{
        $PSItem | Invoke-Application migrate -PassThru:$PassThru
    }
}

function Delete-DatabaseCheckpoint
{
     [CmdletBinding()]
    param(
    [Parameter(ValueFromPipelineByPropertyName=$true)]$Path, 
    [Parameter(ValueFromPipelineByPropertyName=$true)]$ServerInstance,
    [Parameter(ValueFromPipelineByPropertyName=$true)]$Database,
    [Parameter(ValueFromPipelineByPropertyName=$true)]$Username,
    [Parameter(ValueFromPipelineByPropertyName=$true)]$Password,
    [Parameter(ValueFromPipeline=$true)]$PSItem,
    [switch]$PassThru
    )
    process{
        Invoke-Sqlcmd -Query "DROP TABLE schema_version" -HostName $ServerInstance -Database $Database -Username $Username -Password $Password
    }
}

Export-ModuleMember -Function Get-Database, Checkpoint-Database, Update-Database, Delete-DatabaseCheckpoint