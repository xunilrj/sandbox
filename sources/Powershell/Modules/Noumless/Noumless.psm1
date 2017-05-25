function ine
{
    [CmdletBinding()]
    param([Parameter(Position=0)][string]$Expression,[Parameter(ValueFromPipeline=$true)]$Item)
    process{
        $pipe = [System.Collections.Generic.List[string]]::new()
        $pipe.Add("`$_")
        [System.Linq.Enumerable]::ToArray($Expression)|%{
            if($_ -eq "g"){
                $pipe.Add("get")
            }elseif($_ -eq "s"){
                $pipe.Add("stop")
            }
        }

        $cmd = [System.String]::Join("|", [System.Linq.Enumerable]::ToArray($pipe))
        Write-Verbose $cmd
        iex $cmd
    }
}

function get
{
    [CmdletBinding()]
    param([Parameter(ValueFromPipeline=$true)]$Path)
    process{
        $uri = [System.Uri]::new($Path)
        iex "Get-$($uri.Scheme) $($uri.Segments[0])" | Add-Member -Name PSNoumlessScheme -MemberType NoteProperty -Value $uri.Scheme -PassThru
    }
}

function stop
{
    [CmdletBinding()]
    param([Parameter(ValueFromPipeline=$true)]$Item)
    process{
        if($Item.PSNoumlessScheme -ne $null){
            iex "`$_ | Stop-$($Item.PSNoumlessScheme)"
        }
    }    
}