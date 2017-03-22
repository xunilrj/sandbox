
function Get-GitHubModule{
    param([Parameter(Mandatory=$true, Position = 0, ValueFromPipeline=$true)][string[]]$RepoUrl)
    process{
        $RepoUrl | % {
            $currentRepo = $_
            $repo = Invoke-RestMethod "https://api.github.com/repos/$currentRepo/contents"
            $repo | ? type -eq dir | % {
                $files = Invoke-RestMethod $_.url
                $result = $files | ? name -Like *.psm1 | Measure-Object
                if($result.Count -eq 1){
                    New-Object PsCustomObject -Property @{
                        RepoUrl = $RepoUrl;
                        Name = $_.name;
                    }
                }
            }
        }
    }
}

function Download
{
    param([Parameter(Position=0)]$RootPath, [Parameter(ValueFromPipeline=$true)]$Item)
    begin{
        mkdir $RootPath -EA SilentlyContinue
    }
    process{
        if($Item.type -eq "file") {
            Write-Verbose "Downloading $($_.name)"
            $filePath = Join-Path $RootPath $_.name
            Write-Verbose "Downloading $($_.name) to $filePath"
            Invoke-WebRequest $_.url -OutFile "$filePath"
        }

        if($Item.type -eq "dir") {
            $newRootPath = Join-Path $RootPath $_.name
            $files = Invoke-RestMethod $_.url
            $files | Download $newRootPath
        }
     }
}

function Install-GitHubModule
{
    param([Parameter(Mandatory=$true, Position = 0, ValueFromPipeline=$true)][string[]]$RepoUrl)
    
    $RepoUrl | % {
            $currentRepo = $_
            $repo = Invoke-RestMethod "https://api.github.com/repos/$currentRepo/contents"
            $repo | ? type -eq dir | % {
                $files = Invoke-RestMethod $_.url
                $result = $files | ? name -Like *.psm1 | Measure-Object
                if($result.Count -eq 1){
                    $userProfile = [System.Environment]::GetFolderPath("UserProfile")
                    $folderPath = "$userProfile\Documents\WindowsPowerShell\Modules\$($_.name)"
                    $files | Download $folderPath
                }
            }
        }
}

Export-ModuleMember Get-GitHubModule, Install-GitHubModule