function Start-Work
{
    $selectedBranch =git branch | % { $result = $_ -match "[\s*][\s*](?<BRANCHNAME>.*)$"; $Matches["BRANCHNAME"] } | ogv -PassThru -Title "Select the source branch"

    if(($selectedBranch -eq $null) -or ($selectedBranch | Measure-Object).Count -ne 1)
    {
        Write-Error "Select just one branch to be the source"
    }

    $branchType = "feature","hotfix","test","build" | ogv -PassThru -Title "Select the branch type"

    if(($selectedBranch -eq $null) -or ($selectedBranch | Measure-Object).Count -ne 1)
    {
        Write-Error "Select just one branch type"
    }

    $branchName = Read-Host -Prompt "Branch Name"

    Write-Verbose "Source Branch: $selectedBranch"
    Write-Verbose "Branch Type: $branchType"
    Write-Verbose "Branch Name: $branchName"

    git checkout $selectedBranch
    git pull upstream $selectedBranch
    git push origin HEAD
    git checkout -b $branchType/$branchName
}

function Get-CurrentBranch
{
    git branch | % { $result = $_ -match "^\*\s(?<MATCHNAME>.*)$"; if($result) {$Matches["MATCHNAME"]} }
}

function Rebase-Work
{
    Add-Files

    Write-Verbose "git stash"
    git stash *>&1 | %{Write-Verbose $_}
    $currentBranch = Get-CurrentBranch
    Write-Verbose "git checkout master"
    git checkout master *>&1 | %{Write-Verbose $_}
    Write-Verbose "git pull upstream master"
    git pull upstream master *>&1 | %{Write-Verbose $_}
    Write-Verbose "git pull upstream"
    git pull upstream *>&1 | %{Write-Verbose $_}
    Write-Verbose "git push origin HEAD"
    git push origin HEAD *>&1 | %{Write-Verbose $_}
    Write-Verbose "git checkout $currentBranch"
    git checkout $currentBranch *>&1 | %{Write-Verbose $_}
    Write-Verbose "git rebase master"
    git rebase master *>&1 | %{Write-Verbose $_}

    $response = read-host "Press [enter] after conflict were resolved"

    Write-Verbose "git push origin HEAD -f"
    git push origin HEAD -f *>&1 | %{Write-Verbose $_}
    Write-Verbose "git stash apply"
    git stash apply *>&1 | %{Write-Verbose $_}
}

function Add-Files
{
    $modifiedFiles = git status --porcelain | % {$result = $_ -match "(?<STATUS>..)\s(?<FILEPATH>.*)$"; if($result){New-Object PSCustomObject -Property @{Status=$Matches["STATUS"];File=$Matches["FILEPATH"]}}}
    $filesToCommit = $modifiedFiles | ogv -PassThru
    $filesToCommit  | % {git add $_.File *>&1 } | % {Write-Verbose $_}
}

function Commit-Work
{
    $files = Add-Files

    $message = Read-Host "Commit Message"

    git commit -m $message *>&1 | % {Write-Verbose $_}
    git push origin HEAD | % {Write-Verbose $_}
}

function Pull-Work
{
    Write-Verbose "Rebasing work"
    Rebase-Work

    Write-Verbose "Commiting work"
    Commit-Work

    $response = read-host "Press [enter] to after the PR is accepted"

    Write-Verbose "Getting PR"
    Rebase-Work
}