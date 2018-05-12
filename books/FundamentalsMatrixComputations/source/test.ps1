param($Test)
.\build.ps1 *>&1 | % {Write-Verbose $_}
.\.build\Debug\la.exe $Test -r console -d yes