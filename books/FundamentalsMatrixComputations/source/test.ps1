.\build.ps1 *>&1 | % {Write-Verbose $_}
.\.build\Debug\la.exe -r console -d yes