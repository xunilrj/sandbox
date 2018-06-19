gci *.asy|%{ @{File=$_.FullName;Out=$_.Name.Replace(".asy","").Replace(".","")+".pdf"} }|% {
    Write-Verbose "$($_.File) -> $($_.Out)"
    asy $_.File -o $_.Out -f pdf -noprc -render=0 -noView
}