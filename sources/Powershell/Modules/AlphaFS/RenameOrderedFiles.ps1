function RenameOrder($Path){
    Push-Location

    if([System.String]::IsNullOrEmpty($Path)){
        $Path = gl
    }

    cd $Path
    $i = 0
    dir . | % {
        $i = $i + 1
        Write-Verbose $i

        $result = $_.Name -match "^\d+"
        $length = $Matches[0].Length
        
        $newName = [System.Text.RegularExpressions.Regex]::Replace($_.Name, '^\d+', $i.ToString("d$length"))
        $newPath = [System.IO.Path]::Combine($_.Directory, $newName);

        Move-Item $_.FullName $newPath 
    }

    Pop-Location
}