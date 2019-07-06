Import-Module C:\tools\llvm\llvm.psm1 -Force -DisableNameChecking
Set-llvm C:\tools\llvm\v8.0.0

clang++ -std=c++1z -fprofile-instr-generate -fcoverage-mapping  ./sources/main.cpp

rm ./default.profdata
rm ./default.profraw

$obj = New-Object PSCustomObject
./a.exe -l | sls -Pattern "^\s\s[^\s]"|% {
    Write-Verbose $_
    ./a.exe --name $_ | Out-Null
    llvm-profdata merge -sparse default.profraw -o default.profdata
    $res = llvm-cov export ./a.exe -instr-profile .\default.profdata | ConvertFrom-Json
    $res.data.files | % {
        $fileName = $_.Filename.ToLower()
        $decorations = [System.Collections.ArrayList]::new()
        for($i = 0;$i -lt $_.segments.Length; $i=$i+1)
        {
            if($_.segments[$i+0][3] -eq $true){
                if($_.segments[$i+0][2] -eq 0){
                continue;
                }
            }
            else
            {
                continue;
            }
            $lineStart = [int]::Parse($_.segments[$i+0][0].ToString()) - 1
            $offsetStart = [int]::Parse($_.segments[$i+0][1].ToString()) - 1
        
            if($_.segments[$i+1].Length -eq 0) {
                $lineEnd = $lineStart
                $offsetEnd = $offsetStart + 1
                continue
            } else {
                $lineEnd = [int]::Parse($_.segments[$i+1][0].ToString()) - 1
                $offsetEnd = [int]::Parse($_.segments[$i+1][1].ToString()) - 1
            }
            
            
            $decorations.Add(@("d","good", $lineStart, $offsetStart, $lineEnd, $offsetEnd)) | Out-Null
        }
        if( ($obj | Get-Member -Name $fileName) -eq $null){
            $obj | Add-Member NoteProperty -Name $fileName -Value $decorations
        } else {
            $list = $obj | Get-Member
            $decorations | % {
                $obj.$fileName.Add($_) | Out-Null
            }
        }
    }
}

$obj | ConvertTo-Json | Out-File ./decorations.json -Encoding utf8