cat .print | %{
    $r = $_ -match "=([\d]+)-...=(\d+)$"; 
    if ($r) { 
        New-Object -TypeName PSObject -Property @{mem=$Matches[1];pos=$Matches[2];line=$_.ToString()}
    }
} | ConvertTo-Json > b.txt
cat .print | %{
    $r = $_ -match "shr ...,(\d+) - ECX=(\d+)$"; 
    if ($r) { 
        New-Object -TypeName PSObject -Property @{mem=$Matches[2];pos=$Matches[1];line=$_.ToString()}
    }
} | ConvertTo-Json >> b.txt