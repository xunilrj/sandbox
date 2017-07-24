cd (Split-Path $PSCommandPath -Parent)
ri .\imdb_tr.csv -EA SilentlyContinue
$rowNumber = 0
gci .\aclImdb\train\pos -Filter *.txt | % {
    New-Object PSCustomObject -Property @{
        text = cat $_.FullName;
        polarity = "1";
        rownumber = $rowNumber
    }
    $rowNumber += 1
} | Export-Csv ./imdb_tr.csv -NoTypeInformation