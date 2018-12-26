gci *.json |% {
    gc $_ | .\searches\Debug\searches.exe
}

function InsertLineNumber
{    
    param($Format)
    begin{$i = 0}
    process
    {
        "$i $_";
        $i = $i+1;
    }
}