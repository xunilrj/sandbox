cd (Split-Path $PSCommandPath -Parent)

function Out-Plot2
{
    param($F1,$F2,$F3,$F4,$File)
    $outFile = $File.Replace('\',"/")
    $file = [System.IO.Path]::GetTempFileName()
@"
png(filename="$outFile")
plot(sapply(1:1000,function(x){$F1}), type='l', xlab="x", ylab="y", col="blue")
grid (10,10, lty = 6, col = "cornsilk2")
lines(sapply(1:1000, function(x){$F2}), type='l', xlab="x", ylab="y", col="yellow")
lines(sapply(1:1000, function(x){$F3}), type='l', lty=2, xlab="x", ylab="y", col="green")
lines(sapply(1:1000, function(x){$F4}), type='l', lty=3, xlab="x", ylab="y", col="red")
legend('topright', c("f(x)=$F1","g(x)=$F2","f(x)/g(x)","g(x)/f(x)"), lty=1, col=c('blue', 'yellow', 'green', 'red'), bty='n', cex=.75)
dev.off()
"@ | Out-FileUtf8NoBom $file
    gc $file

    & 'C:\Program Files\Microsoft\MRO-3.3.2\bin\Rscript.exe' $file
}

gc .\0.1.functions.csv | ConvertFrom-Csv -Delimiter ";" | % {
    Out-Plot2 $_.F1 $_.F2 "($($_.F1))/($($_.F2))" "($($_.F2))/($($_.F1))" (Join-Path (Resolve-Path .) "0_$($_.i).png")
}

