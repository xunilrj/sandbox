function Out-Plot
{
    param($F1,$F2,$File)
    $outFile = $File.Replace('\',"/")
    $file = [System.IO.Path]::GetTempFileName()
@"
png(filename="$outFile")
plot(sapply(1:1000,function(x){$F1}), type='l', xlab="x", ylab="y", col="blue")
grid (10,10, lty = 6, col = "cornsilk2")
lines(sapply(1:1000, function(x){$F2}), type='l', xlab="x", ylab="y", col="red")
legend('topright', c("f(x)=$F1","g(x)=$F2"), lty=1, col=c('blue', 'red'), bty='n', cex=.75)
dev.off()
"@ | Out-FileUtf8NoBom $file

    & 'C:\Program Files\Microsoft\MRO-3.3.2\bin\Rscript.exe' $file
}