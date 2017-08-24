fibo <- function(n) round(((5 + sqrt(5)) / 10) * (( 1 + sqrt(5)) / 2) ** (n - 1))

for (i in seq(30*10)){
  xlimit = 100*i
  ylimit = c(0,10000)
  png(paste("./complexity.functions.",sprintf("%05d", i),".png", sep=""))
  plot(sapply(1:xlimit,function(x){x}), type='l', xlab="x", ylab="y", col="blue", ylim = ylimit)
  grid (10,10, lty = 6, col = "cornsilk2")
  lines(sapply(1:xlimit,function(x){x**2}), type='l', xlab="x", ylab="y", col="red")
  lines(sapply(1:xlimit,function(x){x*log(x)}), type='l', xlab="x", ylab="y", col="green")
  lines(sapply(1:xlimit,function(x){factorial(x)}), type='l', xlab="x", ylab="y", col="yellow")
  lines(sapply(1:xlimit,function(x){log(x)}), type='l', xlab="x", ylab="y", col="grey")
  lines(sapply(1:xlimit,function(x){fibo(x)}), type='l', xlab="x", ylab="y", col="pink")
  legend('topright', c("x","x**2","x*log(x)","x!","log(x)","fibo(x)"), lty=1, col=c('blue', 'red','green',"yellow","grey","pink"), bty='n', cex=.75)
  dev.off()
}
