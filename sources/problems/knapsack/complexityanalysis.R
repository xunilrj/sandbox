
drawLinearModel <- function(dataFrame, formula){
  
  arguments <- as.list(match.call())
  evaluatedFormula = eval(arguments$formula, dataFrame)
  
  vars <- all.vars(formula)
  
  evaluatedVar1 <- dataFrame[,c(vars[1])]
  evaluatedVar2 <- dataFrame[,c(vars[2])]
  
  lmResult <- lm(evaluatedFormula, data = dataFrame)
  abline(lmResult)
  
  #rsquared1 <- summary(LRResult)$r.squared
  rsquared2 <- cor(evaluatedVar1, evaluatedVar2)^2
  
  usr <- par( "usr" )
  rsquaredlabel = bquote(italic(R)^2 == .(format(rsquared2, digits = 3)))
  text(usr[ 1 ], usr[ 4 ], labels = rsquaredlabel, adj = c( 0, 1 ))
}

mypath <- dirname(sys.frame(1)$ofile)
writeLines(sprintf("Running at %s", mypath))
setwd(mypath)

times <- read.csv('csharptimes.csv')

#Capacity is constant and size grows

png('01-capacity-constant-size-growing.png')

size <- 1024
timesConstantCapacity <- times[which(times$C == size),]
timesConstantCapacity$Time <- timesConstantCapacity$Time / 1000
with(timesConstantCapacity, plot(Size, Time, main = cat("When Capacity = ", toString(size))))
drawLinearModel(timesConstantCapacity, Time ~ Size)

dev.off()

#Size constant and capacity grows

png('02-capacity-equals-size-growing.png')

size <- 47434
timesConstantSize <- times[which(times$Size == size),]
with(timesCES, plot(C, Time, main = cat("When Size =", toString(size))))
drawLinearModel(timesCES, Time ~ C)

dev.off()

timesCES <- times[which(times$C == times$Size),]
with(timesCES, plot(Size, Time, main = "When Capacity = Size"))
drawLinearModel(timesCES, log(Time+1) ~ Size)

#analyzing using logscale
#throw because Time has zeros 

#with(timesCES, plot(log(Size), log(Time), main = "When Capacity = Size"))
#drawLinearModel(timesCES, log(Time) ~ log(Size)) 

# recovering the missing points

png('03-capacity-constant-size-growing-timeadjusted.png')

timesCESAdjusted <- timesCES
timesCESAdjusted$Time <- timesCESAdjusted$Time + 1
with(timesCESAdjusted, plot(log(Size), log(Time), main = "When Capacity = Size"))
drawLinearModel(timesCESAdjusted, log(Time) ~ log(Size))

dev.off()

#ignoring first measurements because of the limitation
#of the stopwatch

png('04-capacity-constant-size-growing-timeadjustedskipped.png')

timesCESAdjustedSkipped <- timesCESAdjusted
timesCESAdjustedSkipped <- timesCESAdjustedSkipped[-(0:5),]
with(timesCESAdjustedSkipped, plot(log(Size), log(Time), main = "When Capacity = Size"))
drawLinearModel(timesCESAdjustedSkipped, log(Time) ~ log(Size))

dev.off()