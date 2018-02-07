days = seq(from = 1, to = 260)
type = c(1,2)
homeWork = rnorm(260, mean=10, sd=1.5)
workHome = rnorm(260, mean=10, sd=2.0)

consuption = data.frame(days, type, consuption = c(homeWork, workHome))
write.csv(file="consuption.csv", x=consuption)

homeWork = consuption[consuption$type == 1,]
workHome = consuption[consuption$type == 2,]

png(filename="homeWork.plot.png")
plot(homeWork$day, homeWork$consuption)
dev.off()

png(filename="homeWork.hist.png")
hist(homeWork$consuption)
dev.off()

png(filename="workHome.plot.png")
plot(workHome$day, workHome$consuption)
dev.off()

png(filename="workHome.hist.png")
hist(workHome$consuption)
dev.off()