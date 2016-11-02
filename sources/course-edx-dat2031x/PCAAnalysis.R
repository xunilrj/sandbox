library(RWeka)
library(FactoMineR)
library(data.table)

setwd('C:/Users/xunil/Downloads')

link   <- "http://www.cs.iastate.edu/~cs573x/labs/lab1/breast-cancer-wisconsin.arff"
fileName <- "breast-cancer-wisconsin.arff"
download.file(link, fileName)

breast <- read.arff(fileName)

cols   <- character(nrow(breast))
cols[] <- "black"
cols[breast$class == 2] <- "red"
cols[breast$class == 4] <- "blue"
pairs(breast, col=cols)

nums <- sapply(breast, is.numeric)
pcaBreasts <- breast[,nums]
res.pca <- PCA(pcaBreasts,graph=FALSE)
plot(res.pca,choix="var",invisible="quanti.sup")

row1 <- which.max(res.pca$var$cor[,1])
row2 <- which.max(res.pca$var$cor[,2])
c(rownames(res.pca$var$cor)[row1],res.pca$var$cor[row1,1:1])
c(rownames(res.pca$var$cor)[row2],res.pca$var$cor[row2,2:2])
