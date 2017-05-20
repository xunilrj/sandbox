source("download.data.R")
source("SplitArray.R")
source("createCorpus.R")
source("generateTDM.R")
source("generateDTM.R")

library(sampling)
library(tm)
library(SnowballC)

library("parallel")
library("foreach")
library("doParallel")

n <- 100
ncores <- detectCores() - 1

process <- function(completeFileName, tempFileFormat){ 
  lines = readLines(completeFileName, skipNul = TRUE)
  splitIn(lines, n, gsub("%s", "txt", tempFileFormat))
  
  cl <- makeCluster(ncores)
  registerDoParallel(cl, cores = ncores)
  foreach(i = 0:(n-1), .packages = c("tm","SnowballC")) %dopar% {
    try({
      source("createCorpus.R")
      source("generateTDM.R")
      source("generateDTM.R")
      createCorpus(tempFileFormat, i)
      generateDTM(tempFileFormat, i)
      generateTDM(tempFileFormat, i)
    })
  }
  stopCluster(cl)
}

print("en_US.twitter.txt")
process("final/en_US/en_US.twitter.txt", "temp/enustweets%03d.%s")
print("en_US.news.txt")
process("final/en_US/en_US.news.txt", "temp/enusnews%03d.%s")
print("en_US.blogs.txt")
process("final/en_US/en_US.blogs.txt", "temp/enusblogs%03d.%s")