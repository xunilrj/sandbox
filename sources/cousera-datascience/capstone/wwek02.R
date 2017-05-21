source("download.data.R")
source("SplitArray.R")
source("createCorpus.R")
source("generateTDM.R")
source("generateDTM.R")
source("rowMap.R")

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
  foreach(i = 0:2, .packages = c("tm","SnowballC")) %dopar% {
    try({
      source("createCorpus.R")
      source("generateTDM.R")
      source("generateDTM.R")
      source("rowMap.R")
      
      createCorpus(tempFileFormat, i)
      
      generateDTM(tempFileFormat, i)
      sumDTMRows(tempFileFormat, i)
      
      generateTDM(tempFileFormat, i)
      sumTDMRows(tempFileFormat, i)
      
      generateBITDM(tempFileFormat, i)
      sumBITDMRows(tempFileFormat, i)
      
      generateTRITDM(tempFileFormat, i)
      sumTRITDMRows(tempFileFormat, i)
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

toaddDF <- function (l1){
  data.frame(word = names(l1), count = as.numeric(l1))  
}
aggregateWordCount <- function (l1,l2){
  agg <- rbind(l1,l2)
  aggregate(count ~ word, data = agg, FUN = sum)
}

n<-3
generateAgg <- function(aggFormat,twitterformat, newsformat, blogsformat){
  aggtdmFile <- sprintf(aggFormat, "tdm")
  aggtdmTweetsFile <- sprintf(aggFormat, "tdmtweet")
  aggtdmNewsFile <- sprintf(aggFormat, "tdmnews")
  aggtdmBlogsFile <- sprintf(aggFormat, "tdmblogs")
  
  if(!file.exists(aggtdmFile)){
    tdms <- sapply(sprintf(twitterformat,0:(n-1)), readRDS)
    tdms <- lapply(tdms, toaddDF)
    aggtdmtweet <- Reduce(aggregateWordCount, tdms)
    saveRDS(aggtdmtweet,aggtdmTweetsFile)
    
    tdms <- sapply(sprintf(newsformat,0:(n-1)), readRDS)
    tdms <- lapply(tdms, toaddDF)
    aggtdmnews <- Reduce(aggregateWordCount, tdms)
    saveRDS(aggtdmnews,aggtdmNewsFile)
    
    tdms <- sapply(sprintf(blogsformat,0:(n-1)), readRDS)
    tdms <- lapply(tdms, toaddDF)
    aggtdmblogs <- Reduce(aggregateWordCount, tdms)
    saveRDS(aggtdmblogs,aggtdmBlogsFile)
    
    aggtdm <- Reduce(aggregateWordCount, list(aggtdmtweet, aggtdmnews, aggtdmblogs))
    saveRDS(aggtdm, aggtdmFile)
  }
}

generateAgg("temp/agg%s.rds", 
  "temp/enustweets%03d..tdm.sum.rds",
  "temp/enusnews%03d..tdm.sum.rds",
  "temp/enusblogs%03d..tdm.sum.rds")

generateAgg("temp/agg%s.bi.rds", 
            "temp/enustweets%03d..bitdm.sum.rds",
            "temp/enusnews%03d..bitdm.sum.rds",
            "temp/enusblogs%03d..bitdm.sum.rds")

generateAgg("temp/agg%s.tri.rds", 
            "temp/enustweets%03d..tritdm.sum.rds",
            "temp/enusnews%03d..tritdm.sum.rds",
            "temp/enusblogs%03d..tritdm.sum.rds")


library(dplyr)
getTop10 <- function(df){
  df %>% arrange(desc(count)) %>% top_n(n = 10, wt = count)
}

aggtdm <- readRDS("temp/aggtdm.rds")
aggtdmtweets <- readRDS("temp/aggtdmtweet.rds")
aggtdmnews <- readRDS("temp/aggtdmnews.rds")
aggtdmblogs <- readRDS("temp/aggtdmblogs.rds")
aggbitdm <- readRDS("temp/aggtdm.bi.rds")
aggbitdmtweets <- readRDS("temp/aggtdmtweet.bi.rds")
aggbitdmnews <- readRDS("temp/aggtdmnews.bi.rds")
aggbitdmblogs <- readRDS("temp/aggtdmblogs.bi.rds")
aggtritdm <- readRDS("temp/aggtdm.tri.rds")
aggtritdmtweets <- readRDS("temp/aggtdmtweet.tri.rds")
aggtritdmnews <- readRDS("temp/aggtdmnews.tri.rds")
aggtritdmblogs <- readRDS("temp/aggtdmblogs.tri.rds")

top10words <- getTop10(aggtdm)
top10wordsnews <- getTop10(aggtdmnews)
top10wordsblogs <- getTop10(aggtdmblogs)
top10wordstweets <- getTop10(aggtdmtweets)
barplot(top10words$count, names.arg = top10words$word, main = "Top 10 Words")
barplot(top10wordsnews$count, names.arg = top10wordsnews$word, main = "Top 10 Words on News")
barplot(top10wordsblogs$count, names.arg = top10wordsblogs$word, main = "Top 10 Words on Blogs")
barplot(top10wordstweets$count, names.arg = top10wordstweets$word, main = "Top 10 Words on Twitter");

top10words <- getTop10(aggbitdm)
top10wordsnews <- getTop10(aggbitdmnews)
top10wordsblogs <- getTop10(aggbitdmblogs)
top10wordstweets <- getTop10(aggbitdmtweets)
barplot(top10words$count, names.arg = top10words$word, main = "Top 10 Words")
barplot(top10wordsnews$count, names.arg = top10wordsnews$word, main = "Top 10 Words on News")
barplot(top10wordsblogs$count, names.arg = top10wordsblogs$word, main = "Top 10 Words on Blogs")
barplot(top10wordstweets$count, names.arg = top10wordstweets$word, main = "Top 10 Words on Twitter");

top10words <- getTop10(aggtritdm)
top10wordsnews <- getTop10(aggtritdmnews)
top10wordsblogs <- getTop10(aggtritdmblogs)
top10wordstweets <- getTop10(aggtritdmtweets)
barplot(top10words$count, names.arg = top10words$word, main = "Top 10 Words")
barplot(top10wordsnews$count, names.arg = top10wordsnews$word, main = "Top 10 Words on News")
barplot(top10wordsblogs$count, names.arg = top10wordsblogs$word, main = "Top 10 Words on Blogs")
barplot(top10wordstweets$count, names.arg = top10wordstweets$word, main = "Top 10 Words on Twitter");

