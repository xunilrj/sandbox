source("download.data.R")
source("SplitArray.R")
source("generateTDM.R")

library(sampling)
library(tm)
library(SnowballC)

library("parallel")
library("foreach")
library("doParallel")
#file.remove("sample.txt")
#file.remove("corpus.enustweets.rds")
#file.remove("tdm.rds")
n<- 100
if (!file.exists("temp")){dir.create("temp")}
lines.enustweets = readLines("final/en_US/en_US.twitter.txt", encoding = "UTF-8")
splitIn(lines.enustweets, n, "temp/enustweets%03d.txt")

if(!file.exists("sample.txt")){
  print("\tsampling texts...")
  
  samples = srswor(length(lines.enustweets)/100,length(lines.enustweets))
  lines.enustweets = lines.enustweets[samples==1]
  writeLines(lines.enustweets, "sample.txt")
  lines.enustweets <- NULL
  gc()
}else{
  print("\tusing generated sample.")
}

cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl, cores = detectCores() - 1)
foreach(i = 1:(n-1), .packages = c("tm","SnowballC")) %dopar% {
  try({
    crateCorpus("temp/enustweets%03d.%s", i)
  })
}
stopCluster(cl)

if(!file.exists("corpus.enustweets.rds")){
  print("\tgenerating corpus...")
  lines.enustweets = readLines("sample.txt", encoding = "UTF-8")
  vectorsource.enustweets = VectorSource(lines.enustweets)
  corpus.enustweets = Corpus(vectorsource.enustweets)
  corpus.enustweets <- tm_map(corpus.enustweets, content_transformer(function(x) iconv(x, "latin1", "ASCII", sub="")))
  corpus.enustweets <- tm_map(corpus.enustweets, content_transformer(tolower))
  corpus.enustweets <- tm_map(corpus.enustweets, removePunctuation)
  corpus.enustweets <- tm_map(corpus.enustweets, removeNumbers)
  corpus.enustweets <- tm_map(corpus.enustweets, removeWords, stopwords("english"))
  corpus.enustweets <- tm_map(corpus.enustweets, stemDocument)
  corpus.enustweets <- tm_map(corpus.enustweets, stripWhitespace)
  saveRDS(corpus.enustweets, "corpus.enustweets.rds")
  
  lines.enustweets <- NULL
  vectorsource.enustweets <- NULL
  corpus.enustweets <- NULL
  gc()
}else{
  print("\tusing generated corpus.")
}

cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl, cores = detectCores() - 1)
foreach(i = 1:(n-1)) %dopar% {
  try({
    generateTDM("temp/enustweets%03d.%s", i)
  })
}
stopCluster(cl)

if(!file.exists("tdm.rds")){
  print("\tgenerating TDM...")
  corpus.enustweets <- readRDS("corpus.enustweets.rds")
  TDM <- TermDocumentMatrix(corpus.enustweets)
  saveRDS(TDM, "tdm.rds")
}else{
  print("\tusing generated TDM.")
}

cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl, cores = detectCores() - 1)
foreach(i = 1:(n-1)) %dopar% {
  try({
    generateDTM("temp/enustweets%03d.%s", i)
  })
}
stopCluster(cl)

if(!file.exists("dtm.rds")){
  print("\tgenerating DTM...")
  corpus.enustweets <- readRDS("corpus.enustweets.rds")
  DTM <- DocumentTermMatrix(corpus.enustweets)
  saveRDS(DTM, "dtm.rds")
}else{
  print("\tusing generated DTM.")
}

TDM <- readRDS("tdm.rds")
DTM <- readRDS("dtm.rds")

rowmap <- function (M,f){
  result <- sapply(seq(nrow(M)), function(i) f(M[i,]))
  names(result) <- rownames(M)
  result
}

terms <- rowmap(TDM, sum)
termstop10 <- terms[order(terms, decreasing=TRUE)][1:10]
termsCount <- sum(terms)

barplot(termstop10)

#
lines = readLines("final/en_US/en_US.news.txt", skipNul = TRUE)
splitIn(lines, 100, "temp/enusnews%03d.txt")

cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl, cores = detectCores() - 1)
foreach(i = 1:(n-1)) %dopar% {
  try({
    generateDTM("temp/enustweets%03d.%s", i)
  })
}
stopCluster(cl)
