generateDTM <- function(format, i){
  library(tm)
  filecorpusRDS <- sprintf(format, i, "rds")
  fileRDS <- sprintf(format, i, "dtm.rds")
  if (!file.exists(fileRDS)){
    corpus <- readRDS(filecorpusRDS)
    DTM <- DocumentTermMatrix(corpus)
    saveRDS(DTM, fileRDS)
  }
}

sumDTMRows <- function(format, i){
  fileDTMRDS <- sprintf(format, i, "dtm.rds")
  fileDTMsumRDS <- sprintf(format, i, ".dtm.sum.rds")
  
  DTM <- readRDS(fileDTMRDS)
  terms <- rowmap(DTM, sum)
  saveRDS(terms, fileDTMsumRDS)
}