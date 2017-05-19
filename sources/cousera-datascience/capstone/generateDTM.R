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