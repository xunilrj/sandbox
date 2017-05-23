generateTDM <- function(format, i){
  library(tm)
  filecorpusRDS <- sprintf(format, i, "rds")
  fileRDS <- sprintf(format, i, "tdm.rds")
  if (!file.exists(fileRDS)){
    corpus <- readRDS(filecorpusRDS)
    TDM <- TermDocumentMatrix(corpus)
    saveRDS(TDM, fileRDS)
  }
}

generateBITDM <- function(format, i){
  library(tm)
  filecorpusRDS <- sprintf(format, i, "rds")
  fileRDS <- sprintf(format, i, "bitdm.rds")
  
  corpus <- readRDS(filecorpusRDS)
  
  if(!file.exists(fileRDS)){
    library(RWeka)
    BiGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
    TDM <- TermDocumentMatrix(corpus, control = list(
      removeSparseTerms=0.8,
      tokenize = BiGramTokenizer
    ))
    saveRDS(TDM, fileRDS)
  }
}

generateTRITDM <- function(format, i){
  library(tm)
  filecorpusRDS <- sprintf(format, i, "rds")
  fileRDS <- sprintf(format, i, "tritdm.rds")
  
  corpus <- readRDS(filecorpusRDS)
  
  if(!file.exists(fileRDS)){
    library(RWeka)
    TriGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
    TDM <- TermDocumentMatrix(corpus, control = list(
      removeSparseTerms=0.8,
      tokenize = TriGramTokenizer
    ))
    saveRDS(TDM, fileRDS)
  }
}


sumTDMRows <- function(format, i){
  fileTDMRDS <- sprintf(format, i, "tdm.rds")
  fileTDMsumRDS <- sprintf(format, i, ".tdm.sum.rds")
  
  if(!file.exists(fileTDMsumRDS)){
    TDM <- readRDS(fileTDMRDS)
    terms <- rowmap(TDM, sum)
    saveRDS(terms, fileTDMsumRDS)
  }
}

sumBITDMRows <- function(format, i){
  fileTDMRDS <- sprintf(format, i, "bitdm.rds")
  fileTDMsumRDS <- sprintf(format, i, ".bitdm.sum.rds")
  
  if(!file.exists(fileTDMsumRDS)){
    TDM <- readRDS(fileTDMRDS)
    terms <- rowmap(TDM, sum)
    saveRDS(terms, fileTDMsumRDS)
  }
}

sumTRITDMRows <- function(format, i){
  fileTDMRDS <- sprintf(format, i, "tritdm.rds")
  fileTDMsumRDS <- sprintf(format, i, ".tritdm.sum.rds")
  
  if(!file.exists(fileTDMsumRDS)){
    TDM <- readRDS(fileTDMRDS)
    terms <- rowmap(TDM, sum)
    saveRDS(terms, fileTDMsumRDS)
  }
}