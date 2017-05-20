createCorpus <- function(format, i){
  library(tm)
  file <- sprintf(format, i, "txt")
  fileRDS <- sprintf(format, i, "rds")
  if (!file.exists(fileRDS)){
    lines.file = readLines(file, encoding = "UTF-8")
    vectorsource.file = VectorSource(lines.file)
    corpus.file = Corpus(vectorsource.file)
    corpus.file <- tm_map(corpus.file, content_transformer(function(x) iconv(x, "latin1", "ASCII", sub="")))
    corpus.file <- tm_map(corpus.file, content_transformer(tolower))
    corpus.file <- tm_map(corpus.file, removePunctuation)
    corpus.file <- tm_map(corpus.file, removeNumbers)
    corpus.file <- tm_map(corpus.file, removeWords, stopwords("english"))
    corpus.file <- tm_map(corpus.file, stemDocument)
    corpus.file <- tm_map(corpus.file, stripWhitespace)
    saveRDS(corpus.file, fileRDS)
  }
}