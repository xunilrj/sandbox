library(quanteda)
library(data.table)

getNgramFreqs <- function(ng, dat, ignores=NULL, sort.by.ngram=TRUE, sort.by.freq=FALSE) {
  # http://stackoverflow.com/questions/36629329/
  # how-do-i-keep-intra-word-periods-in-unigrams-r-quanteda
  if(is.null(ignores)) {
    dat.dfm <- dfm(dat, ngrams=ng, toLower = FALSE, removePunct = FALSE,
                   what = "fasterword", verbose = FALSE)
  } else {
    dat.dfm <- dfm(dat, ngrams=ng, toLower = FALSE, ignoredFeatures=ignores,
                   removePunct = FALSE, what = "fasterword", verbose = FALSE)
  }
  rm(dat)
  # quanteda docfreq will get the document frequency of terms in the dfm
  ngram.freq <- docfreq(dat.dfm)
  if(sort.by.freq) { ngram.freq <- sort(ngram.freq, decreasing=TRUE) }
  if(sort.by.ngram) { ngram.freq <- ngram.freq[sort(names(ngram.freq))] }
  rm(dat.dfm)
  
  return(ngram.freq)
}

getNgramTables <- function(ng, linesCorpus, prefixFilter=NULL) {
  start_msg <- paste0("START getNgramTables building ", ng,
                      "-gram frequency table at ",
                      as.character(Sys.time()), "\n")
  cat(start_msg)
  ngrams <- getNgramFreqs(ng, linesCorpus)
  ngrams_dt <- data.table(ngram=names(ngrams), freq=ngrams)
  if(length(grep('^SOS', ngrams_dt$ngram)) > 0) {
    ngrams_dt <- ngrams_dt[-grep('^SOS', ngrams_dt$ngram),]
  }
  if(!is.null(prefixFilter)) {
    regex <- sprintf('%s%s', '^', prefixFilter)
    ngrams_dt <- ngrams_dt[grep(regex, ngrams_dt$ngram),]
  }
  
  end_msg <- paste0("*** FINISH building ", ng, "-gram frequency table at ",
                    as.character(Sys.time()), "***\n")
  cat(end_msg)
  
  return(ngrams_dt)
}

ltcorpus <- readLines("./capstone/final/finals/en_US.final.txt")

unigs <- getNgramTables(1, ltcorpus)
bigrs <- getNgramTables(2, ltcorpus)
trigs <- getNgramTables(3, ltcorpus)
tetrags <- getNgramTables(4, ltcorpus)
pentags <- getNgramTables(5, ltcorpus)

save(unigs, bigrs, trigs, tetrags, pentags, file = "ngrams.rdata")

countFreq <- function(tokens){
  size <- length(tokens)
  
  if(size == 1) tablefreq <- unigs
  else if(size == 2) tablefreq <- bigrs
  else if(size == 3) tablefreq <- trigs
  else if(size == 4) tablefreq <- tetrags
  else if(size == 5) tablefreq <- pentags
  
  phrase <- paste(tokens, collapse = "_")
  r <- tablefreq[ngram == phrase]
  
  if(length(r) == 0) 0
  else as.numeric(r$freq)
}

Pmle <- function(words, size=2){
  #sum(sapply(1:(length(words)-size+1),function(x) {
  #  currentGram <- words[x:(x+size-1)]
  #  print(sprintf("%s/%s",paste(currentGram, collapse = "_"),paste(head(currentGram, n = size-1), collapse = "_")))
    #log(countFreq(currentGram)/countFreq(head(currentGram, n = size-1)))
  #}))
  #countFreq(words)/countFreq(head(words, n = length(words) - 1))
  currentGram <- words[(length(words):-size):]
  Pmle(head(words, n = length(words) - 1), size)
}

#When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd
Pmle(c("i","would","sleep"))
Pmle(c("i","would","eat"))
Pmle(c("i","would","give"))
Pmle(c("i","would","die"))

Pmle(c("and","id","sleep"))
Pmle(c("and","id","eat"))
Pmle(c("and","id","give"))
Pmle(c("and","id","die"))

Pmle(c("and","i", "would","sleep"),3)
Pmle(c("and","i", "would","eat"),3)
Pmle(c("and","i", "would","give"))
Pmle(c("and","i", "would","die"))

Pmle(c("live", "and","id", "sleep"))
Pmle(c("live", "and","id", "eat"))
Pmle(c("live", "and","id", "give"))
Pmle(c("live", "and","id", "die"))

Pmle(c("live", "and", "i", "would", "sleep"))
Pmle(c("live", "and", "i", "would", "eat"))
Pmle(c("live", "and", "i", "would", "give"))
Pmle(c("live", "and", "i", "would", "die"))
Pmle(c("live", "and", "i", "would", "die"),3)
Pmle(c("live", "and", "i", "would", "die"),4)

Pmle(c("live", "and", "id", "sleep"))
Pmle(c("live", "and", "id", "eat"))
Pmle(c("live", "and", "id", "give"))
Pmle(c("live", "and", "id", "die"))

Pmle(c("id", "live", "and", "id"))
Pmle(c("id", "live", "and", "id", "die"))
