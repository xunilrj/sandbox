splitIn <- function(A, n, format){
  perfile <- length(A) / n
  for (i in 0:(n-1) ) {
    name <- sprintf(format, i)
    from<- (i*perfile)
    to<-((i+1)*perfile)
    if (!file.exists(name)){
      file.create(name)
    }
    writeLines(A[from:to], name)
  }
}