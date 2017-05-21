rowmap <- function (M,f){ 
  result <- sapply(seq(nrow(M)), function(i) f(M[i,])) 
  names(result) <- rownames(M) 
  result 
} 