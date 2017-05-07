url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
destfile <- "coursera-switftkey.zip"

setwd("C:/github/xunilrj-sandbox/sources/cousera-datascience/capstone")

print(destfile)
if (!file.exists(destfile)) {
  print("\tdownloading file...")
  download.file(url, destfile = destfile)
  print("\tdone.")
}else{
  print("\tfile already exists.")
}

if(!dir.exists("final")){
  unzip(destfile, exdir = ".")
}

maxagg <- function(current, newvalue){
  if(newvalue > current){
    newvalue
  }else{
    current
  }
}

countagg <- function(current, newvalue){
  if(current==0){
    current = c(0,0,0)
  }
  current + newvalue
}

mapLines <- function(con, f, agg){
  current <- 0
  while (TRUE) {
    line = readLines(con, n = 1)
    lengthline <- length(line)
    if(lengthline == 0){break}
    newvalue <- f(line[[1]])
    current <- agg(current, newvalue)
  }
  current
}

mapFiles <- function(l,f,agg){
  lapply(l, function(i){
    con <- file(i, "r")
    results <- mapLines(con, f, agg)
    close(con)
    results
  })
}

library(stringi)

setwd("C:/github/xunilrj-sandbox/sources/cousera-datascience/capstone/final/en_US")

files <- c("en_US.blogs.txt","en_US.news.txt","en_US.twitter.txt")
#longest line
#biggers <- mapFiles(files, function(l){nchar(l)}, maxagg)
#love-hate ratio
#lovehateratio <- mapFiles(files, function(l){
#  if(stri_detect_fixed(l,"love")){
#    result <- c(1,0,1)
#  }else{
#    if(stri_detect_fixed(l, "hate")){
#      result <- c(0,1,1)
#    }else{
#      result <- c(0,0,1)
#    }
#  }
#  result
#}, countagg)

con <- file("en_US.twitter.txt", "r")
files <- readLines(con)
grep("biostats",files)

#howmany computers
howmany <- mapFiles(c("en_US.twitter.txt"), function(l){
  if(stri_detect_fixed(l,"A computer once beat me at chess, but it was no match for me at kickboxing")){
    result <- 1
  }else{
      result <- 0
  }
  result
}, function(current, newvalue){
  current + newvalue
})


