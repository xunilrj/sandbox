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