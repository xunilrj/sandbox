getPackage <- function(x) {
  x <- as.character(match.call()[[2]])
  writeLines(sprintf('Testing %s is installed...', x))
  if (!require(x, character.only = TRUE)) {
    writeLines(sprintf('\tInstalling...', x))
    install.packages(pkgs = x, repos = "http://cran.r-project.org")
    require(x, character.only = TRUE)
    writeLines(sprintf('\tInstalled!', x))
  }
}

getPackage('ggplot2')
getPackage('scales')


mypath <- dirname(sys.frame(1)$ofile)
writeLines(sprintf("Running at %s", mypath))
setwd(mypath)

dataFile <- normalizePath(file.path(mypath, "payments.csv"))

writeLines(sprintf('Testing %s exists...', dataFile))
if (!file.exists(dataFile)) {
  writeLines('\tIt does not... Downloading file...')
  download.file("https://d3c33hcgiwev3.cloudfront.net/_e143dff6e844c7af8da2a4e71d7c054d_payments.csv?Expires=1479081600&Signature=lSBVPDerZqlNbFB0HQr28rY2ZVlLzZpIUpRTXTPH4LXhZtDkSmKazumKPixZu6vDVtj4DLMwXafL3sjP7tVsuIChN-RYhGar-2g5~ki3EKOrsVxaBV~HIQV~wOo-SiAaFCyS7Cd6mVmo8UvSvh4K7QSST5qcwBGwEg7U~XZx7VA_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A", destfile = dataFile)
  writeLines('\tFinished!')
} else {
  writeLines('\tFile already exists!')
}

parseFile <- function() {
  writeLines('Parsing data...')
  data <- read.csv(dataFile)
  writeLines('Finished')
  data
}

df <- parseFile()

inNewYork <- df[df$Provider.State == "NY",]

ggplot(df, aes(x = Average.Covered.Charges, y = Average.Total.Payments)) +
  stat_density2d(aes(alpha=..level.., fill=..level..), size=2, 
                 bins=10, geom="polygon") + 
  scale_fill_gradient(low = "yellow", high = "red") +
  scale_alpha(range = c(0.00, 0.5), guide = FALSE) +
  geom_density2d(colour="black", bins=10) +
  guides(alpha=FALSE) + xlim(c(2500, 52500)) + ylim(c(2600, 12000))

plot <- ggplot(inNewYork, aes(x = Average.Covered.Charges,
                      y = Average.Total.Payments)) +
  geom_point() +
  ggtitle("IPPS Summary in New York (2011)") +
  ylab("Average Total Payments") +
  xlab("Average Covered Changes") + 
  scale_x_continuous(labels=dollar_format()) +
  scale_y_continuous(labels=dollar_format())

ggsave('reproducible.research.week01.01.pdf', plot = plot, width = 10, height = 10)

df[,"DRG"] <- sapply(as.character(df$DRG.Definition),
                                      function(x){
                                        strsplit(x," - ")[[1]][1]
                                      }, USE.NAMES = FALSE)

df[,"Provider.Zip.Code"] <- as.factor(df[,"Provider.Zip.Code"])

plot <- ggplot(df, aes(x = Average.Covered.Charges,
                              y = Average.Total.Payments)) +
  geom_point(aes(color = DRG)) +
  ggtitle("IPPS Summary (2011)") +
  ylab("Average Total Payments") +
  xlab("Average Covered Changes") + 
  scale_x_continuous(labels=dollar_format()) +
  scale_y_continuous(labels=dollar_format()) + 
  facet_grid(Provider.State ~ DRG) +
  theme(legend.position = "top",
        legend.text=element_text(size=5),
        axis.text.x = element_text(angle = 90, hjust = 1))

ggsave('reproducible.research.week01.02.pdf', plot = plot, width = 10, height = 10)
