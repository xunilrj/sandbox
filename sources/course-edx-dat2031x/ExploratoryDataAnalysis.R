getPackages <- function(x) {
  x <- as.character(match.call()[[2]])
  writeLines(sprintf('Testing %s is installed...', x))
  if (!require(x, character.only = TRUE)) {
    writeLines(sprintf('\tInstalling...', x))
    install.packages(pkgs = x, repos = "http://cran.r-project.org")
    require(x, character.only = TRUE)
    writeLines(sprintf('\tInstalled!', x))
  }
}

getPackages('ggplot2')
getPackages('gridExtra')
getPackages('pryr')

auto.price <- read.csv('C:/Users/xunil/Downloads/DAT203.1x/Mod4/Automobile price data _Raw_.csv', header = TRUE, stringsAsFactors = FALSE)
cols <- c('price', 'bore', 'stroke','horsepower', 'peak.rpm')
auto.price[, cols] <- lapply(auto.price[, cols], as.numeric)
auto.price <- auto.price[complete.cases(auto.price), ]
auto.price$lnprice <- log(auto.price$price)
auto.price$num.cylinders <-
  ifelse(auto.price$num.of.cylinders %in% c("four", "three"), "threefour",
         ifelse(auto.price$num.of.cylinders %in% c("five", "six"),
                "five-six", "eight-twelve"))

num.cols <- c("wheel.base","width","height","curb.weight","engine.size","bore","compression.ratio","city.mpg","price","lnprice")

pairs(~ ., data = auto.price[, num.cols])

plot.cols2 <- c("length","curb.weight","engine.size","city.mpg","price")

auto.hist <- function(data, x) {
  rg = range(data[,x])
  bw = (rg[2] - rg[1])/30
  
  title <- paste("Histogram of", x, "conditioned on type of drive wheels")
  
  ggplot(data, aes_string(x)) +
    geom_histogram(aes(y = ..count..), binwidth = bw) +
    facet_grid(. ~ drive.wheels) +
    ggtitle(title)
}

auto.box <- function(data, x) {
  title <- paste("Box plot of", x, "by type of drive wheels")
  ggplot(data, aes_string('drive.wheels', x)) +
    geom_boxplot() +
    ggtitle(title)
}

lapply(plot.cols2, pryr::partial(auto.hist, data = auto.price))
lapply(plot.cols2, pryr::partial(auto.box, data = auto.price))

plot.cols3 <- c("length","curb.weight","engine.size","city.mpg")

scatter.auto <- function(data, x){
  require(ggplot2)
  title <- paste("price vs.", x, 'with color by num.cylinders')
  ggplot(data, aes_string(x, 'price')) +
    geom_point(aes(color = factor(num.cylinders))) +
    ggtitle(title)
}

lapply(plot.cols3, pryr::partial(scatter.auto, data = auto.price))

scatter.auto.cond <- function(data, x){
  title <- paste("price vs.", x, 'with color by num.cylinders and body style')
  ggplot(data, aes_string(x, 'price')) +
    geom_point(aes(color = factor(fuel.type))) +
    facet_grid(body.style ~ num.cylinders) +
    ggtitle(title)
}

lapply(plot.cols3, pryr::partial(scatter.auto.cond, data = auto.price))

Income <- read.csv('C:/Users/xunil/Downloads/DAT203.1x/Mod4/Adult Census Income Binary Classification dataset.csv')

name.list <- function(x) {
  names <- names(x)
  len <- length(names)
  names[-len]
}

bar.income <- function(data, x){
  if(!is.numeric(data[,x])) {
    capture.output(
      plot( ggplot(data, aes_string(x)) +
              geom_bar() +
              facet_grid(. ~ income) +
              ggtitle(paste("Counts of income level by",x))))
  }
}

feature.names <- name.list(Income)
lapply(feature.names, pryr::partial(bar.income, data = Income))

box.income <- function(data, x){
  if(is.numeric(data[,x])) {
    capture.output(
      plot( ggplot(data, aes_string('income', x)) +
              geom_boxplot() +
              ggtitle(paste("Counts of income level by",x))))
  }}

lapply(feature.names, pryr::partial(box.income, data = Income))

#Engine Size vs Drive Wheels
#ggplot(data, aes_string(x)) +
#  geom_histogram(aes(y = ..count..), binwidth = bw) +
#  facet_grid(. ~ drive.wheels) +
#  ggtitle(title)

rg = range(auto.price[,c("engine.size")])
bw = (rg[2] - rg[1])/30
ggplot(auto.price, aes(x = engine.size)) +
  geom_histogram(aes(y = ..count..), binwidth = bw) + 
  facet_grid(. ~ drive.wheels)

#Based on the conditioned box plots you created for the
#adult income classification dataset, the median age of
#adults who earn $50K or less is...

ggplot(Income, aes(x = age, y = income)) +
  geom_boxplot()

ggplot(Income, aes(x = income, y = age)) +
  geom_boxplot()
