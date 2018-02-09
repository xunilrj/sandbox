# Car out of Oil, Likelihood and statistics using R

problem based on:  
https://www.khanacademy.org/math/statistics-probability/random-variables-stats-library/combine-random-variables/v/analyzing-distribution-of-sum-of-two-normally-distributed-random-variables

## Problem

Mr K. is a very methodical fellow. He monitor how much his car consume every single day on his daily commute. This data is available on the consumption.csv on this folder.

## Making the premises explicit
 
 One of the most important steps when modeling it to be clear and conscious about your premises. Even for such a simple problem as this one we have a lot of premises that can pass totally unnoticed if we do not explicit them.

 The first premise we are making is that we can achieve the desired precision by modeling the consumption using just one simple model. In our case will be a normal distribution model. In reality almost everything somehow affects the car consumption: the traffic, the weather, the car age, tire age, the driver skill, driver mood, gas price, Mid East politics etc... In our case we will simplify, without further tests, the model to a Normal Distribution (two variables, mean and standard deviation).

 Given that we do not know the real mean and the real standard deviation we will need to estimate these values; this is called "a posteriori" estimation. In the field of "a posteriori" estimation, when we maximize the likelihood of a database to a normal distribution we call "Maximum Likelihood Estimation", which is a special case os "Maximum a Posteriori Estimation" (MAP).

 ## Analyzing the "Home-Work" commute

 Our fist task, off course, is to make a very simple plot of the consumption on the "Home-Work" commute.

    #R
    > consumption <- read.csv("consumption.csv")
    > homeWork <- consumption[comsumption$type == 1,]    
    > png(filename="homeWork.plot.png")
    >   plot(homeWork$day, homeWork$consumption)
    > dev.off()

![Home-Work Consumption](/homeWork.plot.png)

Well, it is not hard to see that we do not have clear relationship between the day and the consumption, but maybe we can understand the data better looking to its histogram.

    #R
    > png(filename="homeWork.hist.png")
    >    hist(homeWork$consumption)
    > dev.off()

![Home-Work Consumption Histogram](/homeWork.hist.png)

hum... The resemblance with a normal distribution is almost perfect. Which is wonderful for us, because allows us to model the consumption using a simple normal distribution.

The graph below show the Normal Likelihood of the home-work data. We can see that the maximum is converging around mean = 10 and standard deviation (sd) = 2.

    #R
    #plotMLE is our custom function (code is below)
    > plotMLE(homeWork$consumption, x=c(5,15),y=c(0,5), levels = 2000)

![Home-Work Consumption Histogram](/homeWork.likelihood.png)

We can ask R to give us the exact value using a numerical optimization algorithm.

    #R
    > optim(c(12,4), normal.lik.muvar, y=homeWork$consumption, method="BFGS")
    #Result
    $par
    [1] 10.01983  1.70105
    $value
    [1] 1.950178
    $counts
    function gradient 
      23       19
    $convergence
    [1] 0
    $message
    NULL

So R is telling us that the optimal (as we defined, more on how we defined this below) parameters are: mean = 10, sd = 1.70. And just to confirm let us plot dashed lines to see if the results are consistent.

    > png(filename="homeWork.likelihood.withlines.png")
    >    plotMLE(homeWork$consumption, x=c(5,15),y=c(0,5), levels = 2000)
    >    abline(h=1.70105, lty=2)
    >    abline(v=10.01983, lty=2)
    > dev.off()

![Home-Work Consumption Histogram](/homeWork.likelihood.withlines.png)

OK. First step done. We now have a model to Mr. K. home work consumption.

    Normal Distribution
    μ (mean): 10
    σ (standard deviation): 1.70

We can even see how out model is compared to the real histogram. And one can easily see that we have a vary good model.

![Home-Work Consumption Histogram](/homeWork.hist.model.png)
