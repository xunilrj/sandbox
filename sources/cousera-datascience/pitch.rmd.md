Clustering
========================================================
author: Daniel Frederico Lins Leite
date: 2017-04-22
autosize: true

Simulation
========================================================

In this presentation we will generate 2000 2D points using two Multivariate Gaussians. Then we will use three different clustering algorithms to try to recognize the original distributions. We will try to find 2, 4 and 6 clusters for each algorithms.


```r
library(plotly)
library('MASS')
simulation.g1 <- mvrnorm(n = 1000, c(0,0), diag(2))
simulation.g2 <- mvrnorm(n = 1000, c(-3,-3), diag(c(0.3,10)))
simulation <- rbind(simulation.g1, simulation.g2)
simulation <- data.frame(x = simulation[,1], y = simulation[,2])
```

k Means Clustering
========================================================

![plot of chunk unnamed-chunk-2](pitch.rmd-figure/unnamed-chunk-2-1.png)

Ward Hierarchical Clustering (using Euclidean distance)
===============

![plot of chunk unnamed-chunk-3](pitch.rmd-figure/unnamed-chunk-3-1.png)


Gaussian Mixture Model (using BIC)
===============

![plot of chunk unnamed-chunk-4](pitch.rmd-figure/unnamed-chunk-4-1.png)

