### ResEcon 703: Topics in Advanced Econometrics
### Lecture 1: Introduction
### Matt Woerman, UMass Amherst

### Fundamentals of R
## Everything is an object
foo
## Every object has a name and value
foo <- 2
## You use functions on objects
mean(foo)
## Functions come in packages/libraries
library(mlogit)

### Playing Around in R
## Basic math operations
1 + 2
1 + 2 == 3
(1 + 2) / 3
2^3
## Basic math with objects
a <- 1
b <- 2
a + b
c <- a + b
c
(a + b) / c
b^c
## Functions
exp(2)
sqrt(3)
d <- c(1, 2, 3, a, b, c) 
d
mean(d)
max(d)
min(d)
## Matrix math
mat_a <- matrix(c(1, 2, 3, 4), nrow = 2)
mat_a
2 * mat_a
mat_a %*% mat_a
## Install and load packages
install.packages(c('tidyverse', 'mlogit', 'gmm'))
library(tidyverse)
library(mlogit)
library(gmm)
