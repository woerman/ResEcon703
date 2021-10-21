########################################################
##### ResEcon 703: Topics in Advanced Econometrics #####
#####      Create examples for week 8 slides       #####
#####         Matt Woerman, UMass Amherst          #####
########################################################

### Load packages --------------------------------------------------------------

library(tidyverse)


### Method of Moments Examples -------------------------------------------------

### Construct MM example for the mean using a Poisson distribution
## Set seed for replication
set.seed(703)
## Draw from a Poisson with gamma = 10
y <- rpois(5, 10)
y
## Estimate mu as the mean
mean(y)