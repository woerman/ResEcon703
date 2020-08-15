### ResEcon 703: Topics in Advanced Econometrics
### Lecture 7: Numerical Optimization
### Matt Woerman, UMass Amherst

### mlogit() Example in R ------------------------------------------------------

### Load and look at dataset
## Load tidyverse and mlogit
library(tidyverse)
library(mlogit)
## Load dataset
data_multi <- read_csv('travel_multinomial.csv')
## Look at dataset
data_multi

### Estimate multinomial logit model using mlogit
## Convert dataset to mlogit format
data_mlogit <- data_multi %>% 
  mlogit.data(shape = 'wide', choice = 'mode', varying = 2:9, sep = '_')
## Model choice as multinomial logit with common cost/income coefficient, 
## alternative intercepts, and alternative-specific time coefficients
model_mlogit <- data_mlogit %>% 
  mlogit(mode ~ I(cost / income) | 1 | time, data = .)
## Summarize model results
model_mlogit %>% 
  summary()
