########################################################
##### ResEcon 703: Topics in Advanced Econometrics #####
#####    Week 8: Generalized Method of Moments     #####
#####         Matt Woerman, UMass Amherst          #####
########################################################

### Generalized Method of Moments Example in R ---------------------------------

### Load and look at dataset
## Load tidyverse
library(tidyverse)
## Load dataset
ac_data <- read_csv('ac_renters.csv')
## Look at dataset
ac_data

### Generalized Method of Moments in R
## Load the gmm package
library(gmm)
## Help file for the gmm function
?gmm
## Arguments for gmm function
gmm(g, x, t0, ...)

### Calculate MM estimator for binary air conditioning choice
## Create dataset for use in MM moment function
mm_data <- ac_data %>% 
  mutate(air_conditioning = 1 * air_conditioning,
         constant = 1) %>% 
  select(air_conditioning, constant, cost_system, cost_operating) %>% 
  as.matrix()
## Look at data matrix
head(mm_data)
## Function to calculate moments for air conditioning choice
mm_fn <- function(params, data){
  ## Select data for X [N x K]
  X <- data[, -1]
  ## Select data for y [N x 1]
  y <- data[, 1]
  ## Calculate representative utility of air conditioning [N x 1]
  utility <- X %*% params
  ## Calculate logit choice probability of air conditioning [N x 1]
  prob <- 1 / (1 + exp(-utility))
  ## Calculate econometric residuals [N x 1]
  residuals <- y - prob
  ## Create moment matrix [N x K]
  moments <- c(residuals) * X
  return(moments)
}
## Arguments for gmm function
gmm(g, x, t0, ...)
## Find the MM estimator
model_1 <- gmm(g = mm_fn, x = mm_data, t0 = rep(0, 3), 
               vcov = 'iid', method = 'BFGS', 
               control = list(reltol = 1e-25, maxit = 10000))
## Summarize model results
summary(model_1)
## Display model coefficients
coef(model_1)

### Calculate MM estimator for binary air conditioning choice with instruments
## Look at variable names to use as instruments
names(ac_data)
## Create dataset for use in GMM moment function
gmm_data <- ac_data %>% 
  mutate(air_conditioning = 1 * air_conditioning,
         constant = 1) %>% 
  select(air_conditioning, constant, cost_system, cost_operating,
         elec_price, square_feet, residents) %>% 
  as.matrix()
## Look at data matrix
head(gmm_data)
## Function to calculate moments for AC choice with instruments
gmm_fn <- function(params, data){
  ## Select data for X [N x K]
  X <- data[, 2:4]
  ## Select data for y [N x 1]
  y <- data[, 1]
  ## select data for Z [N x L]
  Z <- data[, c(2, 5:7)]
  ## Calculate representative utility of air conditioning [N x 1]
  utility <- X %*% params
  ## Calculate logit choice probability of air conditioning [N x 1]
  prob <- 1 / (1 + exp(-utility))
  ## Calculate econometric residuals [N x 1]
  residuals <- y - prob
  ## Create moment matrix [N x L]
  moments <- c(residuals) * Z
  return(moments)
}
## Find the GMM estimator
model_2 <- gmm(g = gmm_fn, x = gmm_data, t0 = rep(0, 3), 
               vcov = 'iid', method = 'BFGS', 
               control = list(reltol = 1e-25, maxit = 10000))
## Summarize model results
summary(model_2)
## Display model coefficients
coef(model_2)
## Test overidentifying restrictions
specTest(model_2)
