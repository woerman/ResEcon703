### ResEcon 703: Topics in Advanced Econometrics
### Lecture 12: Generalized Method of Moments II
### Matt Woerman, UMass Amherst

### Generalized Method of Moments Example in R ---------------------------------

### Load and look at dataset
## Load tidyverse
library(tidyverse)
## Load dataset
data <- read_csv('ac_renters.csv')
## Look at dataset
data

### Generalized Method of Moments in R
## Load the gmm package
library(gmm)
## Help file for the gmm function
?gmm
## Arguments for gmm function
gmm(g, x, t0, ...)

### Calculate GMM estimator for binary logit AC choice using cost data 
## Function to calculate binary logit moments
calculate_moments <- function(parameters, data){
  ## Extract explanatory variable data from matrix
  data_x <- data[, -1]
  ## Extract choice data from matrix
  data_y <- data[, 1]
  ## Calculate net utility of alternative given the parameters
  utility <- data_x %*% parameters
  ## Caclculate logit probability of alternative given the parameters
  probability_choice <- 1 / (1 + exp(-utility))
  ## Calculate residuals
  residuals <- data_y - probability_choice
  ## Create moment matrix
  moments <- c(residuals) * data_x
  return(moments)
}
## Create dataset for use in GMM moment function
data_gmm <- data %>% 
  mutate(air_conditioning = 1 * air_conditioning,
         constant = 1) %>% 
  select(air_conditioning, constant, cost_system, cost_operating) %>% 
  as.matrix()
## Find the GMM estimator
model_gmm <- gmm(calculate_moments, data_gmm, rep(0, 3), vcov = 'iid',
                 control = list(reltol = 1e-25, maxit = 10000))
## Summarize GMM model results
model_gmm %>% 
  summary()

### Calculate GMM estimator for binary logit AC choice using cost data
### instrumenting for operating cost with elec price and square feet
## Function to calculate binary logit moments with instruments
calculate_moments_iv <- function(parameters, data){
  ## Extract explanatory variable data from matrix
  data_x <- data[, 2:4]
  ## Extract choice data from matrix
  data_y <- data[, 1]
  ## Extract instrument data from matrix
  data_z <- data[, c(2:3, 5:6)]
  ## Calculate net utility of alternative given the parameters
  utility <- data_x %*% parameters
  ## Caclculate logit probability of alternative given the parameters
  probability_choice <- 1 / (1 + exp(-utility))
  ## Calculate residuals
  residuals <- data_y - probability_choice
  ## Create moment matrix
  moments <- c(residuals) * data_z
  return(moments)
}
## Create dataset for use in GMM IV moment function
data_gmm_iv <- data %>% 
  mutate(air_conditioning = 1 * air_conditioning,
         constant = 1) %>% 
  select(air_conditioning, constant, cost_system, cost_operating,
         elec_price, square_feet) %>% 
  as.matrix()
## Find the GMM IV estimator
model_gmm_iv <- gmm(calculate_moments_iv, data_gmm_iv, rep(0, 3), 
                    vcov = 'iid',
                    control = list(reltol = 1e-25, maxit = 10000))

## Summarize GMM IV model results
model_gmm_iv %>% 
  summary()
## Test overidentifying restrictions
model_gmm_iv %>% 
  specTest()
