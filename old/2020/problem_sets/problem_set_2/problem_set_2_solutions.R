########################################################
##### ResEcon 703: Topics in Advanced Econometrics #####
#####         Problem set 2 solution code          #####
#####         Matt Woerman, UMass Amherst          #####
########################################################

### Load packages for problem set
library(tidyverse)
library(gmm)


### Problem 1 solutions --------------------------------------------------------

### Create functions for use with maximum likelihood
## Function to summarize MLE model results
summarize_mle <- function(model, names){
  ## Extract model parameter estimates
  parameters <- model$par
  ## Calculate parameters standard errors
  std_errors <- model$hessian %>% 
    solve() %>% 
    diag() %>% 
    sqrt()
  ## Calculate parameter z-stats
  z_stats <- parameters / std_errors
  ## Calculate parameter p-values
  p_values <- 2 * pnorm(-abs(z_stats))
  ## Summarize results in a list
  model_summary <- tibble(names = names,
                          parameters = parameters,
                          std_errors = std_errors,
                          z_stats = z_stats,
                          p_values = p_values)
  ## Return model_summary object
  return(model_summary)
}
## Function to conduct likelihood ratio test
test_likelihood_ratio <- function(model_rest, model_unrest){
  ## Calculate likelihood ratio test statistic
  test_stat <- 2 * (model_rest$value - model_unrest$value)
  ## Calculate the number of restrictions
  df <- length(model_unrest$par) - length(model_rest$par)
  ## Test if likelihood ratio test statistic is greater than critical value
  test <- test_stat > qchisq(0.95, df)
  ## Calculate p-value of test
  p_value <- 1 - pchisq(test_stat, df)
  ## Return test result and p-value
  return(list(reject = test, p_value = p_value))
}

### Part a
## Load dataset
data_multi <- read_csv('commute_multinomial.csv')
## Function to calculate log-likelihood for heating choice
ll_fn_1a <- function(params, data){
  ## Extract individual parameters with descriptive names
  beta_1 <- params[1]
  beta_2 <- params[2]
  ## Calculate representative utility for each alternative given the parameters
  model_data <- data %>% 
    mutate(utility_bike = beta_1 * cost.bike + beta_2 * time.bike,
           utility_bus = beta_1 * cost.bus + beta_2 * time.bus,
           utility_car = beta_1 * cost.car + beta_2 * time.car,
           utility_walk = beta_1 * cost.walk + beta_2 * time.walk)
  ## Calculate logit choice probability denominator given the parameters
  model_data <- model_data %>% 
    mutate(prob_denom = exp(utility_bike) + exp(utility_bus) + 
             exp(utility_car) + exp(utility_walk))
  ## Calculate logit choice probability for each alt given the parameters
  model_data <- model_data %>% 
    mutate(prob_bike = exp(utility_bike) / prob_denom,
           prob_bus = exp(utility_bus) / prob_denom,
           prob_car = exp(utility_car) / prob_denom,
           prob_walk = exp(utility_walk) / prob_denom)
  ## Calculate logit choice probability for chosen alt given the parameters
  model_data <- model_data %>% 
    mutate(prob_choice = prob_bike * (mode == 'bike') + 
             prob_bus * (mode == 'bus') + prob_car * (mode == 'car') + 
             prob_walk * (mode == 'walk'))
  ## Calculate log of logit choice probability for chosen alt given the params
  model_data <- model_data %>% 
    mutate(log_prob = log(prob_choice))
  ## Calculate the log-likelihood for these parameters
  ll <- sum(model_data$log_prob)
  return(-ll)
}
## Maximize the log-likelihood function
model_1a <- optim(par = rep(0, 2), fn = ll_fn_1a, data = data_multi,
                  method = 'BFGS', hessian = TRUE)
## Summarize model results
model_1a %>% 
  summarize_mle(c('cost', 'time'))

### Part b
## Function to calculate log-likelihood for heating choice
ll_fn_1b <- function(params, data){
  ## Extract individual parameters with descriptive names
  alpha_bus <- params[1]
  alpha_car <- params[2]
  alpha_walk <- params[3]
  beta_1 <- params[4]
  beta_2 <- params[5]
  ## Calculate representative utility for each alternative given the parameters
  model_data <- data %>% 
    mutate(utility_bike = beta_1 * cost.bike + beta_2 * time.bike,
           utility_bus = alpha_bus + beta_1 * cost.bus + beta_2 * time.bus,
           utility_car = alpha_car + beta_1 * cost.car + beta_2 * time.car,
           utility_walk = alpha_walk + beta_1 * cost.walk + beta_2 * time.walk)
  ## Calculate logit choice probability denominator given the parameters
  model_data <- model_data %>% 
    mutate(prob_denom = exp(utility_bike) + exp(utility_bus) + 
             exp(utility_car) + exp(utility_walk))
  ## Calculate logit choice probability for each alt given the parameters
  model_data <- model_data %>% 
    mutate(prob_bike = exp(utility_bike) / prob_denom,
           prob_bus = exp(utility_bus) / prob_denom,
           prob_car = exp(utility_car) / prob_denom,
           prob_walk = exp(utility_walk) / prob_denom)
  ## Calculate logit choice probability for chosen alt given the parameters
  model_data <- model_data %>% 
    mutate(prob_choice = prob_bike * (mode == 'bike') + 
             prob_bus * (mode == 'bus') + prob_car * (mode == 'car') + 
             prob_walk * (mode == 'walk'))
  ## Calculate log of logit choice probability for chosen alt given the params
  model_data <- model_data %>% 
    mutate(log_prob = log(prob_choice))
  ## Calculate the log-likelihood for these parameters
  ll <- sum(model_data$log_prob)
  return(-ll)
}
## Maximize the log-likelihood function
model_1b <- optim(par = rep(0, 5), fn = ll_fn_1b, data = data_multi,
                  method = 'BFGS', hessian = TRUE)
## Summarize model results
model_1b %>% 
  summarize_mle(c('bus_int', 'car_int', 'walk_int', 'cost', 'time'))
## Conduct likelihood ratio test of models 1a and 1b
test_1b <- test_likelihood_ratio(model_1a, model_1b)
## Display test results
test_1b

### Part c
## Function to calculate log-likelihood for heating choice
ll_fn_1c <- function(params, data){
  ## Extract individual parameters with descriptive names
  alpha_bus <- params[1]
  alpha_car <- params[2]
  alpha_walk <- params[3]
  beta <- params[4]
  gamma_bike <- params[5]
  gamma_bus <- params[6]
  gamma_car <- params[7]
  gamma_walk <- params[8]
  ## Calculate representative utility for each alternative given the parameters
  model_data <- data %>% 
    mutate(utility_bike = beta * cost.bike + gamma_bike * time.bike,
           utility_bus = alpha_bus + beta * cost.bus + gamma_bus * time.bus,
           utility_car = alpha_car + beta * cost.car + gamma_car * time.car,
           utility_walk = alpha_walk + beta * cost.walk + 
             gamma_walk * time.walk)
  ## Calculate logit choice probability denominator given the parameters
  model_data <- model_data %>% 
    mutate(prob_denom = exp(utility_bike) + exp(utility_bus) + 
             exp(utility_car) + exp(utility_walk))
  ## Calculate logit choice probability for each alt given the parameters
  model_data <- model_data %>% 
    mutate(prob_bike = exp(utility_bike) / prob_denom,
           prob_bus = exp(utility_bus) / prob_denom,
           prob_car = exp(utility_car) / prob_denom,
           prob_walk = exp(utility_walk) / prob_denom)
  ## Calculate logit choice probability for chosen alt given the parameters
  model_data <- model_data %>% 
    mutate(prob_choice = prob_bike * (mode == 'bike') + 
             prob_bus * (mode == 'bus') + prob_car * (mode == 'car') + 
             prob_walk * (mode == 'walk'))
  ## Calculate log of logit choice probability for chosen alt given the params
  model_data <- model_data %>% 
    mutate(log_prob = log(prob_choice))
  ## Calculate the log-likelihood for these parameters
  ll <- sum(model_data$log_prob)
  return(-ll)
}
## Maximize the log-likelihood function
model_1c <- optim(par = rep(0, 8), fn = ll_fn_1c, data = data_multi,
                  method = 'BFGS', hessian = TRUE)
## Summarize model results
model_1c %>% 
  summarize_mle(c('bus_int', 'car_int', 'walk_int', 'cost', 
                  'time_bike', 'time_bus', 'time_car', 'time_walk'))
## Conduct likelihood ratio test of models 1a and 1b
test_1c <- test_likelihood_ratio(model_1b, model_1c)
## Display test results
test_1c


### Problem 2 solutions --------------------------------------------------------

### Part a
## Load dataset
data_binary <- read_csv('commute_binary.csv')
## Create dataset for use in MM moment function
data_2a <- data_binary %>% 
  mutate(choice = 1 * (mode == 'car'),
         constant = 1,
         time.bus = -time.bus) %>% 
  select(choice, constant, cost.car, time.car, time.bus) %>% 
  as.matrix()
## Function to calculate moments for air conditioning choice
mm_fn_2a <- function(params, data){
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
## Use GMM to estimate model
model_2a <- gmm(g = mm_fn_2a, x = data_2a, t0 = rep(0, 4), vcov = 'iid', 
                control = list(reltol = 1e-25, maxit = 10000))
## Summarize model results
summary(model_2a)

### Part b
## Create dataset for use in MM moment function
data_2b <- data_binary %>% 
  mutate(choice = 1 * (mode == 'car'),
         constant = 1,
         time.bus = -time.bus) %>% 
  select(choice, constant, cost.car, time.car, time.bus, 
         price_gas, snowfall, construction, bus_detour) %>% 
  as.matrix()
## Function to calculate moments for air conditioning choice
gmm_fn_2b <- function(params, data){
  ## Select data for X [N x K]
  X <- data[, 2:5]
  ## Select data for y [N x 1]
  y <- data[, 1]
  ## select data for Z [N x L]
  Z <- data[, c(2, 6:9)]
  ## Calculate representative utility of air conditioning [N x 1]
  utility <- X %*% params
  ## Calculate logit choice probability of air conditioning [N x 1]
  prob <- 1 / (1 + exp(-utility))
  ## Calculate econometric residuals [N x 1]
  residuals <- y - prob
  ## Create moment matrix [N x K]
  moments <- c(residuals) * Z
  return(moments)
}
## Use GMM to estimate model
model_2b <- gmm(g = gmm_fn_2b, x = data_2b, t0 = rep(0, 4), vcov = 'iid', 
                control = list(reltol = 1e-25, maxit = 10000))
## Summarize model results
summary(model_2b)
## Test overidentifying restrictions
specTest(model_2b)
