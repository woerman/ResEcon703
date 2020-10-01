########################################################
##### ResEcon 703: Topics in Advanced Econometrics #####
#####           Week 7: Logit Estimation           #####
#####         Matt Woerman, UMass Amherst          #####
########################################################

### Logit Estimation R Example -------------------------------------------------

### Load and look at dataset
## Load tidyverse and mlogit
library(tidyverse)
library(mlogit)
## Load dataset from mlogit package
data('Heating', package = 'mlogit')
## Rename dataset to lowercase
heating <- Heating
## Look at dataset
tibble(heating)

### Optimization in R
## Help file for the optimization function, optim
?optim
## Arguments for optim function
optim(par, fn, gr, ..., method, lower, upper, control, hessian)

### Calculate MLE for multinomial logit heating choice
## Function to calculate log-likelihood for heating choice
ll_fn_1 <- function(params, data){
  ## Extract individual parameters with descriptive names
  alpha_ec <- params[1]
  alpha_er <- params[2]
  alpha_gc <- params[3]
  alpha_gr <- params[4]
  beta_1 <- params[5]
  beta_2 <- params[6]
  ## Calculate representative utility for each alternative given the parameters
  model_data <- data %>% 
    mutate(utility_ec = alpha_ec + beta_1 * ic.ec + beta_2 * oc.ec,
           utility_er = alpha_er + beta_1 * ic.er + beta_2 * oc.er,
           utility_gc = alpha_gc + beta_1 * ic.gc + beta_2 * oc.gc,
           utility_gr = alpha_gr + beta_1 * ic.gr + beta_2 * oc.gr,
           utility_hp = beta_1 * ic.hp + beta_2 * oc.hp)
  ## Calculate logit choice probability denominator given the parameters
  model_data <- model_data %>% 
    mutate(prob_denom = exp(utility_ec) + exp(utility_er) + exp(utility_gc) + 
             exp(utility_gr) + exp(utility_hp))
  ## Calculate logit choice probability for each alt given the parameters
  model_data <- model_data %>% 
    mutate(prob_ec = exp(utility_ec) / prob_denom,
           prob_er = exp(utility_er) / prob_denom,
           prob_gc = exp(utility_gc) / prob_denom,
           prob_gr = exp(utility_gr) / prob_denom,
           prob_hp = exp(utility_hp) / prob_denom)
  ## Calculate logit choice probability for chosen alt given the parameters
  model_data <- model_data %>% 
    mutate(prob_choice = prob_ec * (depvar == 'ec') + 
             prob_er * (depvar == 'er') + prob_gc * (depvar == 'gc') + 
             prob_gr * (depvar == 'gr') + prob_hp * (depvar == 'hp'))
  ## Calculate log of logit choice probability for chosen alt given the params
  model_data <- model_data %>% 
    mutate(log_prob = log(prob_choice))
  ## Calculate the log-likelihood for these parameters
  ll <- sum(model_data$log_prob)
  return(-ll)
}
## Arguments for optim function
optim(par, fn, gr, ..., method, lower, upper, control, hessian)
## Maximize the log-likelihood function
model_1 <- optim(par = rep(0, 6), fn = ll_fn_1, data = heating, 
                 method = 'BFGS', hessian = TRUE)
## Show optimization results
model_1
## Show MLE parameters
model_1$par
## Show Hessian at the MLE
model_1$hessian
## Calculate MLE variance estimator
model_1$hessian %>% 
  solve()
## Calculate MLE standard errors
model_1$hessian %>% 
  solve() %>% 
  diag() %>% 
  sqrt()

### Calculate MLE for multinomial logit heating choice another way
## Pivot heating dataset into a long dataset
heating_long <- heating %>% 
  pivot_longer(contains('.')) %>% 
  separate(name, c('name', 'alt')) %>% 
  pivot_wider() %>% 
  mutate(choice = (depvar == alt)) %>% 
  select(-depvar) %>% 
  arrange(idcase, alt)
## Add columns of ones to long dataset for alt-specific constant terms
heating_long <- heating_long %>% 
  mutate(constant_ec = 1 * (alt == 'ec'),
         constant_er = 1 * (alt == 'er'),
         constant_gc = 1 * (alt == 'gc'),
         constant_gr = 1 * (alt == 'gr'))
## Look at long dataset
heating_long %>% 
  select(idcase, alt, choice, ic, oc, starts_with('constant'))
## Function to calculate log-likelihood for heating choice
ll_fn_2 <- function(params, data){
  ## Select data for X and convert to a matrix
  X <- data %>% 
    select(starts_with('constant'), ic, oc) %>% 
    as.matrix()
  ## Calculate representative utility for each alternative given the parameters
  model_data <- data %>% 
    mutate(utility = X %*% params)
  ## Calculate logit probability denominator given the parameters
  model_data <- model_data %>% 
    group_by(idcase) %>% 
    mutate(prob_denom = sum(exp(utility))) %>% 
    ungroup()
  ## Calculate logit choice probability for each alt given the parameters
  model_data <- model_data %>% 
    mutate(prob = exp(utility) / prob_denom)
  ## Keep logit choice probability for only the chosen alt given the parameters
  model_data <- model_data %>% 
    filter(choice == TRUE)
  ## Calculate log of logit choice probability for chosen alt given the params
  model_data <- model_data %>% 
    mutate(log_prob = log(prob))
  ## Calculate the log-likelihood for these parameters
  ll <- sum(model_data$log_prob)
  return(-ll)
}
## Maximize the log-likelihood function
model_2 <- optim(par = rep(0, 6), fn = ll_fn_2, data = heating_long, 
                 method = 'BFGS', hessian = TRUE)
## Show optimization results
model_2
## Show MLE parameters
model_2$par
## Show Hessian at the MLE
model_2$hessian
## Calculate MLE variance estimator
model_2$hessian %>% 
  solve()
## Calculate MLE standard errors
model_2$hessian %>% 
  solve() %>% 
  diag() %>% 
  sqrt()

### Calculate MLE for multinomial logit heating choice yet another way
## Function to calculate log-likelihood for heating choice
ll_fn_3 <- function(params, data){
  ## Select data for X and convert to a matrix [(N * J) x K]
  X <- data %>% 
    select(starts_with('constant'), ic, oc) %>% 
    as.matrix()
  ## Calculate representative utility for each alternative [(N * J) x 1]
  utility <- X %*% params
  ## Convert utility to a wide matrix [N x J]
  utility <- matrix(data = utility, ncol = 5, byrow = TRUE)
  ## Calculate logit probability denominator [N x 1]
  prob_denom <- rowSums(exp(utility))
  ## Calculate logit choice probability for each alternative [N x J]
  prob <- exp(utility) / prob_denom
  ## Select data for y and convert to a wide matrix [N x J]
  y <- matrix(data = data$choice, ncol = 5, byrow = TRUE)
  ## Calculate logit choice probability for the chosen alternatives [N x 1]
  prob_choice <- rowSums(y * prob)
  ## Calculate log of logit choice probability for chosen alternatives [N x 1]
  log_prob <- log(prob_choice)
  ## Calculate the log-likelihood [1 x 1]
  ll <- sum(log_prob)
  return(-ll)
}
## Maximize the log-likelihood function
model_3 <- optim(par = rep(0, 6), fn = ll_fn_3, data = heating_long, 
                 method = 'BFGS', hessian = TRUE)
## Show optimization results
model_3
## Show MLE parameters
model_3$par
## Show Hessian at the MLE
model_3$hessian
## Calculate MLE variance estimator
model_3$hessian %>% 
  solve()
## Calculate MLE standard errors
model_3$hessian %>% 
  solve() %>% 
  diag() %>% 
  sqrt()

### Test that alternative-specific constants are jointly significant
## Function to calculate restricted log-likelihood for heating choice
ll_fn_2_rest <- function(params, data){
  ## Select data for X and convert to a matrix
  X <- data %>% 
    select(ic, oc) %>% 
    as.matrix()
  ## Calculate representative utility for each alternative given the parameters
  model_data <- data %>% 
    mutate(utility = X %*% params)
  ## Calculate logit probability denominator given the parameters
  model_data <- model_data %>% 
    group_by(idcase) %>% 
    mutate(prob_denom = sum(exp(utility))) %>% 
    ungroup()
  ## Calculate logit choice probability for each alt given the parameters
  model_data <- model_data %>% 
    mutate(prob = exp(utility) / prob_denom)
  ## Keep logit choice probability for only the chosen alt given the parameters
  model_data <- model_data %>% 
    filter(choice == TRUE)
  ## Calculate log of logit choice probability for chosen alt given the params
  model_data <- model_data %>% 
    mutate(log_prob = log(prob))
  ## Calculate the log-likelihood for these parameters
  ll <- sum(model_data$log_prob)
  return(-ll)
}
## Maximize the log-likelihood function
model_2_rest <- optim(par = rep(0, 2), fn = ll_fn_2_rest, 
                      data = heating_long, 
                      method = 'BFGS', hessian = TRUE)
## Show optimization results
model_2_rest
## Look at negative of log-likelihood value for each model
model_2$value
model_2_rest$value
## Calculate likelihood ratio test statistic
lr_stat <- 2 * (-model_2$value - -model_2_rest$value)
lr_stat
## Find chi-squared critical value for 4 degrees of freedom
qchisq(0.95, 4)
## Test if likelihood ratio test statistic is greater than critical value
lr_stat > qchisq(0.95, 4)
## Calculate p-value of test
1 - pchisq(lr_stat, 4)