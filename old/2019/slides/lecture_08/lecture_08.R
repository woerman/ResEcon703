### ResEcon 703: Topics in Advanced Econometrics
### Lecture 8: Logit Estimation
### Matt Woerman, UMass Amherst

### Logit Estimation Example in R ----------------------------------------------

### Load and look at dataset
## Load tidyverse and mlogit
library(tidyverse)
library(mlogit)
## Load dataset from mlogit package
data('Heating', package = 'mlogit')
## Look at dataset
as_tibble(Heating)

### Optimization in R
## Help file for the optimization function, optim
?optim
## Arguments for optim function
optim(par, fn, gr, ..., method, lower, upper, control, hessian)

### Calculate MLE for multinomial logit heating choice using cost data and 
### alternative effects
## Function to calculate log-likelihood using cost data and alternative effects
log_likelihood_wide <- function(parameters, data){
  ## Extract individual parameters
  alpha_1 <- parameters[1]
  alpha_2 <- parameters[2]
  alpha_3 <- parameters[3]
  alpha_4 <- parameters[4]
  beta_1 <- parameters[5]
  beta_2 <- parameters[6]
  ## Calculate utility for each alternative given the parameters
  data <- data %>% 
    mutate(utility_ec = alpha_1 + beta_1 * ic.ec + beta_2 * oc.ec,
           utility_er = alpha_2 + beta_1 * ic.er + beta_2 * oc.er,
           utility_gc = alpha_3 + beta_1 * ic.gc + beta_2 * oc.gc,
           utility_gr = alpha_4 + beta_1 * ic.gr + beta_2 * oc.gr,
           utility_hp = beta_1 * ic.hp + beta_2 * oc.hp)
  ## Calculate logit probability denominator given the parameters
  data <- data %>% 
    mutate(probability_denominator = exp(utility_ec) + exp(utility_er) +
             exp(utility_gc) + exp(utility_gr) + exp(utility_hp))
  ## Calculate logit probability numerator given the parameters
  data <- data %>% 
    mutate(probability_numerator = exp(utility_ec) * (depvar == 'ec') +
             exp(utility_er) * (depvar == 'er') + 
             exp(utility_gc) * (depvar == 'gc') +
             exp(utility_gr) * (depvar == 'gr') + 
             exp(utility_hp) * (depvar == 'hp'))
  ## Calculate log of logit choice probability given the parameters
  data <- data %>% 
    mutate(probability_choice = probability_numerator / probability_denominator,
           log_probability_choice = log(probability_choice))
  ## Calculate the log-likelihood for these parameters
  log_likelihood <- sum(data$log_probability_choice)
  return(-log_likelihood)
}
## Maximize the log-likelihood function
model_wide <- optim(rep(0, 6), log_likelihood_wide, data = Heating, 
                    method = 'BFGS', hessian = TRUE)
## Show optimization results
model_wide
## Show MLE parameters
model_wide$par
## Show Hessian at the MLE
model_wide$hessian
## Calculate MLE variance-covariance matrix
model_wide$hessian %>% 
  solve()
## Calculate MLE standard errors
model_wide$hessian %>% 
  solve() %>% 
  diag() %>% 
  sqrt()

### Calculate MLE for multinomial logit heating choice using cost data scaled 
### by income and alternative effects
## Function to calculate log-likelihood using two variables and four 
## alternative effects
log_likelihood_long <- function(parameters, data){
  ## Extract individual parameters
  alphas <- parameters[1:4]
  beta_1 <- parameters[5]
  beta_2 <- parameters[6]
  ## Assign constant parameters to alternatives
  data <- data %>% 
    group_by(idcase) %>% 
    arrange(idcase, alt) %>% 
    mutate(constant = c(alphas, 0)) %>% 
    ungroup()
  ## Calculate utility for each alternative given the parameters
  data <- data %>% 
    mutate(utility = constant + beta_1 * var_1 + beta_2 * var_2)
  ## Calculate logit probability denominator given the parameters
  data <- data %>% 
    group_by(idcase) %>% 
    mutate(probability_denominator = sum(exp(utility))) %>% 
    ungroup()
  ## Filter on only the alternative that was chosen
  data <- data %>% 
    filter(choice == TRUE)
  ## Calculate log of logit choice probability given the parameters
  data <- data %>%
    mutate(probability_choice = exp(utility) / probability_denominator,
           log_probability_choice = log(probability_choice))
  ## Calculate the log-likelihood for these parameters
  log_likelihood <- sum(data$log_probability_choice)
  return(-log_likelihood)
}
## Gather heating dataset into a long dataset
heating_long <- Heating %>% 
  gather(key, value, starts_with('ic.'), starts_with('oc.')) %>% 
  separate(key, c('cost', 'alt')) %>% 
  spread(cost, value) %>% 
  mutate(choice = (depvar == alt)) %>% 
  select(-depvar) %>% 
  mutate(var_1 = ic / income, var_2 = oc / income)
## Maximize the log-likelihood function
model_long <- optim(rep(0, 6), log_likelihood_long, data = heating_long, 
                    method = 'BFGS', hessian = TRUE)
## Show MLE parameters
model_long$par
## Calculate MLE standard errors
model_long$hessian %>% 
  solve() %>% 
  diag() %>% 
  sqrt()

### Calculate MLE for multinomial logit heating choice using cost data, 
### alternative effects, and alternative-specific coefficients on rooms
## Function to calculate log-likelihood using flexible matrices
log_likelihood_matrix <- function(parameters, data){
  ## Extract explanatory variables
  data_x <- data %>% 
    map(1)
  ## Extract choice data
  data_y <- data %>% 
    map(2)
  ## Calculate utility for each alternative given the parameters
  utility <- data_x %>% 
    map(~ .x %*% parameters)
  ## Calculate logit probability denominator given the parameters
  probability_denominator <- utility %>% 
    map(~ sum(exp(.x))) %>% 
    unlist()
  ## Calculate logit probability numerator given the parameters
  probability_numerator <- utility %>% 
    map2(data_y, ~ exp(sum(.x * .y))) %>% 
    unlist()
  ## Calculate logit choice probability given the parameters
  probability_choice <-  probability_numerator / probability_denominator
  ## Calculate log of logit choice probability given the parameters
  log_probability_choice <- log(probability_choice)
  ## Calculate the log-likelihood for these parameters
  log_likelihood <- sum(log_probability_choice)
  return(-log_likelihood)
}
## Split heating dataset into list of household data frames
heating_split <- heating_long %>% 
  group_by(idcase) %>% 
  arrange(idcase, alt) %>% 
  group_split()
## Crate matrix of dummy variables (intercepts) to bind to data
constant_matrix <- diag(4) %>% 
  rbind(c(0, 0, 0, 0))
## Function to create list of datasets for estimation
create_data_matrix <- function(data){
  data_x <- data %>% 
    select(ic, oc) %>% 
    as.matrix()
  data_x <- constant_matrix %>% 
    cbind(data_x)
  rooms <- data$rooms[1]
  data_x <- 
    data_x %>% 
    cbind(rooms * constant_matrix)
  data_y <- data %>% 
    select(choice) %>% 
    mutate(choice = 1 * choice) %>% 
    pull(choice)
  return(list(x = data_x, y = data_y))
}
## Create list of datasets for estimation
heating_matrix <- heating_split %>% 
  map(.x = ., .f = ~ create_data_matrix(.x))
## Maximize the log-likelihood function
model_matrix <- optim(rep(0, 10), log_likelihood_matrix, 
                      data = heating_matrix, 
                      method = 'BFGS', hessian = TRUE)
## Show MLE parameters
model_matrix$par
## Calculate MLE standard errors
model_matrix$hessian %>% 
  solve() %>% 
  diag() %>% 
  sqrt()

### Test that alternative-specific coefficients are jointly significant
## Calculate likelihood ratio test statistic
lr_test_statistic <- -2 * (model_matrix$value - model_wide$value)
lr_test_statistic
## Find chi-squared critical value for df = 4
qchisq(0.95, 4)
## Test if likelihood ratio test statitsic is greater than critical value
lr_test_statistic > qchisq(0.95, 4)
## Calculate p-value of test
1 - pchisq(lr_test_statistic, 4)
