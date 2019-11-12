### ResEcon 703: Topics in Advanced Econometrics
### Lecture 18: Simulation-Based Estimation II
### Matt Woerman, UMass Amherst

### Simulation-Based Estimation Example in R -----------------------------------

### Load and look at dataset
## Load tidyverse and mlogit
library(tidyverse)
library(mlogit)
## Load dataset from mlogit package
data('HC', package = 'mlogit')
## Look at dataset
as_tibble(HC)

### Format dataset
## Gather into a long dataset
hvac_long <- HC %>% 
  mutate(id = 1:n()) %>% 
  gather(key, value, starts_with('ich.'), starts_with('och.')) %>% 
  separate(key, c('cost', 'alt')) %>% 
  spread(cost, value) %>% 
  mutate(choice = (depvar == alt)) %>% 
  select(-depvar)
## Look at long dataset
as_tibble(hvac_long)
## Combine heating and cooling costs into one variable
hvac_clean <- hvac_long %>% 
  mutate(cooling = (nchar(alt) == 3), 
         ic = if_else(cooling, ich + icca, ich),
         oc = if_else(cooling, och + occa, och)) %>% 
  mutate(cooling = 1 * cooling,
         ic = -ic,
         oc = -oc) %>% 
  select(id, alt, choice, cooling, ic, oc, income)
## Look at cleaned dataset
as_tibble(hvac_clean)

### Map function in R
## List to pass to the map function
list(1:5, 6:10)
## Take mean of each list element
list(1:5, 6:10) %>% 
  map(~ mean(.x))

### Set a seed for replication
## Random draws without setting a seed
rnorm(5)
rnorm(5)
## Random draws with the same seed
set.seed(703)
rnorm(5)
set.seed(703)
rnorm(5)
## Set seed for replication
set.seed(703)

### Draw random variable for random coefficients
## Draw standard normal random variables and split into list
draws_list <- 1:250 %>% 
  map(., ~ tibble(ic_coef = rnorm(100),
                  oc_coef = rnorm(100)))
draws_list[1]

### Organize data for MSLE optimization
## Split data into list by household
data_list <- hvac_clean %>% 
  arrange(id, alt) %>% 
  group_by(id) %>% 
  group_split()
data_list[1]

### Optimization in R
## Help file for the optimization function, optim
?optim
## Arguments for optim function
optim(par, fn, gr, ..., method, lower, upper, control, hessian)

### Find MSLE estimator for mixed logit with random coefficients
## Function to simulate choice probabilities for one household
simulate_probabilities <- function(parameters, draws, data){
  ## Select relevant variables and convert into a matrix [J * K]
  data_matrix <- data %>% 
    select(cooling, ic, oc) %>% 
    as.matrix()
  ## Transform random coefficients based on parameters [R * K]
  coefficients <- draws %>% 
    mutate(cooling_coef = parameters[1],
           ic_coef = exp(parameters[2] + parameters[4] * ic_coef),
           oc_coef = exp(parameters[3] + parameters[5] * oc_coef)) %>% 
    select(cooling_coef, ic_coef, oc_coef)
  ## Calculate utility for each alternative in each draw [R * J]
  utility <- (as.matrix(coefficients) %*% t(data_matrix)) %>% 
    pmin(700) %>% 
    pmax(-700)
  ## Sum the exponential of utility over alternatives [R * 1]
  summed_utility <- utility %>% 
    exp() %>% 
    rowSums()
  ## Calculate the conditional probability for each alternative and draw [R * J]
  conditional_probability <- exp(utility) / summed_utility
  ## Average conditional probabilities over all draws [1 * J]
  simulated_probability <- colMeans(conditional_probability)
  ## Add simulated probability to initial dataset
  data_out <- data %>% 
    mutate(probability = simulated_probability)
  ## Return initial dataset with simulated probability variable
  return(data_out)
}
## Function to calculate simulated log-likelihood
simulate_log_likelihood <- function(parameters, draws_list, data_list){
  ## Simulate probabilities for each household
  data <- map2(.x = draws_list, .y = data_list,
               .f = ~ simulate_probabilities(parameters = parameters, 
                                             draws = .x, 
                                             data = .y))
  ## Combine individual datasets into one
  data <- data %>% 
    bind_rows()
  ## Calcule the log of simulated probability for the chosen alternative
  data <- data %>% 
    filter(choice == TRUE) %>% 
    mutate(log_probability = log(probability))
  ## Calculate the simulated log-likelihood
  simulated_log_likelihood <- sum(data$log_probability)
  ## Return the negative of simulated log-likelihood
  return(-simulated_log_likelihood)
}
## Maximize the log-likelihood function
model <- optim(par = c(6.53, log(0.174), log(1.04), 0, 0), 
               fn = simulate_log_likelihood, 
               draws_list = draws_list, data_list = data_list, 
               method = 'BFGS', hessian = TRUE,
               control = list(trace = 1, REPORT = 5))
## Report model results
model
## Report parameter estimates and standard errors
model$par
model$hessian %>% 
  solve() %>% 
  diag() %>% 
  sqrt()

### Find elasticities with respect to the installation cost (ic) of
### gas central with AC (gcc)
## Function to simulate elasticities for one household
simulate_elasticities <- function(parameters, draws, data){
  ## Select relevant variables and convert into a matrix [J * K]
  data_matrix <- data %>% 
    select(cooling, ic, oc) %>% 
    as.matrix()
  ## Transform random coefficients based on parameters [R * K]
  coefficients <- draws %>% 
    mutate(cooling_coef = parameters[1],
           ic_coef = exp(parameters[2] + parameters[4] * ic_coef),
           oc_coef = exp(parameters[3] + parameters[5] * oc_coef)) %>% 
    select(cooling_coef, ic_coef, oc_coef)
  ## Calculate utility for each alternative in each draw [R * J]
  utility <- (as.matrix(coefficients) %*% t(data_matrix)) %>% 
    pmin(700) %>% 
    pmax(-700)
  ## Sum the exponential of utility over alternatives [R * 1]
  summed_utility <- utility %>% 
    exp() %>% 
    rowSums()
  ## Calculate the conditional probability for each alternative and draw [R * J]
  conditional_probability <- exp(utility) / summed_utility
  ## Average conditional probabilities over all draws [1 * J]
  simulated_probability <- colMeans(conditional_probability)
  ## Calculate integral term for own elasticity for each draw [R * 1]
  own_elasticity_integral <- coefficients$ic_coef * 
    conditional_probability[, 6] *
    (1 - conditional_probability[, 6])
  ## Calculate integral term for cross elasticities for each draw [R * (J - 1)]
  cross_elasticity_integral <- coefficients$ic_coef * 
    conditional_probability[, 6] *
    conditional_probability[, -6]
  ## Combine previous terms in correct order [R * J]
  elasticity_integral <- cross_elasticity_integral[, 1:5] %>% 
    cbind(own_elasticity_integral) %>% 
    cbind(cross_elasticity_integral[, 6]) %>% 
    unname()
  ## Average elasticity integral terms to simulate integral [1 * J]
  simulated_integral <- colMeans(elasticity_integral)
  ## Calculate own-price and cross-price simulated elasticities [1 * J]
  simulated_elasticity <- c(rep(-1, 5), 1, -1) * data$ic[6] / 
    simulated_probability * simulated_integral
  ## Add simulated elasticities to initial dataset
  data_out <- data %>% 
    mutate(elasticity = simulated_elasticity)
  ## Return initial dataset with simulated probability variable
  return(data_out)
}
## Simulate elasticities for each household
data_list <- map2(.x = draws_list, .y = data_list,
                  .f = ~ simulate_elasticities(parameters = model$par, 
                                               draws = .x, 
                                               data = .y))
## Combine list of data into one tibble
data <- data_list %>% 
  bind_rows()
## Calculate average elasticity with respect to ic of gcc
data %>% 
  group_by(alt) %>% 
  summarize(elasticity = mean(elasticity)) %>% 
  ungroup()
