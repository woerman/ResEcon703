########################################################
##### ResEcon 703: Topics in Advanced Econometrics #####
#####     Week 11: Simulation-Based Estimation     #####
#####         Matt Woerman, UMass Amherst          #####
########################################################

### Simulation-Based Estimation R Example --------------------------------------

### Load and look at dataset
## Load tidyverse and mlogit
library(tidyverse)
library(mlogit)
## Load dataset from mlogit package
data('HC', package = 'mlogit')
## Look at dataset
tibble(HC)

### Format dataset
## Pivot into a long dataset
hvac_long <- HC %>% 
  mutate(id = 1:n()) %>% 
  pivot_longer(c(starts_with('ich.'), starts_with('och.')), 
               names_to = c('cost', 'alt'), names_sep = '[.]',
               values_to = 'value') %>% 
  pivot_wider(names_from = cost, values_from = value) %>% 
  mutate(choice = (depvar == alt)) %>% 
  select(-depvar)
## Look at long dataset
tibble(hvac_long)
## Combine heating and cooling costs into one variable
hvac_clean <- hvac_long %>% 
  mutate(ac = 1 * (nchar(alt) == 3), 
         ic = ich + ac * icca,
         oc = och + ac * occa) %>% 
  select(id, alt, choice, ac, ic, oc, income) %>% 
  arrange(id, alt)
## Look at cleaned dataset
tibble(hvac_clean)

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
## Draw standard normal random variables for each household
draws_hh <- map(1:250, ~ tibble(ac_draw = rnorm(100),
                                ic_draw = rnorm(100),
                                oc_draw = rnorm(100)))
draws_hh[[1]]

### Organize data for MSL optimization
## Split data into list by household
data_hh <- hvac_clean %>% 
  group_by(id) %>% 
  group_split()
data_hh[[1]]

### Optimization in R
## Help file for the optimization function, optim
?optim
## Arguments for optim function
optim(par, fn, gr, ..., method, lower, upper, control, hessian)

### Find MSL estimator for mixed logit with random coefficients
## Function to simulate choice probabilities for an individual household
sim_probs_ind <- function(params, draws_ind, data_ind){
  ## Select relevant variables and convert into a matrix [J x K]
  data_matrix <- data_ind %>% 
    select(ac, ic, oc) %>% 
    as.matrix()
  ## Transform random coefficients based on parameters [R x K]
  coef_matrix <- draws_ind %>% 
    mutate(ac_coef = params[1] + params[4] * ac_draw,
           ic_coef = params[2] + params[5] * ic_draw,
           oc_coef = params[3] + params[6] * oc_draw) %>% 
    select(ac_coef, ic_coef, oc_coef) %>% 
    as.matrix()
  ## Calculate representative utility for each alternative in each draw [R x J]
  utility <- (coef_matrix %*% t(data_matrix)) %>% 
    pmin(700) %>% 
    pmax(-700)
  ## Sum the exponential of utility over alternatives [R x 1]
  prob_denom <- utility %>% 
    exp() %>% 
    rowSums()
  ## Calculate the conditional probability for each alternative and draw [R x J]
  cond_prob <- exp(utility) / prob_denom
  ## Calculate simulated choice probabilities as means over all draws [1 x J]
  sim_prob <- colMeans(cond_prob)
  ## Add simulated probability to initial dataset
  data_ind_out <- data_ind %>% 
    mutate(prob = sim_prob)
  ## Return initial dataset with simulated probability variable
  return(data_ind_out)
}
## Function to calculate simulated log-likelihood
sim_ll_fn <- function(params, draws_list, data_list){
  ## Simulate probabilities for each individual household
  data_sim_ind <- map2(.x = draws_list, .y = data_list,
                       .f = ~ sim_probs_ind(params = params, 
                                            draws_ind = .x, 
                                            data_ind = .y))
  ## Combine individual datasets into one
  data_sim <- data_sim_ind %>% 
    bind_rows()
  ## Calculate log of simulated probability for the chosen alternative
  data_sim <- data_sim %>% 
    filter(choice == TRUE) %>% 
    mutate(log_prob = log(prob))
  ## Calculate the simulated log-likelihood
  sim_ll <- sum(data_sim$log_prob)
  ## Return the negative of simulated log-likelihood
  return(-sim_ll)
}
## Maximize the log-likelihood function
model <- optim(par = c(6.53, -0.17, -1.04, 0, 0, 0), fn = sim_ll_fn,
               draws_list = draws_hh, data_list = data_hh, 
               method = 'BFGS', hessian = TRUE,
               control = list(trace = 1, REPORT = 5))
## Report model results
model
## Show MSL parameters
model$par
## Calculate MSL standard errors
model_se <- model$hessian %>% 
  solve() %>% 
  diag() %>% 
  sqrt()
model_se
## Calculate parameter z-stats
model_zstat <- model$par / model_se
model_zstat
## Calculate parameter p-values
model_pvalue <- 2 * pnorm(q = -abs(model_zstat))
model_pvalue

### Find elasticities with respect to the installation cost (ic) of hpc
## Function to simulate elasticities for one household
sim_elas_ind <- function(params, draws_ind, data_ind){
  ## Select relevant variables and convert into a matrix [J x K]
  data_matrix <- data_ind %>% 
    select(ac, ic, oc) %>% 
    as.matrix()
  ## Transform random coefficients based on parameters [R x K]
  coef_matrix <- draws_ind %>% 
    mutate(ac_coef = params[1] + params[4] * ac_draw,
           ic_coef = params[2] + params[5] * ic_draw,
           oc_coef = params[3] + params[6] * oc_draw) %>% 
    select(ac_coef, ic_coef, oc_coef) %>% 
    as.matrix()
  ## Calculate representative utility for each alternative in each draw [R x J]
  utility <- (coef_matrix %*% t(data_matrix)) %>% 
    pmin(700) %>% 
    pmax(-700)
  ## Sum the exponential of utility over alternatives [R x 1]
  prob_denom <- utility %>% 
    exp() %>% 
    rowSums()
  ## Calculate the conditional probability for each alternative and draw [R x J]
  cond_prob <- exp(utility) / prob_denom
  ## Calculate simulated choice probabilities as means over all draws [1 x J]
  sim_prob <- colMeans(cond_prob)
  ## Calculate simulated integral for own elasticity [1 x 1]
  sim_int_own_elas <- mean(coef_matrix[, 2] * 
                             cond_prob[, 7] * (1 - cond_prob[, 7]))
  ## Calculate simulated integral for cross elasticities [1 x (J - 1)]
  sim_int_cross_elas <- colMeans(coef_matrix[, 2] * 
                                   cond_prob[, 7] * cond_prob[, -7])
  ## Combine elasticity simulated integrals into one vector [1 x J]
  sim_int_elas <- c(sim_int_cross_elas, sim_int_own_elas)
  ## Calculate cross-price and own-price simulated elasticities [1 x J]
  sim_elas <- c(rep(-1, 6), 1) * data_ind$ic[7] / sim_prob * sim_int_elas
  ## Add simulated elasticities to initial dataset
  data_ind_out <- data_ind %>% 
    mutate(elasticity = sim_elas)
  ## Return initial dataset with simulated elasticity variable
  return(data_ind_out)
}
## Simulate elasticities for each household
data_ind <- map2(.x = draws_hh, .y = data_hh,
                 .f = ~ sim_elas_ind(params = model$par, 
                                     draws_ind = .x, 
                                     data_ind = .y))
## Combine list of data into one tibble
data <- data_ind %>% 
  bind_rows()
## Calculate average elasticity with respect to ic of hpc
data %>% 
  group_by(alt) %>% 
  summarize(elasticity = mean(elasticity), .groups = 'drop')
