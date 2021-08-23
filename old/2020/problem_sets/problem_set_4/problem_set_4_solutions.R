########################################################
##### ResEcon 703: Topics in Advanced Econometrics #####
#####         Problem set 4 solution code          #####
#####         Matt Woerman, UMass Amherst          #####
########################################################

### Load packages for problem set
library(tidyverse)


### Problem 1 solutions --------------------------------------------------------

### Part a
## Load dataset
data_camping <- read_csv('camping.csv')
## Set seed for replication
set.seed(703)
## Draw standard normal random variables and split into list
draws_camper <- map(1:1000, ~ tibble(time_draw = rnorm(100),
                                     mountain_draw = rnorm(100)))
## Split data into list by camper
data_camper <- data_camping %>% 
  group_by(camper_id) %>% 
  group_split()
## Function to simulate choice probabilities for one individual
sim_probs_ind <- function(params, draws_ind, data_ind){
  ## Select relevant variables and convert into a matrix
  data_matrix <- data_ind %>% 
    select(cost, time, mountain) %>% 
    as.matrix()
  ## Transform random draws into coefficients based on parameters
  coef_matrix <- draws_ind %>% 
    mutate(cost_coef = params[1],
           time_coef = params[2] + params[4] * time_draw,
           mountain_coef = params[3] + params[5] * mountain_draw) %>% 
    select(cost_coef, time_coef, mountain_coef) %>% 
    as.matrix()
  ## Calculate representative utility for each alternative in each draw
  utility <- (coef_matrix %*% t(data_matrix)) %>% 
    pmin(700) %>% 
    pmax(-700)
  ## Sum the exponential of utility over alternatives
  prob_denom <- utility %>% 
    exp() %>% 
    rowSums()
  ## Calculate the conditional probability for each alternative in each draw
  cond_prob <- exp(utility) / prob_denom
  ## Calculate simulated choice probabilities as the means over all draws
  sim_prob <- colMeans(cond_prob)
  ## Add simulated choice probability to initial dataset
  data_ind_out <- data_ind %>% 
    mutate(prob = sim_prob)
  ## Return initial dataset with simulated probability variable
  return(data_ind_out)
}
## Function to calculate simulated log-likelihood
sim_ll_fn <- function(params, draws_list, data_list){
  ## Simulate probabilities for each individual
  data_sim_ind <- map2(.x = draws_list, .y = data_list,
                       .f = ~ sim_probs_ind(params = params, 
                                            draws_ind = .x, 
                                            data_ind = .y))
  ## Combine individual datasets into one
  data_sim <- data_sim_ind %>% 
    bind_rows()
  ## Calculate the log of simulated probability for the chosen alternative
  data_sim <- data_sim %>% 
    filter(visit == 1) %>% 
    mutate(log_prob = log(prob))
  ## Calculate the simulated log-likelihood
  sim_ll <- sum(data_sim$log_prob)
  ## Return the negative of simulated log-likelihood
  return(-sim_ll)
}
## Maximize the log-likelihood function
model_1a <- optim(par = c(-0.02, -0.005, -0.8, 0.002, 5), fn = sim_ll_fn, 
                  draws_list = draws_camper, data_list = data_camper, 
                  method = 'BFGS', hessian = TRUE)
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
## Summarize model results
summarize_mle(model_1a, 
              c('cost', 'time', 'mountain', 'sd.time', 'sd.mountain'))

### Part b
## Function to simulate elasticities for one individual
sim_elas_ind <- function(params, draws_ind, data_ind){
  ## Select relevant variables and convert into a matrix
  data_matrix <- data_ind %>% 
    select(cost, time, mountain) %>% 
    as.matrix()
  ## Transform random draws into coefficients based on parameters
  coef_matrix <- draws_ind %>% 
    mutate(cost_coef = params[1],
           time_coef = params[2] + params[4] * time_draw,
           mountain_coef = params[3] + params[5] * mountain_draw) %>% 
    select(cost_coef, time_coef, mountain_coef) %>% 
    as.matrix()
  ## Calculate representative utility for each alternative in each draw
  utility <- (coef_matrix %*% t(data_matrix)) %>% 
    pmin(700) %>% 
    pmax(-700)
  ## Sum the exponential of utility over alternatives
  prob_denom <- utility %>% 
    exp() %>% 
    rowSums()
  ## Calculate the conditional probability for each alternative in each draw
  cond_prob <- exp(utility) / prob_denom
  ## Calculate simulated choice probabilities as the means over all draws
  sim_prob <- colMeans(cond_prob)
  ## Calculate simulated integral for own elasticity
  sim_int_own_elas <- params[1] * 
    mean(cond_prob[, 1] * (1 - cond_prob[, 1]))
  ## Calculate simulated integral for cross elasticities
  sim_int_cross_elas <- params[1] * 
    colMeans(cond_prob[, 1] * cond_prob[, -1])
  ## Combine elasticity simulated integrals into one vector
  sim_int_elas <- c(sim_int_own_elas, sim_int_cross_elas)
  ## Calculate own-price and cross-price simulated elasticities
  sim_elas <- c(1, rep(-1, 4)) * data_ind$cost[1] / sim_prob * sim_int_elas
  ## Add simulated elasticities to initial dataset
  data_ind_out <- data_ind %>% 
    mutate(elasticity = sim_elas)
  ## Return initial dataset with simulated elasticity variable
  return(data_ind_out)
}
## Simulate elasticities for each individual
data_1b_ind <- map2(.x = draws_camper, .y = data_camper,
                    .f = ~ sim_elas_ind(params = model_1a$par, 
                                        draws_ind = .x, 
                                        data_ind = .y))
## Combine list of data into one tibble
data_1b <- data_1b_ind %>% 
  bind_rows()
## Calculate average elasticity with respect to cost of alternative 1
data_1b %>% 
  group_by(park_id, park) %>% 
  summarize(elasticity = mean(elasticity), .groups = 'drop') %>% 
  arrange(park_id)


### Problem 2 solutions --------------------------------------------------------

### Part a
## Function to simulate individual coefficients for one individual
calc_mean_coefs <- function(params, draws_ind, data_ind){
  ## Select relevant variables and convert into a matrix
  data_matrix <- data_ind %>% 
    select(cost, time, mountain) %>% 
    as.matrix()
  ## Transform random draws into coefficients based on parameters
  coef <- draws_ind %>% 
    mutate(cost_coef = params[1],
           time_coef = params[2] + params[4] * time_draw,
           mountain_coef = params[3] + params[5] * mountain_draw) %>% 
    select(cost_coef, time_coef, mountain_coef)
  ## Convert coefficients tibble to a matrix
  coef_matrix <- as.matrix(coef)
  ## Calculate representative utility for each alternative in each draw
  utility <- (coef_matrix %*% t(data_matrix)) %>% 
    pmin(700) %>% 
    pmax(-700)
  ## Sum the exponential of utility over alternatives
  prob_denom <- utility %>% 
    exp() %>% 
    rowSums()
  ## Calculate the conditional probability for each alternative in each draw
  cond_prob <- exp(utility) / prob_denom
  ## Calculate the numerator of the draw weights as probability of chosen alt
  weights_num <- c(cond_prob %*% data_ind$visit)
  ## Calculate the draw weights
  weights <- weights_num / sum(weights_num)
  ## Add draw weights to dataset of coefficients
  coef <- coef %>%
    mutate(weight = weights)
  ## Calculate weighted mean for each coefficient
  coef_means <- coef %>%
    summarize(cost_coef = sum(cost_coef * weight),
              time_coef_mean = sum(time_coef * weight),
              mountain_coef_mean = sum(mountain_coef * weight))
  ## Add individual coefficient means to initial dataset
  data_ind_out <- data_ind %>% 
    bind_cols(coef_means)
  ## Return initial dataset with simulated probability variable
  return(data_ind_out)
}
## Calculate mean coefficients for each individual
data_2a_ind <- map2(.x = draws_camper, .y = data_camper,
                    .f = ~ calc_mean_coefs(params = model_1a$par, 
                                           draws_ind = .x, 
                                           data_ind = .y))
## Combine list of data into one tibble
data_2a <- data_2a_ind %>% 
  bind_rows()
## Calculate average coefficients for each camping park
coef_park_2a <- data_2a %>% 
  filter(visit == 1) %>% 
  group_by(park_id, park) %>% 
  summarize(cost_coef = mean(cost_coef), 
            time_coef_mean = mean(time_coef_mean), 
            mountain_coef_mean = mean(mountain_coef_mean),
            .groups = 'drop')
coef_park_2a
## Calculate mean values for each camping park
coef_park_2a %>% 
  mutate(time_value = time_coef_mean / cost_coef * 60,
         mountain_value = mountain_coef_mean / -cost_coef) %>% 
  select(park_id, park, time_value, mountain_value)

### Part a canned solutions ----------------------------------------------------
## Load mlogit package
library(mlogit)
## Convert dataset to dfidx format
data_dfidx <- dfidx(data = data_camping, shape = 'long', 
                    choice = 'visit', idx = c('camper_id', 'park_id'))
## Model camping park visit as a mixed logit with fixed cost coefficient
model_2a_alt <- mlogit(formula = visit ~ cost + time + mountain | 0 | 0, 
                       data = data_dfidx, 
                       rpar = c(time = 'n', mountain = 'n'), 
                       R = 100, seed = 703)
## Calculate mean coefficients for each individual
model_2a_alt_params_ind <- fitted(model_2a_alt, type = 'parameters')
## Add mean coefficients to dataset
data_2a_alt <- data_camping %>% 
  filter(visit == 1) %>% 
  mutate(cost_coef = coef(model_2a_alt)[1],
         time_coef_mean = model_2a_alt_params_ind[, 1],
         mountain_coef_mean = model_2a_alt_params_ind[, 2])
## Calculate average coefficients for each camping park
coef_park_2a_alt <- data_2a_alt %>%
  group_by(park_id, park) %>% 
  summarize(cost_coef = mean(cost_coef), 
            time_coef_mean = mean(time_coef_mean), 
            mountain_coef_mean = mean(mountain_coef_mean),
            .groups = 'drop')
coef_park_2a_alt
## Calculate mean values for each camping park
coef_park_2a_alt %>% 
  mutate(time_value = time_coef_mean / cost_coef * 60,
         mountain_value = mountain_coef_mean / -cost_coef) %>% 
  select(park_id, park, time_value, mountain_value)
