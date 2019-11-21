### ResEcon 703: Topics in Advanced Econometrics
### Problem set 4 solution code
### Matt Woerman, UMass Amherst

### Load packages for problem set
library(tidyverse)
library(mlogit)

### Problem 1 solutions --------------------------------------------------------

### Part a
## Load dataset
phones <- read_csv('phones.csv')
## Create dummy variable for Apple
data_1 <- phones %>% 
  mutate(apple = 1 * (phone_id %in% 5:10))
## Convert dataset to mlogit format
data_1a <- data_1 %>% 
  mlogit.data(shape = 'long', choice = 'purchase', alt.var = 'phone_id')
## Model phone purchase as a mixed logit with normal coefficients
model_1a <- data_1a %>% 
  mlogit(purchase ~ apple + storage + screen + price | 0 | 0, data = .,
         rpar = c(apple = 'n', storage = 'n', screen = 'n', price = 'n'), 
         R = 100, seed = 703)
## Summarize model results
model_1a %>% 
  summary()
## Calculate fraction of population with negative coefficients
pnorm(0, model_1a$coefficients[1:4], abs(model_1a$coefficients[5:8]))
## Calculate fraction of population with positive coefficients
1 - pnorm(0, model_1a$coefficients[1:4], abs(model_1a$coefficients[5:8]))

### Part b
## Convert dataset to mlogit format with negative prices
data_1b <- data_1 %>% 
  mlogit.data(shape = 'long', choice = 'purchase', alt.var = 'phone_id',
              opposite = 'price')
## Model phone purchase as a mixed logit with random coefficients
model_1b <- data_1b %>% 
  mlogit(purchase ~ apple + storage + screen + price | 0 | 0, data = .,
         rpar = c(apple = 'n', storage = 'ln', screen = 'n', price = 'ln'), 
         R = 100, seed = 703)
## Summarize model results
model_1b %>% 
  summary()
## Calculate fraction of population with negative coefficients
c(pnorm(0, model_1b$coefficients[1], abs(model_1b$coefficients[5])),
  plnorm(0, model_1b$coefficients[2], abs(model_1b$coefficients[6])),
  pnorm(0, model_1b$coefficients[3], abs(model_1b$coefficients[7])),
  plnorm(0, model_1b$coefficients[4], abs(model_1b$coefficients[8]))) %>% 
  setNames(c('apple', 'storage', 'screen', 'price'))
## Calculate fraction of population with positive coefficients
1 - c(pnorm(0, model_1b$coefficients[1], abs(model_1b$coefficients[5])),
      plnorm(0, model_1b$coefficients[2], abs(model_1b$coefficients[6])),
      pnorm(0, model_1b$coefficients[3], abs(model_1b$coefficients[7])),
      plnorm(0, model_1b$coefficients[4], abs(model_1b$coefficients[8]))) %>% 
  setNames(c('apple', 'storage', 'screen', 'price'))

### Part c
## Convert dataset to mlogit format
data_1c <- data_1 %>% 
  mlogit.data(shape = 'long', choice = 'purchase', alt.var = 'phone_id')
## Model phone purchase as a mixed logit with random coefficients but
## fixed price coefficient
model_1c <- data_1c %>% 
  mlogit(purchase ~ apple + storage + screen + price | 0 | 0, data = .,
         rpar = c(apple = 'n', storage = 'ln', screen = 'n'), 
         R = 100, seed = 703)
## Summarize model results
model_1c %>% 
  summary()
## Calculate distirbuiton of the value of brand preference
c(model_1c$coefficients[1] / -model_1c$coefficients[4],
  abs(model_1c$coefficients[5]) / -model_1c$coefficients[4]) %>% 
  setNames(c('apple', 'sd.apple'))
## Calculate distirbuiton of the value of storage
c(model_1c$coefficients[2] - log(-model_1c$coefficients[4]),
  abs(model_1c$coefficients[6])) %>% 
  setNames(c('storage', 'sd.storage'))
## Calculate distirbuiton of the value of screen size
c(model_1c$coefficients[3] / -model_1c$coefficients[4] * 0.1,
  abs(model_1c$coefficients[7]) / -model_1c$coefficients[4] * 0.01) %>% 
  setNames(c('screen', 'sd.screen'))

### Part d
## Conduct likelihood ratio test of the models in parts b and c
lrtest(model_1c, model_1b)

### Part e
## Calculate individual-level coefficients
coefficients_1e <- model_1c %>% 
  fitted(type = 'parameters')
## Calculate average coefficients for Google phone consumers
data_1 %>% 
  filter(purchase == 1) %>% 
  select(customer_id, phone, storage) %>% 
  rename(storage_data = storage) %>% 
  cbind(coefficients_1e) %>% 
  group_by(phone, storage_data) %>% 
  summarize(apple = mean(apple) / -model_1c$coefficients[4], 
            storage = mean(storage) / -model_1c$coefficients[4], 
            screen = mean(screen) / -model_1c$coefficients[4] * 0.1) %>% 
  ungroup() %>% 
  slice(1:4) %>% 
  rename(apple_coef = apple,
         storage_coef = storage,
         screen_coef = screen,
         storage = storage_data)

### Problem 2 solutions --------------------------------------------------------

### Part a
## Set seed for replication
set.seed(703)
## Draw standard normal random variables and split into list
draws_2_list <- 1:1000 %>% 
  map(., ~ tibble(apple_coef = rnorm(100),
                  storage_coef = rnorm(100),
                  screen_coef = rnorm(100)))
## Split data into list by customer
data_2_list <- data_1 %>% 
  group_by(customer_id) %>% 
  group_split()
## Function to simulate choice probabilities for one individual
simulate_probabilities <- function(parameters, draws, data){
  ## Select relevant variables and convert into a matrix
  data_matrix <- data %>% 
    select(apple, storage, screen, price) %>% 
    as.matrix()
  ## Transform random coefficients based on parameters
  coefficients <- draws %>% 
    mutate(apple_coef = parameters[1] + parameters[5] * apple_coef,
           storage_coef = exp(parameters[2] + parameters[6] * storage_coef),
           screen_coef = parameters[3] + parameters[7] * screen_coef,
           price_coef = parameters[4])
  ## Calculate utility for each alternative in each draw
  utility <- (as.matrix(coefficients) %*% t(data_matrix)) %>% 
    pmin(700) %>% 
    pmax(-700)
  ## Sum the exponential of utility over alternatives
  summed_utility <- utility %>% 
    exp() %>% 
    rowSums()
  ## Calculate the conditional probability for each alternative in each draw
  conditional_probability <- exp(utility) / summed_utility
  ## Average conditional probabilities over all draws
  simulated_probability <- colMeans(conditional_probability)
  ## Add simulated probability to initial dataset
  data_out <- data %>% 
    mutate(probability = simulated_probability)
  ## Return initial dataset with simulated probability variable
  return(data_out)
}
## Function to calculate simulated log-likelihood
simulate_log_likelihood <- function(parameters, draws_list, data_list){
  ## Simulate probabilities for each individual
  data <- map2(.x = draws_list, .y = data_list,
               .f = ~ simulate_probabilities(parameters = parameters, 
                                             draws = .x, 
                                             data = .y))
  ## Combine individual datasets into one
  data <- data %>% 
    bind_rows()
  ## Calcule the log of simulated probability for the chosen alternative
  data <- data %>% 
    filter(purchase == 1) %>% 
    mutate(log_probability = log(probability))
  ## Calculate the simulated log-likelihood
  simulated_log_likelihood <- sum(data$log_probability)
  ## Return the negative of simulated log-likelihood
  return(-simulated_log_likelihood)
}
## Maximize the log-likelihood function
model_2a <- optim(par = c(model_1c$coefficients), fn = simulate_log_likelihood, 
                  draws_list = draws_2_list, data_list = data_2_list, 
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
  model_summary <- list(names = names,
                        parameters = parameters,
                        std_errors = std_errors,
                        z_stats = z_stats,
                        p_values = p_values)
  ## Return model_summary object
  return(model_summary)
}
summarize_mle(model_2a, names(model_2a$par))

### Part b
## Function to simulate individual coefficients for one individual
simulate_coefficients <- function(parameters, draws, data){
  ## Select relevant variables and convert into a matrix
  data_matrix <- data %>% 
    select(apple, storage, screen, price) %>% 
    as.matrix()
  ## Transform random coefficients based on parameters
  coefficients <- draws %>% 
    mutate(apple_coef = parameters[1] + parameters[5] * apple_coef,
           storage_coef = exp(parameters[2] + parameters[6] * storage_coef),
           screen_coef = parameters[3] + parameters[7] * screen_coef,
           price_coef = parameters[4]) %>% 
    select(apple_coef, storage_coef, screen_coef, price_coef)
  ## Calculate utility for each alternative in each draw
  utility <- (as.matrix(coefficients) %*% t(data_matrix)) %>% 
    pmin(700) %>% 
    pmax(-700)
  ## Sum the exponential of utility over alternatives
  summed_utility <- utility %>% 
    exp() %>% 
    rowSums()
  ## Calculate the conditional probability for each alternative in each draw
  conditional_probability <- exp(utility) / summed_utility
  ## Extract conditional probabilities of chosen alternative for each draw
  probability_draw <- conditional_probability %*% data$purchase
  ## Add draw probability to dataset of coefficients
  coefficients <- coefficients %>%
    mutate(probability = c(probability_draw))
  ## Calculate weighted average for each coefficient
  coefficients_weighted <- coefficients %>%
    summarize(apple_coef = sum(apple_coef * probability),
              storage_coef = sum(storage_coef * probability),
              screen_coef = sum(screen_coef * probability),
              probability = sum(probability)) %>% 
    mutate(apple_coef = apple_coef / probability,
           storage_coef = storage_coef / probability,
           screen_coef = screen_coef / probability) %>% 
    select(-probability)
  ## Add individual coefficients to initial dataset
  data_out <- data %>% 
    mutate(apple_coef = coefficients_weighted$apple_coef,
           storage_coef = coefficients_weighted$storage_coef,
           screen_coef = coefficients_weighted$screen_coef)
  ## Return initial dataset with simulated probability variable
  return(data_out)
}
## Calculate individual coefficients for each individual
data_2b_list <- map2(.x = draws_2_list, .y = data_2_list,
                     .f = ~ simulate_coefficients(parameters = model_2a$par, 
                                                  draws = .x, 
                                                  data = .y))
## Combine list of data into one tibble
data_2b <- data_2b_list %>% 
  bind_rows()
## Calculate average coefficients for Google phone consumers
data_2b %>% 
  filter(purchase == 1) %>% 
  group_by(phone, storage) %>% 
  summarize(apple_coef = mean(apple_coef) / -model_2a$par[4], 
            storage_coef = mean(storage_coef) / -model_2a$par[4], 
            screen_coef = mean(screen_coef) / -model_2a$par[4] * 0.1) %>% 
  ungroup() %>% 
  slice(1:4)

### Part c
## Function to simulate elasticities for one individual
simulate_elasticities <- function(parameters, draws, data){
  ## Select relevant variables and convert into a matrix
  data_matrix <- data %>% 
    select(apple, storage, screen, price) %>% 
    as.matrix()
  ## Transform random coefficients based on parameters
  coefficients <- draws %>% 
    mutate(apple_coef = parameters[1] + parameters[5] * apple_coef,
           storage_coef = exp(parameters[2] + parameters[6] * storage_coef),
           screen_coef = parameters[3] + parameters[7] * screen_coef,
           price_coef = parameters[4])
  ## Calculate utility for each alternative in each draw
  utility <- (as.matrix(coefficients) %*% t(data_matrix)) %>% 
    pmin(700) %>% 
    pmax(-700)
  ## Sum the exponential of utility over alternatives
  summed_utility <- utility %>% 
    exp() %>% 
    rowSums()
  ## Calculate the conditional probability for each alternative in each draw
  conditional_probability <- exp(utility) / summed_utility
  ## Calculate simulated choice probabilities
  simulated_probability <- colMeans(conditional_probability)
  ## Calculate mean product of conditional probability for own elasticity
  own_elasticity_integral <- mean(conditional_probability[, 6] * 
                                    (1 - conditional_probability[, 6])) *
    unname(model_2a$par[4])
  ## Calculate mean product of conditional probability for cross elasticities
  cross_elasticity_integral <- colMeans(conditional_probability[, 6] * 
                                          conditional_probability[, -6]) *
    model_2a$par[4]
  ## Combine elasticity integrals into one vector
  elasticity_integral <- c(cross_elasticity_integral[1:5],
                           own_elasticity_integral,
                           cross_elasticity_integral[6:9])
  ## Calculate own-price and cross-price simulated elasticities
  simulated_elasticity <- c(rep(-1, 5), 1, rep(-1, 4)) * data$price[6] / 
    simulated_probability * elasticity_integral
  ## Add simulated elasticities to initial dataset
  data_out <- data %>% 
    mutate(elasticity = simulated_elasticity)
  ## Return initial dataset with simulated probability variable
  return(data_out)
}
## Simulate elasticities for each individual
data_2c_list <- map2(.x = draws_2_list, .y = data_2_list,
                     .f = ~ simulate_elasticities(parameters = model_2a$par, 
                                                  draws = .x, 
                                                  data = .y))
## Combine list of data into one tibble
data_2c <- data_2c_list %>% 
  bind_rows()
## Calculate average elasticity with respect to price of iPhone 11 with 256 GB
data_2c %>% 
  group_by(phone, storage) %>% 
  summarize(elasticity = mean(elasticity)) %>% 
  ungroup()
