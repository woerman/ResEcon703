### ResEcon 703: Topics in Advanced Econometrics
### Problem set 2 solution code
### Matt Woerman, UMass Amherst

### Load packages for problem set
library(tidyverse)

### Problem 1 solutions --------------------------------------------------------

### Create functions to calculate log likelihood, summarize MLE models, and
### conduct likelihood ratio tests
## Function to calculate logit log-likelihood
calculate_log_likelihood <- function(parameters, data_x, data_y){
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
## Function to conduct likelihood ratio test
test_likelihood_ratio <- function(model_restricted, model_unrestricted){
  ## Calculate likelihood ratio test statistic
  test_statistic <- 2 * (model_restricted$value - model_unrestricted$value)
  ## Calculate the number of restrictions
  df <- length(model_unrestricted$par) - length(model_restricted$par)
  ## Test if likelihood ratio test statitsic is greater than critical value
  test <- test_statistic > qchisq(0.95, df)
  ## Calculate p-value of test
  p_value <- 1 - pchisq(test_statistic, df)
  ## Return test result and p-value
  return(list(reject = test, p_value = p_value))
}

### Part a
## Load dataset
data_multinomial <- read_csv('travel_multinomial.csv')
## Gather wide dataset into a long dataset
data_multinomial_long <- data_multinomial %>% 
  mutate(id = 1:n()) %>% 
  gather(key, value, starts_with('cost_'), starts_with('time_')) %>% 
  separate(key, c('key', 'alt')) %>% 
  spread(key, value) %>% 
  mutate(choice = (mode == alt)) %>% 
  select(-mode)
## Split heating dataset into list of household data frames
data_multinomial_split <- data_multinomial_long %>% 
  group_by(id) %>% 
  arrange(id, alt) %>% 
  group_split()
## Create matrices for choice outcomes
data_choice_1 <- data_multinomial_split %>% 
  map(~ .x %>% 
        select(choice) %>% 
        mutate(choice = 1 * choice) %>% 
        pull(choice))
## Create matrices of explanatory variables
data_explanatory_1a <- data_multinomial_split %>% 
  map(~.x %>% 
        select(cost, time) %>% 
        as.matrix())
## Maximize the log-likelihood function
model_1a <- optim(rep(0, 2), calculate_log_likelihood, 
                  data_x = data_explanatory_1a, data_y = data_choice_1,
                  method = 'BFGS', hessian = TRUE)
## Summarize model results
model_1a %>% 
  summarize_mle(c('cost', 'time'))

### Part b
## Create matrices of explanatory variables
data_explanatory_1b <- data_multinomial_split %>% 
  map(~ rep(0, 3) %>% 
        rbind(diag(3)) %>% 
        cbind(.x %>% 
                select(cost, time)) %>% 
        as.matrix())
## Maximize the log-likelihood function
model_1b <- optim(rep(0, 5), calculate_log_likelihood, 
                  data_x = data_explanatory_1b, data_y = data_choice_1,
                  method = 'BFGS', hessian = TRUE)
## Summarize model results
model_1b %>% 
  summarize_mle(c('bus', 'car', 'walk', 'cost', 'time'))

### Part c
## Conduct likelihood ratio test of models 1a and 1b
test_1c <- test_likelihood_ratio(model_1a, model_1b)
## Display test results
test_1c

### Part d
## Create matrices of explanatory variables
data_explanatory_1d <- data_multinomial_split %>% 
  map(~ rep(0, 3) %>% 
        rbind(diag(3)) %>% 
        cbind(.x$cost) %>% 
        cbind(diag(.x$time)) %>% 
        as.matrix())
## Maximize the log-likelihood function
model_1d <- optim(rep(0, 8), calculate_log_likelihood, 
                  data_x = data_explanatory_1d, data_y = data_choice_1,
                  method = 'BFGS', hessian = TRUE)
## Summarize model results
model_1d %>% 
  summarize_mle(c('bus', 'car', 'walk', 'cost', 
                  'time_bike', 'time_bus', 'time_car', 'time_walk'))

### Part e
## Conduct likelihood ratio test of models 1a and 1b
test_1e <- test_likelihood_ratio(model_1b, model_1d)
## Display test results
test_1e

### Problem 2 solutions --------------------------------------------------------

### Create function to calculate sum of squares
## Function to calculate binary logit sum of squares
calculate_sum_of_squares <- function(parameters, data_x, data_y){
  ## Calculate net utility of alternative given the parameters
  utility <- data_x %*% parameters
  ## Caclculate logit probability of alternative given the parameters
  probability_choice <- 1 / (1 + exp(-utility))
  ## Calculate sum of squares
  sum_of_squares <- sum((data_y - probability_choice)^2)
  return(sum_of_squares)
}

### Part a
## Load dataset
data_binary <- read_csv('travel_binary.csv')
## Create vector for choice outcomes
data_choice_2 <- data_binary %>% 
  mutate(choice = 1 * (mode == 'car')) %>% 
  pull(choice)
## Create matrix of explanatory variables
data_explanatory_2a <- data_binary %>% 
  mutate(cost_difference = cost_car - cost_bus,
         time_difference = time_car - time_bus) %>% 
  select(cost_difference, time_difference) %>% 
  as.matrix()
## Minimize the sum of squares
model_2a <- optim(rep(0, 2), calculate_sum_of_squares, 
                  data_x = data_explanatory_2a, data_y = data_choice_2,
                  method = 'BFGS')
## Show parameter estimates
list(names = c('cost', 'time'),
     parameters = model_2a$par)

### Part b
## Create matrix of explanatory variables
data_explanatory_2b <- data_binary %>% 
  mutate(constant = 1,
         cost_bus = -cost_bus,
         time_bus = -time_bus) %>% 
  select(constant, cost_car, time_car, cost_bus, time_bus) %>% 
  as.matrix()
## Minimize the sum of squares
model_2b <- optim(rep(0, 5), calculate_sum_of_squares, 
                  data_x = data_explanatory_2b, data_y = data_choice_2,
                  method = 'BFGS')
## Show parameter estimates
list(names = c('car', 'cost_car', 'time_car', 'cost_bus', 'time_bus'),
     parameters = model_2b$par)

### Part c
## Create the variance-covariance matrix from the given standard erros
variance_covariance_2c <- c(1.61, 0.86, 0.09, 0.31, 0.03) %>% 
  diag()
## Calculate the restriction vector
restriction_vector_2c <- model_2b$par[2] - model_2b$par[4]
## Construct the restriction Jacobian
restriction_jacobian_2c <- c(0, 1, 0, -1, 0) %>% 
  t()
## Calculate the Wald test statistic
wald_test_stat_2c <- t(restriction_vector_2c) %*% 
  solve(restriction_jacobian_2c %*% 
          variance_covariance_2c %*% 
          t(restriction_jacobian_2c)) %*% 
  restriction_vector_2c %>% 
  c()
## Test if Wald test statitsic is greater than critical value
reject_2c <- wald_test_stat_2c > qchisq(0.95, 1)
## Calculate p-value of Wald test
p_value_2c <- 1 - pchisq(wald_test_stat_2c, 1)
## Report Wald test results
list(reject = reject_2c,
     p_value = p_value_2c)

### Part d
## Calculate net utility of driving at NLS parameters
utility_2d <- data_explanatory_2a %*% model_2a$par
## Caclculate logit probability of driving at NLS parameters
probability_2d <- 1 / (1 + exp(-utility_2d))
## Create list of individual derivative vectors
derivative_vector_list_2d <- data_explanatory_2a %>% 
  split(row(.)) %>% 
  map2(as.list(probability_2d), ~ .x * .y * (1 - .y))
## Create list of individual derivative product matrices
derivative_matrix_list_2d <- derivative_vector_list_2d %>% 
  map(~ .x %*% t(.x))
## Sum individual derivative product matrices
derivative_matrix_2d <- derivative_matrix_list_2d %>% 
  reduce(`+`)
## Calculate error variance at NLS parameters
error_variance_2d <- calculate_sum_of_squares(model_2a$par, 
                                              data_explanatory_2a, 
                                              data_choice_2) /
  length(data_choice_2)
## Calculate variance-covariance matrix
variance_covariance_2d <- error_variance_2d * solve(derivative_matrix_2d)
## Calculate parameter standard erros
standard_errors_2d <- variance_covariance_2d %>% 
  diag() %>% 
  sqrt()
## Calculate parameter t-stats
t_stats_2d <- model_2a$par / standard_errors_2d
## Calculate parameter p-values
p_values_2d <- 2 * pt(-abs(t_stats_2d), 
                      length(data_choice_2) - length(model_2a$par))
## Report summary of model results
list(names = c('cost', 'time'),
     parameters = model_2a$par,
     std_errors = standard_errors_2d,
     t_stats = t_stats_2d,
     p_values = p_values_2d)

### Part e
## Calculate net utility of driving at NLS parameters
utility_2e <- data_explanatory_2b %*% model_2b$par
## Caclculate logit probability of driving at NLS parameters
probability_2e <- 1 / (1 + exp(-utility_2e))
## Create list of individual derivative vectors
derivative_vector_list_2e <- data_explanatory_2b %>% 
  split(row(.)) %>% 
  map2(as.list(probability_2e), ~ .x * .y * (1 - .y) * c(1, 1, 1, -1, -1))
## Create list of individual derivative product matrices
derivative_matrix_list_2e <- derivative_vector_list_2e %>% 
  map(~ .x %*% t(.x))
## Sum individual derivative product matrices
derivative_matrix_2e <- derivative_matrix_list_2e %>% 
  reduce(`+`)
## Calculate error variance at NLS parameters
error_variance_2e <- calculate_sum_of_squares(model_2b$par, 
                                              data_explanatory_2b, 
                                              data_choice_2) /
  length(data_choice_2)
## Calculate variance-covariance matrix
variance_covariance_2e <- error_variance_2e * solve(derivative_matrix_2e)
## Calculate parameter standard erros
standard_errors_2e <- variance_covariance_2e %>% 
  diag() %>% 
  sqrt()
## Calculate parameter t-stats
t_stats_2e <- model_2b$par / standard_errors_2e
## Calculate parameter p-values
p_values_2e <- 2 * pt(-abs(t_stats_2e), 
                      length(data_choice_2) - length(model_2b$par))
## Report summary of model results
list(names = c('car', 'cost_car', 'time_car', 'cost_bus', 'time_bus'),
     parameters = model_2b$par,
     std_errors = standard_errors_2e,
     t_stats = t_stats_2e,
     p_values = p_values_2e)
## Use the restriction vector from 2c
restriction_vector_2e <- restriction_vector_2c
## Use the restriction Jacobian from 2c
restriction_jacobian_2e <- restriction_jacobian_2c
## Calculate the Wald test statistic
wald_test_stat_2e <- t(restriction_vector_2e) %*% 
  solve(restriction_jacobian_2e %*% 
          variance_covariance_2e %*% 
          t(restriction_jacobian_2e)) %*% 
  restriction_vector_2e %>% 
  c()
## Test if Wald test statitsic is greater than critical value
reject_2e <- wald_test_stat_2e > qchisq(0.95, 1)
## Calculate p-value of Wald test
p_value_2e <- 1 - pchisq(wald_test_stat_2e, 1)
## Report Wald test results
list(reject = reject_2e,
     p_value = p_value_2e)
