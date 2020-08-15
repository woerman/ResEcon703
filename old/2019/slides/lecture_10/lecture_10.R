### ResEcon 703: Topics in Advanced Econometrics
### Lecture 10: Nonlinear Least Squares II
### Matt Woerman, UMass Amherst

### Nonlinear Least Squares Example in R ---------------------------------------

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

### Calculate NLS estimator for multinomial logit heating choice using cost data 
### and alternative effects
## Function to calculate sum of squares using two variables and four 
## alternative effects
least_squares_long <- function(parameters, data){
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
  ## Calculate logit choice probability given the parameters
  data <- data %>%
    mutate(probability = exp(utility) / probability_denominator)
  ## Calculate regression residual given the parameters
  data <- data %>%
    mutate(residual = choice - probability)
  ## Calculate the sum of squares given the parameters
  sum_squares <- sum(data$residual^2)
  return(sum_squares)
}
## Gather heating dataset into a long dataset
heating_long <- Heating %>% 
  gather(key, value, starts_with('ic.'), starts_with('oc.')) %>% 
  separate(key, c('cost', 'alt')) %>% 
  spread(cost, value) %>% 
  mutate(choice = (depvar == alt)) %>% 
  select(-depvar) %>% 
  mutate(var_1 = ic, var_2 = oc)
## Minimize the sum of squared errors
model_nls_long <- optim(rep(0, 6), least_squares_long, 
                        data = heating_long, method = 'BFGS')
## Show NLS estimation results
model_nls_long

### Estimate the variance of the previous NLS model
## Assign constant parameters to alternatives
variance_data <- heating_long %>% 
  group_by(idcase) %>% 
  arrange(idcase, alt) %>% 
  mutate(constant = c(model_nls_long$par[1:4], 0)) %>% 
  ungroup()
## Calculate utility for each alternative at the NLS parameters
variance_data <- variance_data %>% 
  mutate(utility = constant + 
           model_nls_long$par[5] * var_1 + model_nls_long$par[6] * var_2)
## Calculate logit probability denominator at the NLS parameters
variance_data <- variance_data %>% 
  group_by(idcase) %>% 
  mutate(probability_denominator = sum(exp(utility))) %>% 
  ungroup()
## Calculate logit choice probability at the NLS parameters
variance_data <- variance_data %>%
  mutate(probability = exp(utility) / probability_denominator)
## Create vectors of individual probabilities
variance_data <- variance_data %>% 
  select(idcase, alt, probability) %>% 
  spread(alt, probability) %>% 
  mutate(probability_all = pmap(list(.$ec, .$er, .$gc, .$er), 
                                ~ c(..1, ..2, ..3, ..4))) %>% 
  select(idcase, probability_all) %>% 
  full_join(variance_data, by = 'idcase')
## Calculate the probability-weighted average of each cost for each individual
variance_data <- variance_data %>% 
  group_by(idcase) %>% 
  mutate(ic_weighted = sum(probability * ic),
         oc_weighted = sum(probability * oc)) %>% 
  ungroup()
## Calculate the gradient for each alternative and individual
variance_data <- variance_data %>% 
  group_by(idcase) %>% 
  arrange(idcase, alt) %>% 
  mutate(alt_order = 1:n()) %>% 
  ungroup() %>% 
  mutate(constant_vector = map(.$alt_order, ~ c(rep(0, .x - 1), 
                                                1, 
                                                rep(0, 5 - .x))[1:4])) %>% 
  mutate(gradient_alpha = pmap(list(.$probability, 
                                    .$probability_all, 
                                    .$constant_vector), 
                               ~ ..1 * (..3 - ..2))) %>% 
  mutate(gradient = pmap(list(.$gradient_alpha, .$probability, 
                              .$ic, .$ic_weighted, .$oc, .$oc_weighted),
                         ~ c(..1, 
                             ..2 * (..3 - ..4),
                             ..2 * (..5 - ..6))))
## Calculate the gradient product matrix for each alternative and individual
variance_data <- variance_data %>% 
  mutate(gradient_matrix = map(.$gradient, ~ .x %*% t(.x)))
## Sum the gradient product matrices over all alternatives and individuals
gradient_matrix_sum <- variance_data$gradient_matrix %>% reduce(`+`)
## Estimate the variance of the econometric errors
error_variance <- least_squares_long(model_nls_long$par, heating_long) /
  nrow(heating_long)
## Calculate the variance-covariance matrix
variance_covariance <- error_variance * solve(gradient_matrix_sum)
variance_covariance
## Report estimated parameters and standard errors
model_nls_long$par
variance_covariance %>% 
  diag() %>% 
  sqrt()

### Conduct a Wald test that alternative-specific intercepts equal zero
## Calculate the restriction vector
restriction_vector <- model_nls_long$par[1:4]
## Construct the restriction Jacobian
restriction_jacobian <- diag(4) %>% 
  cbind(rep(0, 4)) %>% 
  cbind(rep(0, 4))
## Calculate the Wald test statistic
wald_test_stat <- t(restriction_vector) %*% 
  solve(restriction_jacobian %*% 
          variance_covariance %*% 
          t(restriction_jacobian)) %*% 
  restriction_vector %>% 
  c()
## Test if Wald test statitsic is greater than critical value
wald_test_stat > qchisq(0.95, 4)
## Calculate p-value of Wald test
1 - pchisq(wald_test_stat, 4)

### Calculate NLS estimator for multinomial logit heating choice using cost 
### data, alternative effects, and alternative-specific coefficients on rooms
## Function to calculate sum of squares using flexible matrices
least_squares_matrix <- function(parameters, data){
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
    map(~ sum(exp(.x)))
  ## Calculate logit probability numerator given the parameters
  probability_numerator <- utility %>% 
    map(~ exp(.x))
  ## Calculate logit choice probability given the parameters
  probability_choice <- probability_numerator %>% 
    map2(probability_denominator, ~ .x / .y)
  ## Calculate square residual given the parameters
  residuals <- data_y %>% 
    map2(probability_choice, ~ .x - .y)
  ## Calculate the sum of squares given the parameters
  sum_squares <- residuals %>%
    map(~ sum(.x^2)) %>% 
    unlist() %>% 
    sum()
  return(sum_squares)
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
## Minimize the sum of square errors
model_matrix <- optim(rep(0, 10), least_squares_matrix, 
                      data = heating_matrix, method = 'BFGS')
## Show NLS parameters
model_matrix$par
