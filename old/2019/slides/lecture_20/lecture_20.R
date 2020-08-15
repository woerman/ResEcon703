### ResEcon 703: Topics in Advanced Econometrics
### Lecture 20: Individual-Specific Parameters II
### Matt Woerman, UMass Amherst

### Individual-Specific Parameters Example in R --------------------------------

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
  mutate(choice = 1 * (depvar == alt)) %>% 
  select(-depvar)
## Look at long dataset
as_tibble(hvac_long)
## Combine heating and cooling costs into one variable
hvac_clean <- hvac_long %>% 
  mutate(cooling = (nchar(alt) == 3), 
         ic = if_else(cooling, ich + icca, ich),
         oc = if_else(cooling, och + occa, och)) %>% 
  mutate(cooling = 1 * cooling) %>% 
  select(id, alt, choice, cooling, ic, oc, income) %>% 
  arrange(id, alt)
## Look at cleaned dataset
as_tibble(hvac_clean)
## Convert cleaned dataset to mlogit format
hvac_mlogit <- mlogit.data(hvac_clean, shape = 'long', 
                           choice = 'choice', alt.var = 'alt')
## Look at data in mlogit format
as_tibble(hvac_mlogit)

### Model HVAC choice as a mixed logit using mlogit
## Model choice using cooling dummy with log-normal distribution and
## fixed coefficients for installation cost and operating cost
model_1 <- hvac_mlogit %>% 
  mlogit(formula = choice ~ cooling + ic + oc | 0 | 0, data = .,
         rpar = c(cooling = 'ln'), R = 100, seed = 703)
## Summarize model results
model_1 %>% 
  summary()

### Find the mean coefficient for each chosen alternative using mlogit
## Calculate mean coefficient for each household
coefficients_1 <- model_1 %>% 
  fitted(type = 'parameters') %>% 
  as_tibble() %>% 
  rename(cooling_coef = cooling)
coefficients_1
## Average coefficient over all households with each HVAC system
hvac_clean %>% 
  filter(choice == 1) %>% 
  cbind(coefficients_1) %>% 
  group_by(alt) %>% 
  summarize(cooling_coef = mean(cooling_coef))

### Model HVAC choice as a mixed logit coded by hand
## Set seed for replication
set.seed(703)
## Draw standard normal random variables and split into list
draws_2_list <- 1:250 %>% 
  map(., ~ tibble(cooling_coef = rnorm(100)))
## Split data into list by household
data_2_list <- hvac_clean %>% 
  group_by(id) %>% 
  group_split()
## Function to simulate choice probabilities for one household
simulate_probabilities <- function(parameters, draws, data){
  ## Select relevant variables and convert into a matrix [J * K]
  data_matrix <- data %>% 
    select(cooling, ic, oc) %>% 
    as.matrix()
  ## Transform random coefficients based on parameters [R * K]
  coefficients <- draws %>% 
    mutate(cooling_coef = exp(parameters[1] + parameters[4] * cooling_coef),
           ic_coef = parameters[2],
           oc_coef = parameters[3])
  ## Calculate utility for each alternative in each draw [R * J]
  utility <- (as.matrix(coefficients) %*% t(data_matrix)) %>% 
    pmin(700) %>% 
    pmax(-700)
  ## Sum the exponential of utility over alternatives [R * 1]
  summed_utility <- utility %>% 
    exp() %>% 
    rowSums()
  ## Calculate the conditional probability for each alternative in each draw [R * J]
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
model_2 <- optim(par = c(log(6.53), -0.174, -1.04, 0), 
                 fn = simulate_log_likelihood, 
                 draws_list = draws_2_list, data_list = data_2_list, 
                 method = 'BFGS', hessian = TRUE,
                 control = list(trace = 1, REPORT = 5))
## Report parameter estimates and standard errors
model_2$par
model_2$hessian %>% 
  solve() %>% 
  diag() %>% 
  sqrt()

### Find the mean coefficient for each chosen alternative coded by hand
## Function to simulate mean coefficient for one household
simulate_coefs <- function(parameters, draws, data){
  ## Select relevant variables and convert into a matrix [J * K]
  data_matrix <- data %>% 
    select(cooling, ic, oc) %>% 
    as.matrix()
  ## Transform random coefficients based on parameters [R * K]
  coefficients <- draws %>% 
    mutate(cooling_coef = exp(parameters[1] + parameters[4] * cooling_coef),
           ic_coef = parameters[2],
           oc_coef = parameters[3])
  ## Calculate utility for each alternative in each draw [R * J]
  utility <- (as.matrix(coefficients) %*% t(data_matrix)) %>% 
    pmin(700) %>% 
    pmax(-700)
  ## Sum the exponential of utility over alternatives [R * 1]
  summed_utility <- utility %>% 
    exp() %>% 
    rowSums()
  ## Calculate the conditional probability for each alternative in each draw [R * J]
  conditional_probability <- exp(utility) / summed_utility
  ## Extract conditional probabilities of chosen alternative for each draw [R * 1]
  probability_draw <- conditional_probability %*% data$choice
  ## Add draw probability to dataset of coefficients
  coefficients <- coefficients %>%
    mutate(probability = c(probability_draw))
  ## Calculate weighted average for each coefficient
  coefficients_weighted <- coefficients %>%
    summarize(cooling_coef = sum(cooling_coef * probability),
              probability = sum(probability)) %>% 
    mutate(cooling_coef = cooling_coef / probability) %>% 
    select(-probability)
  ## Add individual coefficients to initial dataset
  data_out <- data %>% 
    mutate(cooling_coef = coefficients_weighted$cooling_coef)
  ## Return initial dataset with simulated probability variable
  return(data_out)
}
## Calculate individual coefficients for each household
data_2_list <- map2(.x = draws_2_list, .y = data_2_list,
                    .f = ~ simulate_coefs(parameters = model_2$par, 
                                          draws = .x, 
                                          data = .y))
## Combine list of data into one tibble
data_2 <- data_2_list %>% 
  bind_rows()
## Calculate average coefficients for Google phone consumers
data_2 %>%
  filter(choice == 1) %>% 
  group_by(alt) %>% 
  summarize(cooling_coef = mean(cooling_coef))
