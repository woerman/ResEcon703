########################################################
##### ResEcon 703: Topics in Advanced Econometrics #####
#####    Week 12: Individual-Level Coefficients    #####
#####         Matt Woerman, UMass Amherst          #####
########################################################

### Individual-Level Coefficients R Example ------------------------------------

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
## Convert cleaned dataset to dfidx format
hvac_dfidx <- dfidx(hvac_clean, shape = 'long', 
                    choice = 'choice', idx = c('id', 'alt'))
## Look at data in dfidx format
tibble(hvac_dfidx)

### Model HVAC choice as a mixed logit using mlogit
## Model choice using ac, ic, and oc with normal random coefficients
model_1 <- mlogit(formula = choice ~ ac + ic + oc | 0 | 0, 
                  data = hvac_dfidx, 
                  reflevel = 'hpc',
                  rpar = c(ac = 'n', ic = 'n', oc = 'n'), 
                  R = 1000, seed = 703)
## Summarize model results
summary(model_1)

### Find the mean coefficient for each chosen alternative using mlogit
## Calculate mean coefficient for each household
coefs_1 <- model_1 %>% 
  fitted(type = 'parameters') %>% 
  as_tibble() %>% 
  rename(ac_coef = ac, ic_coef = ic, oc_coef = oc)
coefs_1
## Average coefficient over all households with each HVAC system
hvac_clean %>% 
  filter(choice == 1) %>% 
  cbind(coefs_1) %>% 
  group_by(alt) %>% 
  summarize(ac_coef = mean(ac_coef), 
            ic_coef = mean(ic_coef), 
            oc_coef = mean(oc_coef),
            .groups = 'drop')

### Model HVAC choice as a mixed logit coded by hand
## Set seed for replication
set.seed(703)
## Draw standard normal random variables for each household
draws_hh <- map(1:250, ~ tibble(ac_draw = rnorm(100),
                                ic_draw = rnorm(100),
                                oc_draw = rnorm(100)))
## Split data into list by household
data_hh <- hvac_clean %>% 
  group_by(id) %>% 
  group_split()
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
model_2 <- optim(par = c(6.53, -0.17, -1.04, 0, 0, 0), fn = sim_ll_fn,
                 draws_list = draws_hh, data_list = data_hh, 
                 method = 'BFGS', hessian = TRUE,
                 control = list(trace = 1, REPORT = 5))
## Report model results
model_2

### Find the mean coefficient for each chosen alternative coded by hand
## Function to simulate individual coefficients for one individual
calc_mean_coefs <- function(params, draws_ind, data_ind){
  ## Select relevant variables and convert into a matrix [J x K]
  data_matrix <- data_ind %>% 
    select(ac, ic, oc) %>% 
    as.matrix()
  ## Transform random draws into coefficients based on parameters
  coef <- draws_ind %>% 
    mutate(ac_coef = params[1] + params[4] * ac_draw,
           ic_coef = params[2] + params[5] * ic_draw,
           oc_coef = params[3] + params[6] * oc_draw) %>% 
    select(ac_coef, ic_coef, oc_coef)
  ## Convert coefficients tibble to a matrix [R x K]
  coef_matrix <- as.matrix(coef)
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
  ## Calculate the numerator of the draw weights as prob of chosen alt [R x 1]
  weights_num <- c(cond_prob %*% data_ind$choice)
  ## Calculate the draw weights [R x 1]
  weights <- weights_num / sum(weights_num)
  ## Add draw weights to dataset of coefficients
  coef <- coef %>%
    mutate(weight = weights)
  ## Calculate weighted mean for each coefficient
  coef_means <- coef %>%
    summarize(ac_coef_mean = sum(ac_coef * weight),
              ic_coef_mean = sum(ic_coef * weight),
              oc_coef_mean = sum(oc_coef * weight))
  ## Add individual coefficient means to initial dataset
  data_ind_out <- data_ind %>% 
    bind_cols(coef_means)
  ## Return initial dataset with simulated probability variable
  return(data_ind_out)
}
## Calculate mean coefficients for each individual
data_2_ind <- map2(.x = draws_hh, .y = data_hh,
                   .f = ~ calc_mean_coefs(params = model_2$par, 
                                          draws_ind = .x, 
                                          data_ind = .y))
## Combine list of data into one tibble
data_2 <- data_2_ind %>% 
  bind_rows()
## Calculate mean coefficients by chosen alternative
data_2 %>%
  filter(choice == 1) %>% 
  group_by(alt) %>% 
  summarize(ac_coef = mean(ac_coef_mean),
            ic_coef = mean(ic_coef_mean),
            oc_coef = mean(oc_coef_mean),
            .groups = 'drop')
