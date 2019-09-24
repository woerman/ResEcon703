### ResEcon 703: Topics in Advanced Econometrics
### Problem set 1 solution code
### Matt Woerman, UMass Amherst

### Load packages for problem set
library(tidyverse)
library(lmtest)
library(sandwich)
library(car)
library(mlogit)

### Problem 1 solutions --------------------------------------------------------

### Part a
## Load dataset
data_binary <- read_csv('travel_binary.csv')
## Clean choice variable
data_binary <- data_binary %>% 
  mutate(car = (mode == 'car'))
## Model choice as a linear probability model
reg_lpm <- data_binary %>% 
  lm(formula = car ~ time_car + time_bus + cost_car + cost_bus, data = .)
## Calculate heteroskedastic-robust standard errors
reg_lpm %>% 
  coeftest(vcov = vcovHC(reg_lpm))

### Part b
## Conduct a Wald test on time coefficients
reg_lpm %>% 
  linearHypothesis('time_car = -time_bus', vcov = vcovHC(reg_lpm))
## Conduct a Wald test on cost coefficients
reg_lpm %>% 
  linearHypothesis('cost_car = -cost_bus', vcov = vcovHC(reg_lpm))

### Part c
## Calculate cost difference to force coefficients to be equal
data_binary <- data_binary %>% 
  mutate(cost_difference = cost_car - cost_bus)
## Model choice as LPM with cost difference
reg_lpm_restricted <- data_binary %>% 
  lm(formula = car ~ time_car + time_bus + cost_difference, data = .)
## Calculate heteroskedastic-robust standard errors
reg_lpm_restricted %>% 
  coeftest(vcov = vcovHC(reg_lpm_restricted))

### Part d
## Calculate estimated probability of car for each individual
data_binary <- data_binary %>% 
  mutate(car_probability_lpm = predict(reg_lpm_restricted))
## Count number of individuals with probabilities outside [0, 1]
data_binary %>%
  filter(car_probability_lpm < 0 | car_probability_lpm > 1) %>% 
  nrow()

### Problem 2 solutions --------------------------------------------------------

### Part a
## Model choice as binary logit with equal cost coefficients
reg_logit <- data_binary %>% 
  glm(formula = car ~ time_car + time_bus + cost_difference, 
      data = ., family = 'binomial')
## Summarize model results
reg_logit %>% 
  summary()
## Calculate estimated utility and probability of car
data_binary <- data_binary %>% 
  mutate(utility_logit = predict(reg_logit),
         car_probability_logit = 1 / (1 + exp(-utility_logit)))
## Calculate mean marginal effects
coef(reg_logit)[2:4] * 
  mean(data_binary$car_probability_logit * 
         (1 - data_binary$car_probability_logit))
## Calculate hourly time-value for each mode
coef(reg_logit)[2:3] / coef(reg_logit)[4] * 60

### Part b
## Model choice as binary logit with cost divided by income
reg_logit_income <- data_binary %>% 
  glm(formula = car ~ time_car + time_bus + I(cost_difference / income), 
      data = ., family = 'binomial')
## Summarize model results
reg_logit_income %>% 
  summary()
## Calculate estimated utility and probability of car
data_binary <- data_binary %>% 
  mutate(utility_logit_income = predict(reg_logit_income),
         car_probability_logit_income = 1 / (1 + exp(-utility_logit_income)))
## Calculate mean marginal effects
coef(reg_logit_income)[2:4] * 
  mean(data_binary$car_probability_logit_income * 
         (1 - data_binary$car_probability_logit_income)) / 
  c(1, 1, mean(data_binary$income))
## Calculate hourly time-values at different income levels
rep(-abs(coef(reg_logit_income)[2:3]), 3) / coef(reg_logit_income)[4] * 60 * 
  c(15, 15, 25, 25, 35, 35)

### Part c
## Model choice as binary logit for single individuals
reg_logit_single <- data_binary %>%
  filter(marital_status == 'single') %>% 
  glm(formula = car ~ time_car + time_bus + I(cost_difference / income), 
      data = ., family = 'binomial')
## Summarize model results
reg_logit_single %>% 
  summary()
## Model choice as binary logit for married individuals
reg_logit_married <- data_binary %>%
  filter(marital_status == 'married') %>% 
  glm(formula = car ~ time_car + time_bus + I(cost_difference / income), 
      data = ., family = 'binomial')
## Summarize model results
reg_logit_married %>% 
  summary() 
## Calculate time-values for singles at different incomes
rep(-abs(coef(reg_logit_single)[2:3]), 3) / coef(reg_logit_single)[4] * 60 * 
  c(15, 15, 25, 25, 35, 35)
## Calculate time-values for marrieds at different incomes
rep(-abs(coef(reg_logit_married)[2:3]), 3) / coef(reg_logit_married)[4] * 60 * 
  c(15, 15, 25, 25, 35, 35)

### Part d
## Create new dataset with express bus times
data_binary_express <- data_binary %>% 
  mutate(time_bus = ifelse(cost_bus == 3, time_bus - 10, time_bus))
## Calculate estimated utility and probability of car with express buses
data_binary_express <- data_binary_express %>%
  mutate(utility_logit_express = predict(reg_logit_income, 
                                         newdata = data_binary_express),
         car_probability_logit_express = 1 / (1 + exp(-utility_logit_express)))
## Count number of bus riders in original model
bus_logit_income <- data_binary %>% 
  filter(car_probability_logit_income < 0.5) %>% 
  nrow()
## Count number of bus riders with express buses
bus_logit_express <- data_binary_express %>% 
  filter(car_probability_logit_express < 0.5) %>% 
  nrow()
## Calculate difference in bus ridership due to express buses
bus_logit_express - bus_logit_income

### Part e
## Create new dataset with bus subsidies
data_binary_subsidy <- data_binary %>% 
  mutate(cost_difference = ifelse(cost_bus == 3, 
                                  cost_difference + 0.5, 
                                  cost_difference))
## Calculate estimated utility and probability of car with bus subsidies
data_binary_subsidy <- data_binary_subsidy %>%
  mutate(utility_logit_subsidy = predict(reg_logit_income, 
                                         newdata = data_binary_subsidy),
         car_probability_logit_subsidy = 1 / (1 + exp(-utility_logit_subsidy)))
## Count number of bus riders with express buses
bus_logit_subsidy <- data_binary_subsidy %>% 
  filter(car_probability_logit_subsidy < 0.5) %>% 
  nrow()
## Calculate difference in bus ridership due to bus subsidies
bus_logit_subsidy - bus_logit_income

### Problem 3 solutions --------------------------------------------------------

### Part a
## Load dataset
data_multi <- read_csv('travel_multinomial.csv')
## Convert dataset to mlogit format
data_mlogit <- data_multi %>% 
  mlogit.data(shape = 'wide', choice = 'mode', varying = 2:9, sep = '_')
## Model choice as multinomial logit with common cost/income coefficient, 
## alternative intercepts, and alternative-specific time coefficients
model_mlogit <- data_mlogit %>% 
  mlogit(mode ~ I(cost / income) | 1 | time, data = .)
## Summarize model results
model_mlogit %>% 
  summary()
## Calculate estimated probabilities for each mode
fitted_mlogit <- fitted(model_mlogit, type = 'probabilities')
## Assign probabilities as variables in dataset
data_multi <- data_multi %>% 
  mutate(bike_probability_mlogit = fitted_mlogit[, 1],
         bus_probability_mlogit = fitted_mlogit[, 2],
         car_probability_mlogit = fitted_mlogit[, 3],
         walk_probability_mlogit = fitted_mlogit[, 4])
## Calculate own- and cross-price elasticities with respect to car cost
data_multi <- data_multi %>%
  mutate(elasticity_cost_car_own_mlogit = coef(model_mlogit)[4] * 
           cost_car / income * (1 - car_probability_mlogit),
         elasticity_cost_car_other_mlogit = -coef(model_mlogit)[4] * 
           cost_car / income * car_probability_mlogit)
## Report means of own- and cross-price elasticities with respect to car cost
data_multi %>% 
  select(elasticity_cost_car_own_mlogit, elasticity_cost_car_other_mlogit) %>% 
  summarize_all(mean) %>% 
  unlist()
## Calculate own- and cross-price elasticities with respect to bus cost
data_multi <- data_multi %>% 
  mutate(elasticity_cost_bus_own_mlogit = coef(model_mlogit)[4] * 
           cost_bus / income * (1 - bus_probability_mlogit),
         elasticity_cost_bus_other_mlogit = -coef(model_mlogit)[4] * 
           cost_bus / income * bus_probability_mlogit)
## Report means of own- and cross-price elasticities with respect to bus cost
data_multi %>% 
  select(elasticity_cost_bus_own_mlogit, elasticity_cost_bus_other_mlogit) %>% 
  summarize_all(mean) %>% 
  unlist()
## Calculate own- and cross- elasticities with respect to bus time
data_multi <- data_multi %>% 
  mutate(elasticity_time_bus_own_mlogit = coef(model_mlogit)[6] * 
           time_bus * (1 - bus_probability_mlogit),
         elasticity_time_bus_other_mlogit = -coef(model_mlogit)[6] * 
           time_bus * bus_probability_mlogit)
## Report means of own- and cross- elasticities with respect to bus time
data_multi %>% 
  select(elasticity_time_bus_own_mlogit, elasticity_time_bus_other_mlogit) %>% 
  summarize_all(mean) %>% 
  unlist()
## Calculate hourly time-value for each mode at different incomes
rep(coef(model_mlogit)[5:8], 3) / coef(model_mlogit)[4] * 60 * 
  c(rep(15, 4), rep(25, 4), rep(35, 4))

### Part b
## Model choice as multinomial logit with common cost/income coefficient and 
## alternative-specific age and time coefficients
model_mlogit_age <- data_mlogit %>% 
  mlogit(mode ~ I(cost / income) | age | time, data = .)
## Summarize model results
model_mlogit_age %>% 
  summary()
## Calculate estimated probabilities for each mode
fitted_mlogit_age <- fitted(model_mlogit_age, type = 'probabilities')
## Assign probabilities as variables in dataset
data_multi <- data_multi %>% 
  mutate(bike_probability_mlogit_age = fitted_mlogit_age[, 1],
         bus_probability_mlogit_age = fitted_mlogit_age[, 2],
         car_probability_mlogit_age = fitted_mlogit_age[, 3],
         walk_probability_mlogit_age = fitted_mlogit_age[, 4])
## Calculate own- and cross-price elasticities with respect to car cost
data_multi <- data_multi %>% 
  mutate(elasticity_cost_car_own_mlogit_age = coef(model_mlogit_age)[4] * 
           cost_car / income * (1 - car_probability_mlogit_age),
         elasticity_cost_car_other_mlogit_age = -coef(model_mlogit_age)[4] * 
           cost_car / income * car_probability_mlogit_age)
## Report means of own- and cross-price elasticities with respect to car cost
data_multi %>% 
  select(elasticity_cost_car_own_mlogit_age, 
         elasticity_cost_car_other_mlogit_age) %>% 
  summarize_all(mean) %>% 
  unlist()
## Calculate own- and cross-price elasticities with respect to bus cost
data_multi <- data_multi %>% 
  mutate(elasticity_cost_bus_own_mlogit_age = coef(model_mlogit_age)[4] * 
           cost_bus / income * (1 - bus_probability_mlogit),
         elasticity_cost_bus_other_mlogit_age = -coef(model_mlogit_age)[4] * 
           cost_bus / income * bus_probability_mlogit_age)
## Report means of own- and cross-price elasticities with respect to bus cost
data_multi %>% 
  select(elasticity_cost_bus_own_mlogit_age, 
         elasticity_cost_bus_other_mlogit_age) %>% 
  summarize_all(mean) %>% 
  unlist()
## Calculate own- and cross- elasticities with respect to bus time
data_multi <- data_multi %>% 
  mutate(elasticity_time_bus_own_mlogit_age = coef(model_mlogit_age)[9] * 
           time_bus * (1 - bus_probability_mlogit_age),
         elasticity_time_bus_other_mlogit_age = -coef(model_mlogit_age)[9] * 
           time_bus * bus_probability_mlogit_age)
## Report means of own- and cross- elasticities with respect to bus time
data_multi %>% 
  select(elasticity_time_bus_own_mlogit_age, 
         elasticity_time_bus_other_mlogit_age) %>% 
  summarize_all(mean) %>% 
  unlist()
## Calculate hourly time-value for each mode at different incomes
rep(coef(model_mlogit_age)[8:11], 3) / coef(model_mlogit_age)[4] * 60 * 
  c(rep(15, 4), rep(25, 4), rep(35, 4))

### Part c
## Create index of individuals
data_multi <- data_multi %>% 
  mutate(id = 1:n())
## Find estimated choice for each individual and join to dataset
data_multi <- data_multi %>% 
  select(id, contains('probability_mlogit_age')) %>% 
  gather(mode_mlogit_age, probability_mlogit_age, -id) %>% 
  group_by(id) %>% 
  arrange(id, desc(probability_mlogit_age)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(id, mode_mlogit_age) %>% 
  mutate(mode_mlogit_age = str_remove(mode_mlogit_age, 
                                      '_probability_mlogit_age')) %>% 
  inner_join(data_multi)
## Create new dataset with express bus times
data_multi_express <- data_multi %>%
  mutate(time_bus = if_else(cost_bus == 3, time_bus - 10, time_bus))
## Convert new dataset of relevant variables to mlogit format
data_mlogit_express <- data_multi_express %>% 
  select(mode, time_car, cost_car, time_bus, cost_bus, time_bike, cost_bike, 
         time_walk, cost_walk, age, income) %>% 
  mlogit.data(shape = 'wide', choice = 'mode', varying = 2:9, sep = '_')
## Calculate estimated probabilities for each mode with express buses
predict_mlogit_express <- predict(model_mlogit_age, 
                                  newdata = data_mlogit_express)
## Assign probabilities as variables in new dataset
data_multi_express <- data_multi_express %>% 
  mutate(bike_probability_mlogit_express = predict_mlogit_express[, 1],
         bus_probability_mlogit_express = predict_mlogit_express[, 2],
         car_probability_mlogit_express = predict_mlogit_express[, 3],
         walk_probability_mlogit_express = predict_mlogit_express[, 4])
## Create index of individuals in new dataset
data_multi_express <- data_multi_express %>% 
  mutate(id = 1:n())
## Find estimated choice for each individual with express buses
data_multi_express <- data_multi_express %>% 
  select(id, contains('probability_mlogit_express')) %>% 
  gather(mode_mlogit_express, probability_mlogit_express, -id) %>% 
  group_by(id) %>% 
  arrange(id, desc(probability_mlogit_express)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(id, mode_mlogit_express) %>% 
  mutate(mode_mlogit_express = str_remove(mode_mlogit_express, 
                                          '_probability_mlogit_express')) %>% 
  inner_join(data_multi_express)
## Count number of bus riders in original model
bus_mlogit_age <- data_multi_express %>% 
  filter(mode_mlogit_age == 'bus') %>% 
  nrow()
## Count number of bus riders with express buses
bus_mlogit_express <- data_multi_express %>% 
  filter(mode_mlogit_express == 'bus') %>% 
  nrow()
## Calculate difference in bus ridership due to express buses
bus_mlogit_express - bus_mlogit_age
## Count number of bikers in original model
bike_mlogit_age <- data_multi_express %>% 
  filter(mode_mlogit_age == 'bike') %>% 
  nrow()
## Count number of bikers with express buses
bike_mlogit_express <- data_multi_express %>% 
  filter(mode_mlogit_express == 'bike') %>% 
  nrow()
## Calculate difference in bikers due to express buses
bike_mlogit_express - bike_mlogit_age
## Count number of drivers in original model
car_mlogit_age <- data_multi_express %>% 
  filter(mode_mlogit_age == 'car') %>% 
  nrow()
## Count number of drivers with express buses
car_mlogit_express <- data_multi_express %>% 
  filter(mode_mlogit_express == 'car') %>% 
  nrow()
## Calculate difference in drivers due to express buses
car_mlogit_express - car_mlogit_age
## Count number of walkers in original model
walk_mlogit_age <- data_multi_express %>% 
  filter(mode_mlogit_age == 'walk') %>% 
  nrow()
## Count number of walkers with express buses
walk_mlogit_express <- data_multi_express %>% 
  filter(mode_mlogit_express == 'walk') %>% 
  nrow()
## Calculate difference in walkers due to express buses
walk_mlogit_express - walk_mlogit_age

### Part d
## Create express bus time variable
data_multi <- data_multi %>% 
  mutate(time_bus_express = if_else(cost_bus == 3, time_bus - 10, time_bus))
## Calculate representative utility of each original mode
data_multi <- data_multi %>%
  mutate(utility_bike_mlogit_age = 0 + 
           coef(model_mlogit_age)[4] * cost_bike / income + 
           0 * age + 
           coef(model_mlogit_age)[8] * time_bike,
         utility_bus_mlogit_age = coef(model_mlogit_age)[1] + 
           coef(model_mlogit_age)[4] * cost_bus / income + 
           coef(model_mlogit_age)[5] * age + 
           coef(model_mlogit_age)[9] * time_bus,
         utility_car_mlogit_age = coef(model_mlogit_age)[2] + 
           coef(model_mlogit_age)[4] * cost_car / income + 
           coef(model_mlogit_age)[6] * age + 
           coef(model_mlogit_age)[10] * time_car,
         utility_walk_mlogit_age = coef(model_mlogit_age)[3] + 
           coef(model_mlogit_age)[4] * cost_walk / income + 
           coef(model_mlogit_age)[7] * age + 
           coef(model_mlogit_age)[11] * time_walk)
## Calculate representative utility of express buses
data_multi <- data_multi %>% 
  mutate(utility_bus_mlogit_express = coef(model_mlogit_age)[1] + 
           coef(model_mlogit_age)[4] * cost_bus / income + 
           coef(model_mlogit_age)[5] * age + 
           coef(model_mlogit_age)[9] * time_bus_express)
## Calculate sum of exponential of representative utilities for original modes
data_multi <- data_multi %>% 
  mutate(sum_exp_mlogit_age = exp(utility_bike_mlogit_age) + 
           exp(utility_bus_mlogit_age) + exp(utility_car_mlogit_age) + 
           exp(utility_walk_mlogit_age))
## Calculate sum of exponential of representative utilities with express buses
data_multi <- data_multi %>% 
  mutate(sum_exp_mlogit_express = exp(utility_bike_mlogit_age) + 
           exp(utility_bus_mlogit_express) + exp(utility_car_mlogit_age) + 
           exp(utility_walk_mlogit_age))
## Calculate change in consumer surplus from express buses
data_multi <- data_multi %>% 
  mutate(surplus_change_express = (log(sum_exp_mlogit_express) - 
                                     log(sum_exp_mlogit_age)) / 
           abs(coef(model_mlogit_age)[4] / income))
## Scale consumer surplus change for total number of graduate students
sum(data_multi$surplus_change_express) * 7078 / 1000
