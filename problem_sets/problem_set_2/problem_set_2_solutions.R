########################################################
##### ResEcon 703: Topics in Advanced Econometrics #####
#####         Problem set 2 solution code          #####
#####         Matt Woerman, UMass Amherst          #####
########################################################

### Load packages for problem set
library(tidyverse)
library(mlogit)


### Problem 1 solutions --------------------------------------------------------

### Part a
## Load dataset
data_binary <- read_csv('commute_binary.csv')
## Clean choice variable
data_binary <- data_binary %>% 
  mutate(car = (mode == 'car'))
## Model choice as binary logit
model_1a <- glm(formula = car ~ cost.car + time.car + time.bus, 
                family = 'binomial',
                data = data_binary)
## Summarize model results
summary(model_1a)
## Calculate estimated utility and probability of car
data_binary <- data_binary %>% 
  mutate(utility_1a = predict(model_1a),
         prob_car_1a = 1 / (1 + exp(-utility_1a)))
## Calculate marginal effects
data_binary <- data_binary %>% 
  mutate(prob_prod_1a = prob_car_1a * (1 - prob_car_1a),
         mfx_cost_car = coef(model_1a)[2] * prob_prod_1a,
         mfx_time_car = coef(model_1a)[3] * prob_prod_1a,
         mfx_time_bus = coef(model_1a)[4] * prob_prod_1a)
## Summarize marginal effects
data_binary %>% 
  select(starts_with('mfx')) %>% 
  summary()
## Calculate hourly time-value for each commute mode
abs(coef(model_1a)[3:4] / coef(model_1a)[2]) * 60

### Part b
## Model choice as binary logit with cost divided by income
model_1b <- glm(formula = car ~ I(cost.car / income) + time.car + time.bus, 
                family = 'binomial',
                data = data_binary)
## Summarize model results
summary(model_1b)
## Calculate marginal utility of car cost at different incomes
-coef(model_1b)[2] / c(15, 25, 35)
## Calculate hourly time-value for each commute mode at different incomes
rep(abs(coef(model_1b)[3:4] / coef(model_1b)[2]), 3) * 
  c(rep(15, 2), rep(25, 2), rep(35, 2)) * 60


### Problem 2 solutions --------------------------------------------------------

### Part a
## Load dataset
data_multi <- read_csv('commute_multinomial.csv')
## Convert dataset to data frame format
data_df <- as.data.frame(data_multi)
## Convert dataset to mlogit format
data_dfidx <- dfidx(data_df, shape = 'wide', choice = 'mode', varying = 3:10)
## Model choice as multinomial logit with common cost coefficient, 
## alternative intercepts, and alternative-specific time coefficients
model_2a <- mlogit(formula = mode ~ cost | 1 | time, 
                   data = data_dfidx)
## Summarize model results
summary(model_2a)
## Calculate the choice probabilities for car
data_multi <- data_multi %>% 
  mutate(prob_car_2a = fitted(model_2a, type = 'probabilities')[, 3])
## Calculate the own elasticity of car cost
data_multi <- data_multi %>% 
  mutate(elas_own_car_cost_2a = 
           coef(model_2a)[4] * cost.car * (1 - prob_car_2a))
## Calculate the cross-elasticity of car cost
data_multi <- data_multi %>% 
  mutate(elas_cross_car_cost_2a = 
           -coef(model_2a)[4] * cost.car * prob_car_2a)
## Summarize elasticities
data_multi %>% 
  select(starts_with('elas')) %>% 
  summary()

### Part b
## Create a separate dataset of single students
data_dfidx_single <- data_dfidx %>% 
  filter(marital_status == 'single')
## Create a separate datasets of married students
data_dfidx_married <- data_dfidx %>% 
  filter(marital_status == 'married')
## Model choice for single students
model_2b_single <- mlogit(formula = mode ~ cost | 1 | time, 
                          data = data_dfidx_single)
## Model choice for single students
model_2b_married <- mlogit(formula = mode ~ cost | 1 | time, 
                           data = data_dfidx_married)
## Summarize model results for single students
summary(model_2b_single)
## Summarize model results for married students
summary(model_2b_married)
## Calculate hourly time-value for each commute mode for single students
abs(coef(model_2b_single)[5:8] / coef(model_2b_single)[4]) * 60
## Calculate hourly time-value for each commute mode for married students
abs(coef(model_2b_married)[5:8] / coef(model_2b_married)[4]) * 60

### Part c
## Create counterfactual data with more frequent buses
data_df_counter <- data_df %>% 
  mutate(time.bus = 0.8 * time.bus)
## Convert counterfactual data to dfidx format
data_counter_dfidx <- dfidx(data_df_counter, shape = 'wide', 
                            choice = 'mode', varying = 3:10)
## Calculate aggregate choices using observed data
agg_choices_obs <- predict(model_2a, newdata = data_dfidx)
## Calculate aggregate choices using counterfactual data
agg_choices_counter <- predict(model_2a, newdata = data_counter_dfidx)
## Calculate difference between aggregate choices
colSums(agg_choices_counter - agg_choices_obs)
## Calculate log-sum values using observed data
logsum_obs <- logsum(model_2a, data = data_dfidx)
## Calculate log-sum values using counterfactual data
logsum_counter <- logsum(model_2a, data = data_counter_dfidx)
## Calculate change in consumer surplus from subsidy
sum((logsum_counter - logsum_obs) / -coef(model_2a)[4])
