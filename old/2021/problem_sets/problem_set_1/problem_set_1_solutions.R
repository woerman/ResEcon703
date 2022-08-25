########################################################
##### ResEcon 703: Topics in Advanced Econometrics #####
#####         Problem set 1 solution code          #####
#####         Matt Woerman, UMass Amherst          #####
########################################################

### Load packages for problem set
library(tidyverse)
library(lmtest)
library(sandwich)
library(car)
library(mlogit)


### Problem 1 solutions --------------------------------------------------------

### Part a
## Load dataset
data_binary <- read_csv('commute_binary.csv')
## Clean choice variable
data_binary <- data_binary %>% 
  mutate(car = (mode == 'car'))
## Model choice as a linear probability model
reg_1a <- lm(formula = car ~ cost.car + time.car + time.bus, 
             data = data_binary)
## Calculate heteroskedastic-robust standard errors
coeftest(reg_1a, vcov = vcovHC(reg_1a))
## Calculate estimated probability of car for each individual
data_binary <- data_binary %>% 
  mutate(prob_car_1a = predict(reg_1a))
## Count number of individuals with probabilities outside [0, 1]
data_binary %>%
  filter(prob_car_1a < 0 | prob_car_1a > 1) %>% 
  nrow()
## Conduct a Wald test on time coefficients
linearHypothesis(reg_1a, 'time.car = -time.bus', vcov = vcovHC(reg_1a))


### Problem 2 solutions --------------------------------------------------------

### Part a
## Model choice as binary logit
model_2a <- glm(formula = car ~ cost.car + time.car + time.bus, 
                family = 'binomial',
                data = data_binary)
## Summarize model results
summary(model_2a)
## Calculate estimated utility and probability of car
data_binary <- data_binary %>% 
  mutate(utility_2a = predict(model_2a),
         prob_car_2a = 1 / (1 + exp(-utility_2a)))
## Calculate marginal effects
data_binary <- data_binary %>% 
  mutate(prob_prod_2a = prob_car_2a * (1 - prob_car_2a),
         mfx_cost_car = coef(model_2a)[2] * prob_prod_2a,
         mfx_time_car = coef(model_2a)[3] * prob_prod_2a,
         mfx_time_bus = coef(model_2a)[4] * prob_prod_2a)
## Summarize marginal effects
data_binary %>% 
  select(starts_with('mfx')) %>% 
  summary()
## Calculate hourly time-value for each commute mode
abs(coef(model_2a)[3:4] / coef(model_2a)[2]) * 60

### Part b
## Model choice as binary logit with cost divided by income
model_2b <- glm(formula = car ~ I(cost.car / income) + time.car + time.bus, 
                family = 'binomial',
                data = data_binary)
## Summarize model results
summary(model_2b)
## Calculate marginal utility of car cost at different incomes
coef(model_2b)[2] / c(15, 25, 35)
## Calculate hourly time-value for each commute mode at different incomes
rep(abs(coef(model_2b)[3:4] / coef(model_2b)[2]), 3) * 
  c(rep(15, 2), rep(25, 2), rep(35, 2)) * 60


### Problem 3 solutions --------------------------------------------------------

### Part a
## Load dataset
data_multi <- read_csv('commute_multinomial.csv')
## Convert dataset to data frame format
data_df <- as.data.frame(data_multi)
## Convert dataset to mlogit format
data_dfidx <- dfidx(data_df, shape = 'wide', choice = 'mode', varying = 3:10)
## Model choice as multinomial logit with common cost coefficient, 
## alternative intercepts, and alternative-specific time coefficients
model_3a <- mlogit(formula = mode ~ cost | 1 | time, 
                   data = data_dfidx)
## Summarize model results
summary(model_3a)
## Calculate the choice probabilities for car
data_multi <- data_multi %>% 
  mutate(prob_car_3a = fitted(model_3a, type = 'probabilities')[, 3])
## Calculate the own elasticity of car cost
data_multi <- data_multi %>% 
  mutate(elas_own_car_cost_3a = 
           coef(model_3a)[4] * cost.car * (1 - prob_car_3a))
## Calculate the cross-elasticity of car cost
data_multi <- data_multi %>% 
  mutate(elas_cross_car_cost_3a = 
           -coef(model_3a)[4] * cost.car * prob_car_3a)
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
model_3b_single <- mlogit(formula = mode ~ cost | 1 | time, 
                          data = data_dfidx_single)
## Model choice for single students
model_3b_married <- mlogit(formula = mode ~ cost | 1 | time, 
                           data = data_dfidx_married)
## Summarize model results for single students
summary(model_3b_single)
## Summarize model results for married students
summary(model_3b_married)
## Calculate hourly time-value for each commute mode for single students
abs(coef(model_3b_single)[5:8] / coef(model_3b_single)[4]) * 60
## Calculate hourly time-value for each commute mode for married students
abs(coef(model_3b_married)[5:8] / coef(model_3b_married)[4]) * 60

### Part c
## Create counterfactual data with more frequent buses
data_df_counter <- data_df %>% 
  mutate(time.bus = 0.8 * time.bus)
## Convert counterfactual data to dfidx format
data_counter_dfidx <- dfidx(data_df_counter, shape = 'wide', 
                            choice = 'mode', varying = 3:10)
## Calculate aggregate choices using observed data
agg_choices_obs <- predict(model_3a, newdata = data_dfidx)
## Calculate aggregate choices using counterfactual data
agg_choices_counter <- predict(model_3a, newdata = data_counter_dfidx)
## Calculate difference between aggregate choices
colSums(agg_choices_counter - agg_choices_obs)
## Calculate log-sum values using observed data
logsum_obs <- logsum(model_3a, data = data_dfidx)
## Calculate log-sum values using counterfactual data
logsum_counter <- logsum(model_3a, data = data_counter_dfidx)
## Calculate change in consumer surplus from subsidy
sum((logsum_counter - logsum_obs) / -coef(model_3a)[4])
