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


### Problem 1 solutions --------------------------------------------------------

### Part a
## Load dataset
data_binary <- read_csv('commute_binary.csv')
## Calculate means and medians of time and cost variables for full sample
summ_stats_1a <- data_binary %>% 
  summarize(mn_cost_c = mean(cost.car),
            md_cost_c = median(cost.car),
            mn_time_c = mean(time.car),
            md_time_c = median(time.car),
            mn_time_b = mean(time.bus),
            md_time_b = median(time.bus))
## Report summary stats for full sample
summ_stats_1a

### Part b
## Calculate means and medians of time and cost variable by commute mode
summ_stats_1b <- data_binary %>% 
  group_by(mode) %>% 
  summarize(mn_cost_c = mean(cost.car),
            md_cost_c = median(cost.car),
            mn_time_c = mean(time.car),
            md_time_c = median(time.car),
            mn_time_b = mean(time.bus),
            md_time_b = median(time.bus))
## Report summary stats for subsamples
summ_stats_1b


### Problem 2 solutions --------------------------------------------------------

### Part a
## Clean choice variable
data_binary <- data_binary %>% 
  mutate(car = (mode == 'car'))
## Model choice as a linear probability model
reg_2a <- lm(formula = car ~ cost.car + time.car + time.bus, 
            data = data_binary)
## Calculate heteroskedastic-robust standard errors
coeftest(reg_2a, vcov = vcovHC(reg_2a))
## Calculate estimated probability of car for each individual
data_binary <- data_binary %>% 
  mutate(prob_car_2a = predict(reg_2a))
## Count number of individuals with probabilities outside [0, 1]
data_binary %>%
  filter(prob_car_2a < 0 | prob_car_2a > 1) %>% 
  nrow()
## Conduct a Wald test on time coefficients
linearHypothesis(reg_2a, 'time.car = -time.bus', vcov = vcovHC(reg_2a))
