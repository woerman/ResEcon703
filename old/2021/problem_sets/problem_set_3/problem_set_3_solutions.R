########################################################
##### ResEcon 703: Topics in Advanced Econometrics #####
#####         Problem set 3 solution code          #####
#####         Matt Woerman, UMass Amherst          #####
########################################################

### Load packages for problem set
library(tidyverse)
library(mlogit)


### Problem 1 solutions --------------------------------------------------------

### Part a
## Load dataset
data_camping <- read_csv('camping.csv')
## Convert dataset to dfidx format
data_dfidx <- dfidx(data = data_camping, shape = 'long', 
                    choice = 'visit', idx = c('camper_id', 'park_id'))
# Model camping park visit as a multinomial logit
model_1a <- mlogit(formula = visit ~ cost + time + mountain | 0,
                   data = data_dfidx)
## Summarize model results
summary(model_1a)
## Calculate value of time and mountain park
coef(model_1a)[2:3] / coef(model_1a)[1] * c(60, -1)
## Calculate mean elasticities with respect to the cost of alternative 1
data_camping %>%
  filter(park_id == 1) %>%
  mutate(prob = fitted(model_1a, type = 'probabilities')[, 1],
         own_elas = coef(model_1a)[1] * cost * (1 - prob),
         cross_elas = -coef(model_1a)[1] * cost * prob) %>%
  summarize(own_elas = mean(own_elas),
            cross_elas = mean(cross_elas))

### Part b
## Model camping park visit as a nested logit
model_1b <- mlogit(formula = visit ~ cost + time + mountain | 0,
                   data = data_dfidx,
                   nests = list(mountain = 1:2, beach = 3:5))
## Summarize model results
summary(model_1b)
## Conduct likelihood ratio test of the models in parts b and d
lrtest(model_1a, model_1b)
## Calculate value of time and mountain park
coef(model_1b)[2:3] / coef(model_1b)[1] * c(60, -1)
## Calculate choice probabilities for every alternative
probs_1b <- fitted(model_1b, type = 'probabilities')
## Calculate mean elasticities with respect to the cost of alternative 1
data_camping %>% 
  filter(park_id == 1) %>% 
  mutate(prob = probs_1b[, 1],
         prob_nest = rowSums(probs_1b[, 1:2]),
         prob_cond = prob / prob_nest,
         own_elas = coef(model_1b)[1] * cost * 
           ((1 / coef(model_1b)[4]) - 
              ((1 - coef(model_1b)[4]) / 
                 coef(model_1b)[4] * prob_cond) - 
              prob),
         cross_elas_mountain = -coef(model_1b)[1] * cost * prob * 
           (1 + ((1 - coef(model_1b)[4]) / 
                   (coef(model_1b)[4] * prob_nest))),
         cross_elas_beach = -coef(model_1b)[1] * cost * prob) %>% 
  summarize(own_elas = mean(own_elas), 
            cross_elas_mountain = mean(cross_elas_mountain),
            cross_elas_beach = mean(cross_elas_beach))


### Problem 2 solutions --------------------------------------------------------

### Part a
## Model camping park visit as a mixed logit
model_2a <- mlogit(formula = visit ~ cost + time + mountain | 0, 
                   data = data_dfidx, 
                   rpar = c(cost = 'n', time = 'n', mountain = 'n'), 
                   R = 100, seed = 703)
## Summarize model results
summary(model_2a)

### Part b
## Model camping park visit as a mixed logit with fixed cost coefficient
model_2b <- mlogit(formula = visit ~ cost + time + mountain | 0, 
                   data = data_dfidx, 
                   rpar = c(time = 'n', mountain = 'n'), 
                   R = 100, seed = 703)
## Summarize model results
summary(model_2b)
## Conduct likelihood ratio test of the models in parts a and b
lrtest(model_2a, model_2b)
## Calculate distribution of the value of time
c(coef(model_2b)[2] / coef(model_2b)[1] * 60,
  abs(coef(model_2b)[4]) / -coef(model_2b)[1] * 60) %>% 
  setNames(c('time', 'sd.time'))
## Calculate distribution of the value of a mountain park
c(coef(model_2b)[3] / -coef(model_2b)[1],
  abs(coef(model_2b)[5]) / -coef(model_2b)[1]) %>% 
  setNames(c('mountain', 'sd.mountain'))
## Calculate proportion of visitors with a positive value of mountain parks
1 - pnorm(q = 0,
          mean = coef(model_2b)[3] / -coef(model_2b)[1],
          sd = abs(coef(model_2b)[5]) / -coef(model_2b)[1])

### Part c
## Create counterfactual camping dataset
data_camping_counter <- data_camping %>% 
  mutate(cost = if_else(park_id == 1, cost + 20, cost))
## Convert dataset to dfidx format
data_counter_dfidx <- dfidx(data = data_camping_counter, shape = 'long', 
                            choice = 'visit', idx = c('camper_id', 'park_id'))
## Calculate aggregate choices using observed data
agg_choices_obs_2b <- predict(model_2b, newdata = data_dfidx)
## Calculate aggregate choices using counterfactual data
agg_choices_counter_2b <- predict(model_2b, newdata = data_counter_dfidx)
## Calculate difference between aggregate choices
colSums(agg_choices_counter_2b - agg_choices_obs_2b)
## Calculate log-sum values using observed data
logsum_obs_2b <- logsum(model_2b, data = data_dfidx)
## Calculate log-sum values using counterfactual data
logsum_counter_2b <- logsum(model_2b, data = data_counter_dfidx)
## Calculate change in consumer surplus from subsidy
sum((logsum_counter_2b - logsum_obs_2b)) / -coef(model_2b)[1]
