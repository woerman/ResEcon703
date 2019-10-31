### ResEcon 703: Topics in Advanced Econometrics
### Problem set 3 solution code
### Matt Woerman, UMass Amherst

### Load packages for problem set
library(tidyverse)
library(gmm)
library(mlogit)

### Problem 1 solutions --------------------------------------------------------

### Part a
## Load dataset
travel_binary <- read_csv('travel_binary.csv')
## Create matrix of data
data_1a <- travel_binary %>% 
  mutate(choice = 1 * (mode == 'car'), 
         cost_difference = cost_car - cost_bus,
         time_difference = time_car - time_bus) %>% 
  select(choice, cost_difference, time_difference) %>% 
  as.matrix()
## Function to calculate binary logit moments
calculate_moments_1a <- function(parameters, data){
  ## Extract explanatory variable data from matrix
  data_x <- data[, -1]
  ## Extract choice data from matrix
  data_y <- data[, 1]
  ## Calculate net utility of alternative given the parameters
  utility <- data_x %*% parameters
  ## Caclculate logit probability of alternative given the parameters
  probability_choice <- 1 / (1 + exp(-utility))
  ## Calculate residuals
  residuals <- data_y - probability_choice
  ## Create moment matrix
  moments <- c(residuals) * data_x
  return(moments)
}
## Use GMM to estimate model
model_1a <- gmm(calculate_moments_1a, data_1a, c(0, 0), vcov = 'iid',
                control = list(reltol = 1e-25, maxit = 10000))
## Summarize model results
model_1a %>% 
  summary()

### Part b
## Create matrix of data
data_1b <- travel_binary %>% 
  mutate(choice = 1 * (mode == 'car'), 
         constant = 1, 
         cost_bus = -cost_bus, 
         time_bus = -time_bus) %>% 
  select(choice, constant, cost_car, time_car, cost_bus, time_bus) %>% 
  as.matrix()
## Function to calculate binary logit moments
calculate_moments_1b <- function(parameters, data){
  ## Extract explanatory variable data from matrix
  data_x <- data[, -1]
  ## Extract choice data from matrix
  data_y <- data[, 1]
  ## Calculate net utility of alternative given the parameters
  utility <- data_x %*% parameters
  ## Caclculate logit probability of alternative given the parameters
  probability_choice <- 1 / (1 + exp(-utility))
  ## Calculate residuals
  residuals <- data_y - probability_choice
  ## Create moment matrix
  moments <- c(residuals) * data_x
  return(moments)
}
## Use GMM to estimate model
model_1b <- gmm(calculate_moments_1b, data_1b, rep(0, 5), vcov = 'iid',
                control = list(reltol = 1e-25, maxit = 10000))
## Summarize model results
model_1b %>% 
  summary()

### Part c
## Create matrix of data
data_1c <- travel_binary %>% 
  mutate(choice = 1 * (mode == 'car'), 
         constant = 1, 
         cost_bus = -cost_bus, 
         time_bus = -time_bus) %>% 
  select(choice, constant, cost_car, time_car, cost_bus, time_bus,
         price_gas, snowfall, car_in_shop) %>% 
  as.matrix()
## Function to calculate binary logit moments
calculate_moments_1c <- function(parameters, data){
  ## Extract explanatory variable data from matrix
  data_x <- data[, 2:6]
  ## Extract choice data from matrix
  data_y <- data[, 1]
  ## Extract instrument data from matrix
  data_z <- data[, c(2, 5:9)]
  ## Calculate net utility of alternative given the parameters
  utility <- data_x %*% parameters
  ## Caclculate logit probability of alternative given the parameters
  probability_choice <- 1 / (1 + exp(-utility))
  ## Calculate residuals
  residuals <- data_y - probability_choice
  ## Create moment matrix
  moments <- c(residuals) * data_z
  return(moments)
}
## Use GMM to estimate model
model_1c <- gmm(calculate_moments_1c, data_1c, rep(0, 5), vcov = 'iid',
                control = list(reltol = 1e-25, maxit = 10000))
## Summarize model results
model_1c %>% 
  summary()
## Test overidentifying restrictions
model_1c %>% 
  specTest()

### Problem 2 solutions --------------------------------------------------------

### Part a
## Load dataset
phones <- read_csv('phones.csv')
## Convert dataset to mlogit format
data_2 <- phones %>% 
  mlogit.data(shape = 'long', choice = 'purchase', alt.var = 'phone_id')
## Model phone purchase as a multinomial logit
model_2a <- data_2 %>% 
  mlogit(purchase ~ storage + screen + price | 0 | 0, data = .)
## Summarize model results
model_2a %>% 
  summary()
## Calculate estimated probabilities for each phone
probability_2a <- fitted(model_2a, type = 'probabilities')[1, ]
## Calculate value of storage and screen size
model_2a$coefficients[1:2] / -model_2a$coefficients[3] * c(1, 0.1)
## Calculate elasticities with respect to the price of alternative 6
model_2a$coefficients[3] * phones$price[6] * 
  c(1 - probability_2a[6], -probability_2a[6]) %>% 
  setNames(c('own', 'cross'))

### Part b
## Model phone purchase as a nested logit with brand nests
model_2b <- data_2 %>% 
  mlogit(purchase ~ storage + screen + price | 0 | 0, data = .,
         nests = list(google = 1:4, apple = 5:10))
## Summarize model results
model_2b %>% 
  summary()
## Calculate estimated probabilities for each phone
probability_2b <- fitted(model_2b, type = 'probabilities')[1, ]
## Calculate value of storage and screen size
model_2b$coefficients[1:2] / -model_2b$coefficients[3] * c(1, 0.1)
## Calculate elasticities with respect to the price of alternative 6
c(model_2b$coefficients[3] * phones$price[6] * 
    ((1 / model_2b$coefficients[5]) - 
       ((1 - model_2b$coefficients[5]) / model_2b$coefficients[5] * 
          (probability_2b[6] / sum(probability_2b[5:10]))) - 
       probability_2b[6]),
  -model_2b$coefficients[3] * phones$price[6] * probability_2b[6] * 
    (1 + (1 - model_2b$coefficients[5]) / model_2b$coefficients[5] * 
       1 / sum(probability_2b[5:10])),
  -model_2b$coefficients[3] * phones$price[6] * probability_2b[6]) %>% 
  setNames(c('own', 'within nest', 'other nest'))

### Part c
## Conduct likelihood ratio test of the models in parts a and b
lrtest(model_2a, model_2b)

### Part d
## Model phone purchase as a nested logit with model nests
model_2d <- data_2 %>% 
  mlogit(purchase ~ storage + screen + price | 0 | 0, data = .,
         nests = list(model_1 = 1:2,
                      model_2 = 3:4,
                      model_3 = 5:6,
                      model_4 = 7:8, 
                      model_5 = 9:10))
## Summarize model results
model_2d %>% 
  summary()
## Calculate estimated probabilities for each phone
probability_2d <- fitted(model_2d, type = 'probabilities')[1, ]
## Calculate value of storage and screen size
model_2d$coefficients[1:2] / -model_2d$coefficients[3] * c(1, 0.1)
## Calculate elasticities with respect to the price of alternative 6
c(model_2d$coefficients[3] * phones$price[6] * 
    ((1 / model_2d$coefficients[6]) - 
       ((1 - model_2d$coefficients[6]) / model_2d$coefficients[6] * 
          (probability_2d[6] / sum(probability_2d[5:6]))) - 
       probability_2d[6]),
  -model_2d$coefficients[3] * phones$price[6] * probability_2d[6] * 
    (1 + (1 - model_2d$coefficients[6]) / model_2d$coefficients[6] * 
       1 / sum(probability_2d[5:6])),
  -model_2d$coefficients[3] * phones$price[6] * probability_2d[6]) %>% 
  setNames(c('own', 'within nest', 'other nest'))
