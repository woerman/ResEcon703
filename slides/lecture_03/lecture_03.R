### ResEcon 703: Topics in Advanced Econometrics
### Lecture 3: Random Utility Model
### Matt Woerman, UMass Amherst

### Linear Probability Model Example in R --------------------------------------

### Load and look at dataset
## Load tidyverse
library(tidyverse)
## Load dataset
data <- read_csv('ac_renters.csv')
## Look at dataset
data

### Model air conditioning as a linear probability model
## Regress air conditioning on cost variables
reg_lmp <- data %>% 
  lm(formula = air_conditioning ~ cost_system + cost_operating)
## Summarize regression results
reg_lmp %>% 
  summary()

### Visualize probability of air conditioning adoption
## Calculate probability of air conditioining
data <- data %>% 
  mutate(probability_ac_lmp = predict(reg_lmp))
## Plot air conditioning vs. probability of air conditioning
data %>% 
  ggplot(aes(x = probability_ac_lmp, y = air_conditioning)) +
  geom_point() +
  xlab('Probability of air conditioning') +
  ylab('Air conditioining')

### Visualize probability of air conditioning using bins
## Plot fraction vs. probability of air conditioning using bins
data %>% 
  mutate(bin = cut(probability_ac_lmp,
                   breaks = seq(0, 1, 0.05),
                   labels = 1:20)) %>%
  group_by(bin) %>% 
  summarize(fraction_ac = mean(air_conditioning)) %>% 
  mutate(bin = as.numeric(bin),
         bin_mid = 0.05 * (bin - 1) + 0.025) %>% 
  ggplot(aes(x = bin_mid, y = fraction_ac)) +
  geom_point() +
  xlab('Probability of air conditioning') +
  ylab('Fraction with air conditioning')
  
### Visualize heteroskedastic residuals
## Calculate squared residuals
data <- data %>% 
  mutate(sq_residual_lmp = (air_conditioning - probability_ac_lmp)^2)
## Plot squared residual vs. probability of air conditioning
data %>% 
  ggplot(aes(x = probability_ac_lmp, y = sq_residual_lmp)) +
  geom_point() +
  xlab('Probability of air conditioning') +
  ylab('Squared residual')

### Calculate heteroskedastic-robust standard errors
## Load lmtest and sandwich
library(lmtest)
library(sandwich)
## Summarize regression results with 
robust standard errors
reg_lmp %>% 
  coeftest(vcov = vcovHC(reg_lmp))

### Model air conditioning with heterogeneous cost coefficients
## Regress air conditioning on costs divided by income
reg_lmp_income <- data %>% 
  lm(formula = air_conditioning ~ I(cost_system / income) + 
       I(cost_operating / income))
## Summarize regression results with robust standard errors
reg_lmp_income %>% 
  coeftest(vcov = vcovHC(reg_lmp_income))

### Visualize income variable
## Plot kernel density of income
data %>% 
  ggplot(aes(x = income)) +
  geom_density() +
  xlab('Income') +
  ylab('Kernel density')

### Calculate marginal effects of cost variables
## Calculate marginal effects of costs when income == 60
coef(reg_lmp_income)[2:3] / 60
## Calculate marginal effects of costs when income == 30
coef(reg_lmp_income)[2:3] / 30
## Calculate marginal effects of costs when income == 90
coef(reg_lmp_income)[2:3] / 90

### Model air conditioning with residents as an explanatory variable
## Regress air conditoning on scaled costs and number of residents
reg_lmp_residents <- data %>% 
  lm(formula = air_conditioning ~ I(cost_system / income) + 
       I(cost_operating / income) + residents)
## Summarize regression results with robust standard errors
reg_lmp_residents %>% 
  coeftest(vcov = vcovHC(reg_lmp_residents))
