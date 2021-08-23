########################################################
##### ResEcon 703: Topics in Advanced Econometrics #####
#####         Week 3: Random Utility Model         #####
#####         Matt Woerman, UMass Amherst          #####
########################################################

### Linear Probability Model ---------------------------------------------------

### Load and look at dataset
## Load tidyverse
library(tidyverse)
## Load dataset
ac_data <- read_csv('ac_renters.csv')
## Look at dataset
ac_data

### Model air conditioning as a linear probability model
## Regress air conditioning on cost variables
reg_lpm <- lm(formula = air_conditioning ~ cost_system + cost_operating,
              data = ac_data)
## Summarize regression results
summary(reg_lpm)
## Display regression coefficients
coef(reg_lpm)

### Calculate the fitted values of the model 
## Calculate probability of air conditioning
ac_data <- ac_data %>% 
  mutate(probability_ac_lpm = predict(reg_lpm))
## Look at probabilities and other data
ac_data %>% 
  select(air_conditioning, starts_with('cost'), probability_ac_lpm)

### Visualize probability of air conditioning adoption
## Plot density of probabilities
ac_data %>% 
  ggplot(aes(x = probability_ac_lpm)) +
  geom_density() +
  xlab('Probability of air conditioning') +
  ylab('Kernel Density')  
## Plot air conditioning vs. probability of air conditioning
ac_data %>% 
  ggplot(aes(x = probability_ac_lpm, y = air_conditioning)) +
  geom_point() +
  xlab('Probability of air conditioning') +
  ylab('Air conditioning')
## Plot fraction vs. probability of air conditioning using bins
ac_data %>% 
  mutate(bin = cut(probability_ac_lpm,
                   breaks = seq(-0.2, 1, 0.05),
                   labels = 1:24)) %>%
  group_by(bin) %>% 
  summarize(fraction_ac = mean(air_conditioning), .groups = 'drop') %>% 
  mutate(bin = as.numeric(bin),
         bin_mid = 0.05 * (bin - 1) + 0.025 - 0.2) %>% 
  ggplot(aes(x = bin_mid, y = fraction_ac)) +
  geom_point() +
  xlab('Probability of air conditioning') +
  ylab('Fraction with air conditioning')

### Visualize heteroskedastic residuals
## Calculate squared residuals
ac_data <- ac_data %>% 
  mutate(sq_residual_lpm = (air_conditioning - probability_ac_lpm)^2)
## Plot squared residual vs. probability of air conditioning
ac_data %>% 
  ggplot(aes(x = probability_ac_lpm, y = sq_residual_lpm)) +
  geom_point() +
  xlab('Probability of air conditioning') +
  ylab('Squared residual')

### Calculate heteroskedastic-robust standard errors
## Load lmtest and sandwich
library(lmtest)
library(sandwich)
## Summarize regression results with robust standard errors
reg_lpm %>% 
  coeftest(vcov = vcovHC(reg_lpm))

### Model air conditioning with heterogeneous cost coefficients
## Regress air conditioning on costs divided by income
reg_lpm_inc <- lm(formula = air_conditioning ~ I(cost_system / income) + 
                    I(cost_operating / income),
                  data = ac_data)
## Summarize regression results with robust standard errors
reg_lpm_inc %>% 
  coeftest(vcov = vcovHC(reg_lpm_inc))
## Display regression coefficients
coef(reg_lpm_inc)

### Visualize income variable
## Plot kernel density of income
ac_data %>% 
  ggplot(aes(x = income)) +
  geom_density() +
  xlab('Income') +
  ylab('Kernel density')

### Calculate marginal effects of cost variables
## Calculate marginal effects of costs when income == 30
coef(reg_lpm_inc)[2:3] / 30
## Calculate marginal effects of costs when income == 60
coef(reg_lpm_inc)[2:3] / 60
## Calculate marginal effects of costs when income == 90
coef(reg_lpm_inc)[2:3] / 90

### Model air conditioning with residents as an explanatory variable
## Regress air conditioning on scaled costs and number of residents
reg_lpm_res <- lm(formula = air_conditioning ~ I(cost_system / income) + 
                    I(cost_operating / income) + residents,
                  data = ac_data)
## Summarize regression results with robust standard errors
reg_lpm_res %>% 
  coeftest(vcov = vcovHC(reg_lpm_res))
