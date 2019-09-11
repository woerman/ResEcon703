### ResEcon 703: Topics in Advanced Econometrics
### Lecture 4: Logit Model I
### Matt Woerman, UMass Amherst

### Load packages --------------------------------------------------------------
library(tidyverse)
library(evd)

### Logit Model ----------------------------------------------------------------

### Plot type I extreme value density and distribution
## Plot density of type I extreme value
ev_pdf <- tibble(x = seq(-5, 5, .01), y = dgumbel(x)) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  xlab(NULL) +
  ylab(NULL)
## Output density plot
ggsave('ev_pdf.pdf', ev_pdf, width = 3, height = 3)
## Plot distirbution of type I extreme value
ev_cdf <- tibble(x = seq(-5, 5, .01), y = pgumbel(x)) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  xlab(NULL) +
  ylab(NULL)
## Output distribution plot
ggsave('ev_cdf.pdf', ev_cdf, width = 3, height = 3)

### Plot logistic density and distribution
## Plot density of logistic
logistic_pdf <- tibble(x = seq(-5, 5, .01), y = dlogis(x)) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  xlab(NULL) +
  ylab(NULL)
## Output density plot
ggsave('logistic_pdf.pdf', logistic_pdf, width = 3, height = 3)
## Plot distirbution of logistic
logistic_cdf <- tibble(x = seq(-5, 5, .01), y = plogis(x)) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  xlab(NULL) +
  ylab(NULL)
## Output distribution plot
ggsave('logistic_cdf.pdf', logistic_cdf, width = 3, height = 3)

### Binary Logit Model Example in R --------------------------------------------

### Load and look at dataset
## Load tidyverse
library(tidyverse)
## Load dataset
data <- read_csv('ac_renters.csv')
## Look at dataset
data

### Model air conditioning as a binary logit
## Regress air conditioning on cost variables
reg_logit <- data %>% 
  glm(formula = air_conditioning ~ cost_system + cost_operating, 
      family = 'binomial')
## Summarize regression results
reg_logit %>% 
  summary()

### Visualize utility of air conditioning adoption using bins
## Calculate predicted utility of air conditioning
data <- data %>% 
  mutate(utility_ac_logit = predict(reg_logit))
## Plot fraction vs. utility of air conditioning using bins
data %>% 
  mutate(bin = cut(utility_ac_logit,
                   breaks = seq(-1.7, 1.4, 0.1),
                   labels = 1:31)) %>%
  group_by(bin) %>% 
  summarize(fraction_ac = mean(air_conditioning)) %>% 
  mutate(bin = as.numeric(bin),
         bin_mid = 0.1 * (bin - 1) - 1.65) %>% 
  ggplot(aes(x = bin_mid, y = fraction_ac)) +
  geom_point() +
  xlab('Utility of air conditioning') +
  ylab('Fraction with air conditioining')

### Visualize probability of air conditioning using bins
## Calculate predicted probability of air conditioning
data <- data %>% 
  mutate(prob_ac_logit = exp(utility_ac_logit) / 
           (1 + exp(utility_ac_logit)))
## Plot fraction vs. probability of air conditioning using bins
data %>% 
  mutate(bin = cut(prob_ac_logit,
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

### Calculate marginal effects and elasticities
## Calculate the average marginal effect of each cost variable
coef(reg_logit)[2:3] *
  mean(data$prob_ac_logit * (1 - data$prob_ac_logit))
## Calculate the elasticity of each cost variable
coef(reg_logit)[2:3] *
  c(mean(data$cost_system * (1 - data$prob_ac_logit)),
    mean(data$cost_operating * (1 - data$prob_ac_logit)))

### Visualize income variable
## Plot kernel density of income
data %>% 
  ggplot(aes(x = income)) +
  geom_density() +
  xlab('Income') +
  ylab('Kernel density')

### Model air conditioning with heterogeneous cost coefficients
## Regress air conditioning on costs divided by income
reg_logit_income <- data %>% 
  glm(formula = air_conditioning ~ I(cost_system / income) + 
        I(cost_operating / income),
      family = 'binomial')
## Summarize regression results
reg_logit_income %>% 
  summary()

### Calculate marginal effects and elasticities
## Calculate predicted utility and probability
data <- data %>% 
  mutate(utility_ac_logit_income = predict(reg_logit_income),
         prob_ac_logit_income = exp(utility_ac_logit_income) / 
                                      (1 + exp(utility_ac_logit_income)))
## Calculate the average marginal effect of each cost variable
coef(reg_logit_income)[2:3] *
  mean(data$prob_ac_logit_income * (1 - data$prob_ac_logit_income) / 
         data$income)
## Calculate the average elasticity of each cost variable
coef(reg_logit_income)[2:3] *
  c(mean(data$cost_system * (1 - data$prob_ac_logit_income) / 
           data$income),
    mean(data$cost_operating * (1 - data$prob_ac_logit_income) / 
           data$income))
