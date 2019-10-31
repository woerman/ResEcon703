### ResEcon 703: Topics in Advanced Econometrics
### Lecture 16: Mixed Logit Model II
### Matt Woerman, UMass Amherst

### Generalized Extreme Value Model Example in R -------------------------------

### Load and look at dataset
## Load tidyverse and mlogit
library(tidyverse)
library(mlogit)
## Load dataset from mlogit package
data('HC', package = 'mlogit')
## Look at dataset
as_tibble(HC)

### Format dataset
## Gather into a long dataset
hvac_long <- HC %>% 
  mutate(id = 1:n()) %>% 
  gather(key, value, starts_with('ich.'), starts_with('och.')) %>% 
  separate(key, c('cost', 'alt')) %>% 
  spread(cost, value) %>% 
  mutate(choice = (depvar == alt)) %>% 
  select(-depvar)
## Look at long dataset
as_tibble(hvac_long)
## Combine heating and cooling costs into one variable
hvac_clean <- hvac_long %>% 
  mutate(cooling = (nchar(alt) == 3), 
         ic = if_else(cooling, ich + icca, ich),
         oc = if_else(cooling, och + occa, och)) %>% 
  mutate(cooling = 1 * cooling) %>% 
  select(id, alt, choice, cooling, ic, oc, income)
## Look at cleaned dataset
as_tibble(hvac_clean)
## Convert cleaned dataset to mlogit format
hvac_mlogit <- mlogit.data(hvac_clean, shape = 'long', 
                           choice = 'choice', alt.var = 'alt')
## Look at data in mlogit format
as_tibble(hvac_mlogit)

### Mixed logit model using the mlogit package
## Help file for the mlogit function
?mlogit
## Arguments for mlogit mixed logit functionality
mlogit(formula, data, reflevel, rpar, correlation, R, seed, ...)

### Model HVAC choice as a mixed logit
## Model choice using alternative intercepts and cost data with normal 
## coefficients
model_1 <- hvac_mlogit %>% 
  mlogit(formula = choice ~ ic + oc | 1 | 0, data = ., reflevel = 'hpc',
         rpar = c(ic = 'n', oc = 'n'), R = 1000, seed = 321)
## Summarize model results
model_1 %>% 
  summary()
## Plot distributions of random coefficients
ggplot(data = data.frame(x = c(-8, 1)), aes(x)) +
  stat_function(fun = dnorm, n = 1001, 
                args = list(mean = model_1$coefficients[7], 
                            sd = abs(model_1$coefficients[9]))) +
  stat_function(fun = dnorm, n = 1001, 
                args = list(mean = model_1$coefficients[8], 
                            sd = abs(model_1$coefficients[10])), 
                linetype = 'dashed') +
  xlab(NULL) +
  ylab(NULL)

### Model HVAC choice as a mixed logit
## Model choice using alternative intercepts and cost data with 
## log-normal coefficients
model_2 <- hvac_mlogit %>% 
  mlogit(formula = choice ~ ic + oc | 1 | 0, data = ., reflevel = 'hpc',
         rpar = c(ic = 'ln', oc = 'ln'), R = 1000, seed = 321)

### Reformat dataset with negative costs
## Convert cleaned dataset to mlogit format with negative costs
hvac_mlogit_neg <- mlogit.data(hvac_clean, shape = 'long', 
                               choice = 'choice', alt.var = 'alt',
                               opposite = c('ic', 'oc'))
## Look at data in mlogit format
as_tibble(hvac_mlogit_neg)

### Model HVAC choice as a mixed logit
## Model choice using alternative intercepts and cost data with 
## log-normal coefficients
model_2 <- hvac_mlogit_neg %>% 
  mlogit(formula = choice ~ ic + oc | 1 | 0, data = ., reflevel = 'hpc',
         rpar = c(ic = 'ln', oc = 'ln'), R = 1000, seed = 321)
## Summarize model results
model_2 %>% 
  summary()
## Plot distributions of random coefficients
ggplot(data = data.frame(x = c(0, 10)), aes(x)) +
  stat_function(fun = dlnorm, n = 1001, 
                args = list(mean = model_2$coefficients[7], 
                            sd = abs(model_2$coefficients[9]))) +
  stat_function(fun = dlnorm, n = 1001, 
                args = list(mean = model_2$coefficients[8], 
                            sd = abs(model_2$coefficients[10])), 
                linetype = 'dashed') +
  xlab(NULL) +
  ylab(NULL)

### Model HVAC choice as a mixed logit
## Model choice using alternative intercepts and cost data with 
## correlated log-normal coefficients
model_3 <- hvac_mlogit_neg %>% 
  mlogit(formula = choice ~ ic + oc | 1 | 0, data = ., reflevel = 'hpc',
         rpar = c(ic = 'ln', oc = 'ln'), correlation = TRUE, R = 1000, 
         seed = 321)
## Summarize model results
model_3 %>% 
  summary()
## Calculate coefficient variances and covariance
model_3_vcov <- c(model_3$coefficients[9]^2,
                  model_3$coefficients[10]^2 + 
                    model_3$coefficients[11]^2,
                  model_3$coefficients[9] * 
                    model_3$coefficients[10]) %>% 
  setNames(c('var.ic', 'var.oc', 'cov.ic:oc'))
model_3_vcov
## Calculate coefficient variances and covariance using vcov
vcov(model_3, what = 'rpar') %>% 
  summary()
## Plot distribution of implied discount rate
ggplot(data = data.frame(x = c(0, 0.5)), aes(x)) +
  stat_function(fun = dlnorm, n = 1001, 
                args = list(mean = model_2$coefficients[7] - 
                              model_2$coefficients[8], 
                            sd = sqrt(model_3_vcov[1] + model_3_vcov[2] - 
                                        2 * model_3_vcov[3]))) +
  xlab(NULL) +
  ylab(NULL)
