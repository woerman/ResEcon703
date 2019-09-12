### ResEcon 703: Topics in Advanced Econometrics
### Lecture 5: Logit Model II
### Matt Woerman, UMass Amherst

### Multinomial Logit Model Example in R ---------------------------------------

### Load and look at dataset
## Load tidyverse and mlogit
library(tidyverse)
library(mlogit)
## Load dataset from mlogit package
data('Heating', package = 'mlogit')
## Look at dataset
as_tibble(Heating)
## Gather into a long dataset
heating_long <- Heating %>% 
  gather(key, value, starts_with('ic.'), starts_with('oc.')) %>% 
  separate(key, c('cost', 'alt')) %>% 
  spread(cost, value) %>% 
  mutate(choice = (depvar == alt)) %>% 
  select(-depvar)
## Look at long dataset
as_tibble(heating_long)

### Convert data to mlogit format
## Convert wide data to mlogit format
heating_mlogit <- mlogit.data(Heating, shape = 'wide', 
                              choice = 'depvar', varying = 3:12)
## Convert long data to mlogit format
heating_long_mlogit <- mlogit.data(heating_long, shape = 'long', 
                                   choice = 'choice', alt.var = 'alt')
## Look at wide data in mlogit format
as_tibble(heating_mlogit)
## Look at long data in mlogit format
as_tibble(heating_long_mlogit)

### Model heating choice as a multinomial logit
## Model choice using cost data and alternative effects
model_mlogit <- heating_mlogit %>% 
  mlogit(formula = depvar ~ ic + oc | 1 | 0, data = ., reflevel = 'hp')
## Summarize model results
model_mlogit %>% 
  summary()

### Calculate mean own-price elasticities for central gas
## Calculate probability of central gas
Heating <- Heating %>% 
  mutate(prob_gc_mlogit = fitted(model_mlogit, 
                                 type = 'probabilities')[, 4])
## Calculate mean own-price elasticities
coef(model_mlogit)[5:6] *
  c(mean(Heating$ic.gc * (1 - Heating$prob_gc_mlogit)),
    mean(Heating$oc.gc * (1 - Heating$prob_gc_mlogit)))

### Visualize distibution of own-price elasticity of ic for gc
## Calculate and plot density of own-price elasticity of ic for gc
Heating %>% 
  mutate(elasticity = coef(model_mlogit)[5] * ic.gc * 
           (1 - prob_gc_mlogit)) %>% 
  ggplot(aes(x = elasticity)) +
  geom_density() +
  xlab('Own-price elasticity of central gas installation cost') +
  ylab('Kernel density')

### Calculate mean cross-price elasticities of ec with respect to gc
## Calculate mean cross-price elasticity
-coef(model_mlogit)[5:6] *
  c(mean(Heating$ic.gc * Heating$prob_gc_mlogit),
    mean(Heating$oc.gc * Heating$prob_gc_mlogit))

### Calculate the tradeoff between installation cost and operating cost
## Calculate install cost equivalence of an increse in operating cost
-coef(model_mlogit)[6] / coef(model_mlogit)[5]

### Calculte the implied discount rate of consumers
## Calculate the implied discount in perpetuity
coef(model_mlogit)[5] / coef(model_mlogit)[6]

### Model heating choice with heterogeneous cost coefficients
## Model heating choice with costs divided by income
model_mlogit_income <- heating_mlogit %>% 
  mlogit(formula = depvar ~ I(ic / income) + I(oc / income) | 1 | 0, 
         data = ., reflevel = 'hp')
## Summarize model results
model_mlogit_income %>% 
  summary()

### Model heating choice with alternative-specific demographics
## Model heating choice with alternative-specific rooms coefficient
model_mlogit_rooms <- heating_mlogit %>% 
  mlogit(formula = depvar ~ ic + oc | rooms | 0, data = ., 
         reflevel = 'hp')
## Summarize model results
model_mlogit_rooms %>% 
  summary()

### Model heating choice with alternative-specific costs
## Model heating choice with alternative-specific cost coefficient
model_mlogit_costs <- heating_mlogit %>% 
  mlogit(formula = depvar ~ oc | 1 | ic, data = ., reflevel = 'hp')
## Summarize model results
model_mlogit_costs %>% 
  summary()
