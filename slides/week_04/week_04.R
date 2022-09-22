########################################################
##### ResEcon 703: Topics in Advanced Econometrics #####
#####             Week 4: Logit Model              #####
#####         Matt Woerman, UMass Amherst          #####
########################################################

### Binary Logit Model R Example -----------------------------------------------

### Load and look at dataset
## Load tidyverse
library(tidyverse)
## Load dataset
ac_data <- read_csv('ac_renters.csv')
## Look at dataset
ac_data

### Model air conditioning as a binary logit
## Model air conditioning as a function of cost variables
binary_logit <- glm(formula = 
                      air_conditioning ~ cost_system + cost_operating, 
                    family = 'binomial', 
                    data = ac_data)
## Summarize model results
summary(binary_logit)
## Display model coefficients
coef(binary_logit)

### Calculate the fitted values of the model 
## Calculate utility of air conditioning
ac_data <- ac_data %>% 
  mutate(utility_ac_logit = predict(binary_logit))
## Look at utilities and other data
ac_data %>% 
  select(air_conditioning, starts_with('cost'), utility_ac_logit)

### Visualize utility of air conditioning adoption
## Plot density of utilities
ac_data %>% 
  ggplot(aes(x = utility_ac_logit)) +
  geom_density() +
  xlab('Utility of air conditioning') +
  ylab('Kernel Density')
## Plot fraction vs. utility of air conditioning using bins
ac_data %>% 
  mutate(bin = cut(utility_ac_logit,
                   breaks = seq(-3, 2, 0.25),
                   labels = 1:20)) %>%
  group_by(bin) %>% 
  summarize(fraction_ac = mean(air_conditioning), .groups = 'drop') %>% 
  mutate(bin = as.numeric(bin),
         bin_mid = 0.25 * (bin - 1) - 2.875) %>% 
  ggplot(aes(x = bin_mid, y = fraction_ac)) +
  geom_point() +
  xlab('Utility of air conditioning') +
  ylab('Fraction with air conditioining')

### Calculate the choice probabilities implied by the model 
## Calculate choice probability of air conditioning
ac_data <- ac_data %>% 
  mutate(probability_ac_logit = 1 / (1 + exp(-utility_ac_logit)))
## Look at utilities and probabilities
ac_data %>% 
  select(air_conditioning, utility_ac_logit, probability_ac_logit)

### Visualize probability of air conditioning adoption
## Plot density of probabilities
ac_data %>% 
  ggplot(aes(x = probability_ac_logit)) +
  geom_density() +
  xlab('Probability of air conditioning') +
  ylab('Kernel Density')
## Plot fraction vs. probability of air conditioning using bins
ac_data %>% 
  mutate(bin = cut(probability_ac_logit,
                   breaks = seq(0, 1, 0.05),
                   labels = 1:20)) %>%
  group_by(bin) %>% 
  summarize(fraction_ac = mean(air_conditioning), .groups = 'drop') %>% 
  mutate(bin = as.numeric(bin),
         bin_mid = 0.05 * (bin - 1) + 0.025) %>% 
  ggplot(aes(x = bin_mid, y = fraction_ac)) +
  geom_point() +
  xlab('Probability of air conditioning') +
  ylab('Fraction with air conditioning')

### Calculate marginal effects and elasticities
## Calculate the marginal effect of each cost variable
ac_data <- ac_data %>% 
  mutate(marg_eff_system = coef(binary_logit)[2] * 
           probability_ac_logit * (1 - probability_ac_logit),
         marg_eff_operating = coef(binary_logit)[3] * 
           probability_ac_logit * (1 - probability_ac_logit))
## Calculate the elasticity of each cost variable
ac_data <- ac_data %>% 
  mutate(elasticity_system = coef(binary_logit)[2] * 
           cost_system * (1 - probability_ac_logit),
         elasticity_operating = coef(binary_logit)[3] * 
           cost_operating * (1 - probability_ac_logit))
## Look at marginal effects and elasticities
ac_data %>% 
  select(starts_with('marg_eff'), starts_with('elasticity'))
## Summarize marginal effects and elasticities
ac_data %>% 
  select(starts_with('marg_eff'), starts_with('elasticity')) %>% 
  summary()

### Model air conditioning with heterogeneous cost coefficients
## Model air conditioning as a function of costs divided by income
binary_logit_inc <- glm(formula = air_conditioning ~ I(cost_system / income) + 
                          I(cost_operating / income), 
                        family = 'binomial', 
                        data = ac_data)
## Summarize model results
summary(binary_logit_inc)
## Display model coefficients
coef(binary_logit_inc)

### Visualize income variable
## Plot kernel density of income
ac_data %>% 
  ggplot(aes(x = income)) +
  geom_density() +
  xlab('Income') +
  ylab('Kernel density')

### Calculate marginal utility of cost variables
## Calculate marginal utility of costs when income == 30
coef(binary_logit_inc)[2:3] / 30
## Calculate marginal utility of costs when income == 60
coef(binary_logit_inc)[2:3] / 60
## Calculate marginal utility of costs when income == 90
coef(binary_logit_inc)[2:3] / 90

### Calculate the trade-off between system cost and operating cost
## Calculate system cost equivalence of an increase in operating cost
-coef(binary_logit)[3] / coef(binary_logit)[2]

### Calculate the implied discount factor of consumers
## Calculate the implied discount rate
coef(binary_logit)[2] / coef(binary_logit)[3]


### Multinomial Logit Model R Example ------------------------------------------

### Load and look at dataset
## Load tidyverse and mlogit
library(tidyverse)
library(mlogit)
## Load dataset from mlogit package
data('Heating', package = 'mlogit')
## Rename dataset to lowercase
heating <- Heating
## Look at dataset
tibble(heating)
## Pivot into a long dataset
heating_long <- heating %>% 
  pivot_longer(contains('.')) %>% 
  separate(name, c('name', 'alt')) %>% 
  pivot_wider() %>% 
  mutate(choice = (depvar == alt)) %>% 
  select(-depvar)
## Look at long dataset
tibble(heating_long)

### Convert data to dfidx formats
## Convert wide data to dfidx format
heating_dfidx <- dfidx(heating, shape = 'wide', 
                       choice = 'depvar', varying = 3:12)
## Convert long data to dfidx format
heating_long_dfidx <- dfidx(heating_long, shape = 'long', 
                            choice = 'choice', idx = c('idcase', 'alt'))
## Look at wide data in dfidx format
tibble(heating_dfidx)
## Look at long data in dfidx format
tibble(heating_long_dfidx)

### Model heating choice as a multinomial logit
## Model choice using cost data and alternative effects
model_mlogit <- mlogit(formula = depvar ~ ic + oc | 1 | 0, 
                       data = heating_dfidx, 
                       reflevel = 'hp')
## Summarize model results
summary(model_mlogit)
## Display model coefficients
coef(model_mlogit)

### Model heating choice with alternative-specific demographic parameters
## Model heating choice with alternative-specific rooms coefficient
model_mlogit_rooms <- mlogit(formula = depvar ~ ic + oc | rooms | 0, 
                             data = heating_dfidx, 
                             reflevel = 'hp')
## Summarize model results
summary(model_mlogit_rooms)

### Model heating choice with alternative-specific cost parameters
## Model heating choice with alternative-specific ic coefficient
model_mlogit_costs <- mlogit(formula = depvar ~ oc | 1 | ic, 
                             data = heating_dfidx, 
                             reflevel = 'hp')
## Summarize model results
summary(model_mlogit_costs)

### Calculate the fitted values of the model
## Look at fitted utilities
fitted(model_mlogit, type = 'linpred') %>% 
  head()
## Look at fitted choice probabilities
fitted(model_mlogit, type = 'probabilities') %>% 
  head()

### Calculate and visualize own-price marginal effects for central gas
## Calculate probability of central gas
heating <- heating %>% 
  mutate(prob_gc_mlogit = 
           fitted(model_mlogit, type = 'probabilities')[, 4])
## Calculate own-price marginal effects for central gas
heating <- heating %>% 
  mutate(mfx_gc_ic_mlogit = 
           coef(model_mlogit)[5] * prob_gc_mlogit * (1 - prob_gc_mlogit),
         mfx_gc_oc_mlogit = 
           coef(model_mlogit)[6] * prob_gc_mlogit * (1 - prob_gc_mlogit))
## Plot kernel density of own-price elasticity of ic for gc
heating %>% 
  ggplot(aes(x = mfx_gc_ic_mlogit)) +
  geom_density() +
  xlab('Own-price marginal effect of central gas installation cost') +
  ylab('Kernel density')
## Plot kernel density of own-price elasticity of oc for gc
heating %>% 
  ggplot(aes(x = mfx_gc_oc_mlogit)) +
  geom_density() +
  xlab('Own-price marginal effect of central gas operating cost') +
  ylab('Kernel density')

### Calculate and visualize own-price elasticities for central gas
## Calculate own-price elasticities for central gas
heating <- heating %>% 
  mutate(elas_gc_ic_mlogit = 
           coef(model_mlogit)[5] * ic.gc * (1 - prob_gc_mlogit),
         elas_gc_oc_mlogit = 
           coef(model_mlogit)[6] * oc.gc * (1 - prob_gc_mlogit))
## Plot kernel density of own-price elasticity of ic for gc
heating %>% 
  ggplot(aes(x = elas_gc_ic_mlogit)) +
  geom_density() +
  xlab('Own-price elasticity of central gas installation cost') +
  ylab('Kernel density')
## Plot kernel density of own-price elasticity of oc for gc
heating %>% 
  ggplot(aes(x = elas_gc_oc_mlogit)) +
  geom_density() +
  xlab('Own-price elasticity of central gas operating cost') +
  ylab('Kernel density')

### Calculate and visualize cross-price elasticities for central gas costs
## Calculate own-price elasticities for central gas
heating <- heating %>% 
  mutate(crosselas_gc_ic_mlogit = 
           -coef(model_mlogit)[5] * ic.gc * prob_gc_mlogit,
         crosselas_gc_oc_mlogit = 
           -coef(model_mlogit)[6] * oc.gc * prob_gc_mlogit)
## Plot kernel density of cross-price elasticity of ic for gc
heating %>% 
  ggplot(aes(x = crosselas_gc_ic_mlogit)) +
  geom_density() +
  xlab('Cross-price elasticity of central gas installation cost') +
  ylab('Kernel density')
## Plot kernel density of cross-price elasticity of oc for gc
heating %>% 
  ggplot(aes(x = crosselas_gc_oc_mlogit)) +
  geom_density() +
  xlab('Cross-price elasticity of central gas operating cost') +
  ylab('Kernel density')

### Calculate marginal effects and elasticities of oc at data means
## Calculate marginal effects of ic at data means
effects(model_mlogit, covariate = 'ic', type = 'aa')
## Calculate elasticities of ic at data means
effects(model_mlogit, covariate = 'ic', type = 'rr')

### Model heating choice with heterogeneous cost coefficients
## Model heating choice with costs divided by income
model_mlogit_inc <- mlogit(formula = depvar ~ 
                             I(ic / income) + I(oc / income) | 1 | 0, 
                           data = heating_dfidx, 
                           reflevel = 'hp')
## Summarize model results
summary(model_mlogit_inc)
## Display model coefficients
coef(model_mlogit_inc)

### Visualize income variable
## Plot kernel density of income
heating %>% 
  ggplot(aes(x = income)) +
  geom_density() +
  xlab('Income') +
  ylab('Kernel density')

### Calculate marginal utility of cost variables
## Calculate marginal utility of costs when income == 3
coef(model_mlogit_inc)[5:6] / 3
## Calculate marginal utility of costs when income == 5
coef(model_mlogit_inc)[5:6] / 5
## Calculate marginal utility of costs when income == 7
coef(model_mlogit_inc)[5:6] / 7

### Model heating choice with regional scale parameters
## Create new dataset to work around bug
heating_nas <- heating_long %>%
  mutate(available = 1)
heating_nas <- heating_nas %>%
  bind_rows(heating_nas %>%
              filter(idcase == 1) %>%
              mutate(idcase = 9999) %>%
              mutate(available = 1 * choice))
heating_nas_dfidx <- dfidx(heating_nas, shape = 'long', 
                           idx = c('idcase', 'alt'), choice = 'choice',
                           subset = available == 1)
## Model heating choice with regional scale parameters
model_mlogit_region <- mlogit(formula = 
                                choice ~ ic + oc | 1 | 0 | region,
                              data = heating_nas_dfidx,
                              reflevel = 'hp')
## Summarize model results
summary(model_mlogit_region)
## Display model coefficients
coef(model_mlogit_region)

### Calculate the trade-off between installation cost and operating cost
## Calculate ic equivalence of an increase in oc
-coef(model_mlogit)[6] / coef(model_mlogit)[5]

### Calculate the implied discount factor of consumers
## Calculate the implied discount rate
coef(model_mlogit)[5] / coef(model_mlogit)[6]

### Calculate aggregate choice effects from a heat pump subsidy
## Create data with 50% heat pump subsidy
heating_subsidy <- Heating %>% 
  mutate(ic.hp = 0.5 * ic.hp)
## Convert subsidy data to dfidx format
heating_subsidy_dfidx <- dfidx(heating_subsidy, shape = 'wide', 
                               choice = 'depvar', varying = 3:12)
## Look at subsidy dataset
tibble(heating_subsidy_dfidx)
## Calculate aggregate choices using observed data
agg_choices_obs <- predict(model_mlogit, 
                           newdata = heating_dfidx) %>% 
  colSums()
## Calculate aggregate choices using subsidy data
agg_choices_subsidy <- predict(model_mlogit, 
                               newdata = heating_subsidy_dfidx) %>% 
  colSums()
## Calculate difference between aggregate choices in levels
agg_choices_subsidy - agg_choices_obs
## Calculate difference between aggregate choices in percentages
(agg_choices_subsidy - agg_choices_obs) / agg_choices_obs

### Calculate consumer surplus effects from a heat pump subsidy
## Calculate total log-sum value using observed data
logsum_obs <- logsum(model_mlogit, data = heating_dfidx) %>% 
  sum()
## Calculate total log-sum value using subsidy data
logsum_subsidy <- logsum(model_mlogit, data = heating_subsidy_dfidx) %>% 
  sum()
## Calculate change in consumer surplus from subsidy
(logsum_subsidy - logsum_obs) / (-coef(model_mlogit)[5])
