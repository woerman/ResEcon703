########################################################
##### ResEcon 703: Topics in Advanced Econometrics #####
#####   Week 9: Generalized Extreme Value Models   #####
#####         Matt Woerman, UMass Amherst          #####
########################################################

### Generalized Extreme Value Models R Example ---------------------------------

### Load and look at dataset
## Load tidyverse and mlogit
library(tidyverse)
library(mlogit)
## Load dataset from mlogit package
data('HC', package = 'mlogit')
## Look at dataset
tibble(HC)

### Format dataset
## Combine heating and cooling costs into one variable
hvac_clean <- HC %>% 
  mutate(id = 1:n(),
         ic.gcc = ich.gcc + icca, ic.ecc = ich.ecc + icca,
         ic.erc = ich.erc + icca, ic.hpc = ich.hpc + icca,
         ic.gc = ich.gc, ic.ec = ich.ec, ic.er = ich.er,
         oc.gcc = och.gcc + occa, oc.ecc = och.ecc + occa,
         oc.erc = och.erc + occa, oc.hpc = och.hpc + occa,
         oc.gc = och.gc, oc.ec = och.ec, oc.er = och.er) %>%
  select(id, depvar, starts_with('ic.'), starts_with('oc.'), income)
## Look at cleaned dataset
tibble(hvac_clean)
## Convert cleaned dataset to dfidx format
hvac_dfidx <- dfidx(hvac_clean, shape = 'wide', 
                    choice = 'depvar', varying = 3:16)
## Look at data in dfidx format
tibble(hvac_dfidx)

### Model HVAC choice as a multinomial logit
## Model choice using alternative intercepts and cost data
model_1 <- mlogit(formula = depvar ~ ic + oc | 1 | 0,
                  data = hvac_dfidx,
                  reflevel = 'hpc')
## Summarize model results
summary(model_1)

### Nested logit models using the mlogit package
## Help file for the mlogit function
?mlogit
## Arguments for mlogit GEV functionality
mlogit(formula, data, reflevel, nests, ...)

### Model HVAC choice as a nested logit with cooling vs. heating-only nests
## Model choice with cooling vs. heating-only nests
model_2 <- mlogit(formula = depvar ~ ic + oc | 1 | 0, 
                  data = hvac_dfidx,
                  reflevel = 'hpc',
                  nests = list(cooling = c('ecc', 'erc', 'gcc', 'hpc'), 
                               heating_only = c('ec', 'er', 'gc')))
## Summarize model results
summary(model_2)
## Display model coefficients
coef(model_2)
## Conduct likelihood ratio test of models 1 and 2
lrtest(model_1, model_2)

### Model HVAC choice as a nested logit with electric vs. gas nests
## Model choice with electric vs. gas nests
model_3 <- mlogit(formula = depvar ~ ic + oc | 1 | 0, 
                  data = hvac_dfidx,
                  reflevel = 'hpc',
                  nests = list(electric = c('ec', 'ecc', 'er', 
                                            'erc', 'hpc'),
                               gas = c('gc', 'gcc')))
## Summarize model results
summary(model_3)
## Display model coefficients
coef(model_3)
## Conduct likelihood ratio test of models 1 and 3
lrtest(model_1, model_3)