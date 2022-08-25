########################################################
##### ResEcon 703: Topics in Advanced Econometrics #####
#####          Week 10: Mixed Logit Model          #####
#####         Matt Woerman, UMass Amherst          #####
########################################################

### Mixed Logit Model R Example ------------------------------------------------

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

### Mixed logit model using the mlogit package
## Help file for the mlogit function
?mlogit
## Arguments for mlogit mixed logit functionality
mlogit(formula, data, reflevel, rpar, correlation, R, seed, ...)

### Model HVAC choice as a mixed logit
## Model choice using alt intercepts and cost data with normal coefs
model_2 <- mlogit(formula = depvar ~ ic + oc | 1 | 0, 
                  data = hvac_dfidx, 
                  reflevel = 'hpc',
                  rpar = c(ic = 'n', oc = 'n'), 
                  R = 1000, seed = 703)
## Summarize model results
summary(model_2)
## Display model coefficients
coef(model_2)
## Conduct likelihood ratio test of models 1 and 2
lrtest(model_1, model_2)
## Plot distributions of random coefficients
ggplot(data = data.frame(x = c(-8, 1)), aes(x)) +
  stat_function(fun = dnorm, n = 1001, 
                args = list(mean = coef(model_2)[7], 
                            sd = abs(coef(model_2)[9]))) +
  stat_function(fun = dnorm, n = 1001, 
                args = list(mean =coef(model_2)[8], 
                            sd = abs(coef(model_2)[10])), 
                linetype = 'dashed') +
  xlab(NULL) +
  ylab(NULL)

### Model HVAC choice as a mixed logit
## Model choice using alt intercepts and cost data with normal oc coef
model_3 <- mlogit(formula = depvar ~ ic + oc | 1 | 0, 
                  data = hvac_dfidx, 
                  reflevel = 'hpc',
                  rpar = c(oc = 'n'), 
                  R = 1000, seed = 703)
## Summarize model results
summary(model_3)
## Display model coefficients
coef(model_3)
## Conduct likelihood ratio test of models 2 and 3
lrtest(model_2, model_3)
## Calculate ic equivalence of an increase in oc
c(-coef(model_3)[8] / coef(model_3)[7], 
  abs(coef(model_3)[9] / coef(model_3)[7]))
## Plot distributions of ic equivalence
ggplot(data = data.frame(x = c(-18, 2)), aes(x)) +
  stat_function(fun = dnorm, n = 1001, 
                args = list(mean = -coef(model_3)[8] / coef(model_3)[7], 
                            sd = abs(coef(model_3)[9] / 
                                       coef(model_3)[7]))) +
  xlab(NULL) +
  ylab(NULL)
