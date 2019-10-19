### ResEcon 703: Topics in Advanced Econometrics
### Lecture 14: Generalized Extreme Value Models II
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

### Generalized extereme value models using the mlogit package
## Help file for the mlogit function
?mlogit
## Arguments for mlogit GEV functionality
mlogit(formula, data, reflevel, nests, un.nest.el, heterosc, ...)

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
  select(id, alt, choice, ic, oc, income)
## Look at cleaned dataset
as_tibble(hvac_clean)
## Convert cleaned dataset to mlogit format
hvac_mlogit <- mlogit.data(hvac_clean, shape = 'long', 
                           choice = 'choice', alt.var = 'alt')
## Look at data in mlogit format
as_tibble(hvac_mlogit)

### Model HVAC choice as a nested logit
## Model choice using alternative intercepts, cost data, and nests for  
## cooling vs. heating-only with unequal correlations
model_1 <- hvac_mlogit %>% 
  mlogit(formula = choice ~ ic + oc | 1 | 0, data = ., reflevel = 'hpc',
         nests = list(cooling = c('ecc', 'erc', 'gcc', 'hpc'), 
                      heating_only = c('ec', 'er', 'gc')),
         un.nest.el = FALSE)
## Summarize model results
model_1 %>% 
  summary()

### Model HVAC choice as a nested logit with equal correlations
## Model choice using alternative intercepts, cost data, and nests for 
## cooling vs. heating-only with a common correlation
model_2 <- hvac_mlogit %>% 
  mlogit(formula = choice ~ ic + oc | 1 | 0, data = ., reflevel = 'hpc',
         nests = list(cooling = c('ecc', 'erc', 'gcc', 'hpc'), 
                      heating_only = c('ec', 'er', 'gc')),
         un.nest.el = TRUE)
## Summarize model results
model_2 %>% 
  summary()

### Test that the nest correlations are equal
## Conduct a likelihood ratio test of the first two models
lrtest(model_1, model_2)

### Model HVAC choice as a nested logit
## Model choice using alternative intercepts, cost data, and nests for  
## electric vs. gas
model_3 <- hvac_mlogit %>% 
  mlogit(formula = choice ~ ic + oc | 1 | 0, data = ., reflevel = 'hpc',
         nests = list(electric = c('ec', 'ecc', 'er', 'erc', 'hpc'),
                      gas = c('gc', 'gcc')))
## Summarize model results
model_3 %>% 
  summary()

### Model HVAC choice as a paired combinatorial logit
## Model choice using alternative intercepts, cost data, and paired 
## combinatorial nests
model_4 <- hvac_mlogit %>% 
  mlogit(formula = choice ~ ic + oc | 1 | 0, data = ., reflevel = 'hpc',
         nests = 'pcl')
## Summarize model results
model_4 %>% 
  summary()

### Model HVAC choice as a paired combinatorial logit
## Model choice using alternative intercepts, cost data, and paired 
## combinatorial nests with normalized parameters
model_5 <- hvac_mlogit %>% 
  mlogit(formula = choice ~ ic + oc | 1 | 0, data = ., reflevel = 'hpc',
         nests = 'pcl', constPar = c('iv:hpc.er', 'iv:hpc.gc', 
                                     'iv:ec.erc', 'iv:ec.gcc',
                                     'iv:ecc.er', 'iv:ecc.gc',
                                     'iv:er.gc', 'iv:er.gcc',
                                     'iv:erc.gc', 'iv:erc.gcc'))
## Summarize model results
model_5 %>% 
  summary()
