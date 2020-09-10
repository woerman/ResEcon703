########################################################
##### ResEcon 703: Topics in Advanced Econometrics #####
#####       Create figures for week 4 slides       #####
#####         Matt Woerman, UMass Amherst          #####
########################################################

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
## Plot distribution of type I extreme value
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