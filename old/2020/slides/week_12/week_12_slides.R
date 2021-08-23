########################################################
##### ResEcon 703: Topics in Advanced Econometrics #####
#####      Create figures for week 12 slides       #####
#####         Matt Woerman, UMass Amherst          #####
########################################################

### Load packages --------------------------------------------------------------
library(tidyverse)


### Example of Individual-Specific Parameters ----------------------------------

### Plot unconditional distribution
## Plot unconditional distribution of driving preferences, N(3, 2)
plot_uncond <- ggplot(data = data.frame(x = c(-5, 10)), aes(x)) +
  stat_function(fun = dnorm, n = 1001, 
                args = list(mean = 3, 
                            sd = 2)) +
  xlab(NULL) +
  ylab(NULL)
## Output distribution plot
ggsave('uncond_dist.pdf', plot_uncond, width = 5, height = 3)

### Plot unconditional and conditional distributions
## Plot unconditional distribution of driving preferences, N(3, 2)
plot_all <- ggplot(data = data.frame(x = c(-5, 10)), aes(x)) +
  stat_function(fun = dnorm, n = 1001, 
                args = list(mean = 3, 
                            sd = 2))
## Plot conditional distribution for drivers, N(5, 1.2)
plot_all <- plot_all +
  stat_function(fun = dnorm, n = 1001, 
                args = list(mean = 5, 
                            sd = 1.2), 
                linetype = 'dashed')
## Plot conditional distribution for non-drivers, N(0, 1.5)
plot_all <- plot_all +
  stat_function(fun = dnorm, n = 1001, 
                args = list(mean = 0, 
                            sd = 1.5),
                linetype = 'dotted')
## Format plot
plot_all <- plot_all +
  xlab(NULL) +
  ylab(NULL)
## Output distribution plot
ggsave('all_dist.pdf', plot_all, width = 5, height = 3)
