### ResEcon 703: Topics in Advanced Econometrics
### Lecture 19: Individual-Specific Parameters I
### Matt Woerman, UMass Amherst

### Load packages --------------------------------------------------------------
library(tidyverse)

### Example of Individual-Specific Parameters ----------------------------------

### Plot unconditional and conditional distributions
## Plot unconditional distribution of driving preferences, N(3, 2)
plot <- ggplot(data = data.frame(x = c(-5, 10)), aes(x)) +
  stat_function(fun = dnorm, n = 1001, 
                args = list(mean = 3, 
                            sd = 2))
## Plot conditional distribution for drivers, N(5, 1.2)
plot <- plot +
  stat_function(fun = dnorm, n = 1001, 
                args = list(mean = 5, 
                            sd = 1.2), 
                linetype = 'dashed')
## Plot conditional distribution for non-drivers, N(0, 1.5)
plot <- plot +
  stat_function(fun = dnorm, n = 1001, 
                args = list(mean = 0, 
                            sd = 1.5),
                linetype = 'dotted')
## Format plot
plot <- plot +
  xlab(NULL) +
  ylab(NULL)
## Display plot of unconditional and conditional distributions
plot
