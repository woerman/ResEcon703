### ResEcon 703: Topics in Advanced Econometrics
### Lecture 6: Maximum Likelihood Estimation
### Matt Woerman, UMass Amherst

### Load packages --------------------------------------------------------------
library(tidyverse)

### Likelihood and Log-Likelihood Functions ------------------------------------

### Plot likelihood and log-likehood for poisson parameters
## Set seed for randomization
set.seed(321)
## Draw 10 random variables from a poisson distribution with lambda 5
poisson_data <- rpois(10, 5)
## Function to calculate poisson likelihood for a parameter
poisson_likelihood <- function(data, parameter){
  output <- dpois(data, parameter) %>% 
    prod()
  return(output)
}
## Function to calculate poisson log-likelihood for a parameter
poisson_log_likelihood <- function(data, parameter){
  output <- dpois(data, parameter) %>% 
    log() %>% 
    sum()
  return(output)
}
## Calculate likelihood for a range of lambda parameters
poisson_fit <- tibble(parameter = seq(2, 8, 0.01)) %>% 
  mutate(likelihood = map_dbl(.x = .$parameter,
                              .f = poisson_likelihood,
                              data = poisson_data),
         log_likelihood = map_dbl(.x = .$parameter,
                                  .f = poisson_log_likelihood,
                                  data = poisson_data))
## Plot likelihood
likelihood_plot <- poisson_fit %>% 
  ggplot(aes(x = parameter, y = likelihood)) + 
  geom_line() +
  xlab('Parameter') +
  ylab('Likelihood') +
  geom_vline(xintercept = 5.3, linetype = 'dashed')
## Save likelihood plot
ggsave('likelihood.pdf', likelihood_plot, width = 3, height = 3)
## Plot log-likelihood
log_likelihood_plot <- poisson_fit %>% 
  ggplot(aes(x = parameter, y = log_likelihood)) + 
  geom_line() +
  xlab('Parameter') +
  ylab('Log-likelihood') +
  geom_vline(xintercept = 5.3, linetype = 'dashed')
## Save likelihood plot
ggsave('log_likelihood.pdf', log_likelihood_plot, width = 3, height = 3)
