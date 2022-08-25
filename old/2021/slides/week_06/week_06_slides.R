########################################################
##### ResEcon 703: Topics in Advanced Econometrics #####
#####      Create examples for week 6 slides       #####
#####         Matt Woerman, UMass Amherst          #####
########################################################

### Load packages --------------------------------------------------------------

library(tidyverse)


### Maximum Likelihood Intuition -----------------------------------------------

### Create example for maximum likelihood intuition
## Set seed for replication
set.seed(703)
## Draw five random variables from a normal (50, 5)
y <- rnorm(5, 50, 1)
## Round for easier presentation
y <- round(y, 1)
y
## Calculate likelihood of drawing these variables from 0 mean
prod(dnorm(y, 0, 1))
## Calculate likelihood of drawing these variables from 0 mean
prod(dnorm(y, 50, 1))


### Probability Density Function -----------------------------------------------

### Plot a probability density function of a standard normal distribution
## Plot density of standard normal
norm_pdf <- tibble(x = seq(-5, 5, .01), y = dnorm(x)) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  xlab(NULL) +
  ylab(NULL)
## Output density plot
ggsave('norm_pdf.pdf', norm_pdf, width = 5, height = 3)


### Maximum Likelihood Examples ------------------------------------------------

### Construct MLE example for Poisson
## Plot densities of Poisson distributions
pois_1_pmf <- tibble(x = 0:12, y = dpois(x, 1)) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  scale_x_continuous(name = NULL, limits = c(0, 12), breaks = seq(0, 12, 2)) +
  scale_y_continuous(name = NULL, limits = c(0, .37), breaks = seq(0, .3, .1))
pois_3_pmf <- tibble(x = 0:12, y = dpois(x, 3)) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  scale_x_continuous(name = NULL, limits = c(0, 12), breaks = seq(0, 12, 2)) +
  scale_y_continuous(name = NULL, limits = c(0, .37), breaks = seq(0, .3, .1))
pois_5_pmf <- tibble(x = 0:12, y = dpois(x, 5)) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  scale_x_continuous(name = NULL, limits = c(0, 12), breaks = seq(0, 12, 2)) +
  scale_y_continuous(name = NULL, limits = c(0, .37), breaks = seq(0, .3, .1))
## Output density plot
ggsave('pois_1_pmf.pdf', pois_1_pmf, width = 3, height = 3)
ggsave('pois_3_pmf.pdf', pois_3_pmf, width = 3, height = 3)
ggsave('pois_5_pmf.pdf', pois_5_pmf, width = 3, height = 3)
## Draw from a Poisson distribution with lambda = 2
y_pois <- rpois(10, 2)
y_pois
## Estimate lambda as the mean
mean(y_pois)
## Calculate components of likelihood and log-likelihood functions
sum(y_pois)
prod(factorial(y_pois))
sum(log(factorial(y_pois)))
## Function to calculate Poisson likelihood for a parameter
pois_like <- function(data, parameter){
  output <- dpois(data, parameter) %>% 
    prod()
  return(output)
}
## Function to calculate Poisson log-likelihood for a parameter
pois_ll <- function(data, parameter){
  output <- dpois(data, parameter) %>% 
    log() %>% 
    sum()
  return(output)
}
## Calculate likelihood for a range of lambda parameters
poisson_fit <- tibble(parameter = seq(0, 3, 0.01)) %>% 
  mutate(like = map_dbl(.x = .$parameter,
                        .f = pois_like,
                        data = y_pois),
         ll = map_dbl(.x = .$parameter,
                      .f = pois_ll,
                      data = y_pois))
## Plot likelihood
like_plot <- poisson_fit %>% 
  ggplot(aes(x = parameter, y = like)) + 
  geom_line() +
  xlab('Parameter') +
  ylab('Likelihood') +
  geom_vline(xintercept = 1.3, linetype = 'dashed')
## Save likelihood plot
ggsave('likelihood.pdf', like_plot, width = 3, height = 3)
## Plot log-likelihood
ll_plot <- poisson_fit %>% 
  ggplot(aes(x = parameter, y = ll)) + 
  geom_line() +
  xlab('Parameter') +
  ylab('Log-likelihood') +
  geom_vline(xintercept = 1.3, linetype = 'dashed')
## Save likelihood plot
ggsave('log_likelihood.pdf', ll_plot, width = 3, height = 3)

### Construct MLE example for normal
## Draw from a normal distribution with (5, 2)
y_norm <- rnorm(5, 5, 2)
round(y_norm, 2)
## Estimate mu as the mean
mean(y_norm)
## Estimate sigma^2 as the sample variance
mean((y_norm - mean(y_norm))^2)