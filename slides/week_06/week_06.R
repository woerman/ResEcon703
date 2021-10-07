########################################################
##### ResEcon 703: Topics in Advanced Econometrics #####
#####    Week 6: Maximum Likelihood Estimation     #####
#####         Matt Woerman, UMass Amherst          #####
########################################################

### Maximum Likelihood Estimation R Example ------------------------------------

### Examine the mtcars dataset
## Load tidyverse
library(tidyverse)
## Look at the mtcars data
tibble(mtcars)
## Summarize the mtcars dataset
mtcars %>% 
  select(mpg, disp, hp, wt, qsec) %>% 
  summary()
## Plot the mtcars dataset
ggplot(data = mtcars, mapping = aes(x = hp, y = mpg)) +
  geom_point()

### Optimization in R
## Help file for the optimization function, optim
?optim
## Arguments for optim function
optim(par, fn, gr, ..., method, lower, upper, control, hessian)

### Estimate regression by MLE using numerical optimization
## Create function to calculate OLS log-likelihood
ll_ols <- function(params, data, y_var, x_vars) {
  ## Add column of ones for the constant term
  reg_data <- data %>% 
    mutate(constant = 1)
  ## Select data for X and convert to a matrix
  X <- reg_data %>% 
    select(all_of(c('constant', x_vars))) %>% 
    as.matrix()
  ## Select data for y and convert to a matrix
  y <- reg_data %>% 
    select(all_of(y_var)) %>% 
    as.matrix()
  ## Select coefficient parameters
  beta_hat <- params[-length(params)]
  ## Select error variance parameters
  sigma2_hat <- params[length(params)]
  ## Calculate fitted y values
  y_hat <- X %*% beta_hat
  ## Calculate the pdf values of each outcome
  y_pdf <- dnorm(y, mean = y_hat, sd = sqrt(sigma2_hat))
  ## Calculate the log-likelihood
  ll <- sum(log(y_pdf))
  ## Return the negative of log-likelihood for minimization
  return(-ll)
}
## Maximize the OLS log-likelihood function
mle_ols_1 <- optim(par = c(0, 0, 1), fn = ll_ols, 
                   data = mtcars, y_var = 'mpg', x_vars = 'hp',
                   method = 'BFGS', hessian = TRUE)
## Show optimization results
mle_ols_1
## Show parameter estimates
mle_ols_1$par
## Calculate MLE standard errors
mle_ols_1$hessian %>% 
  solve() %>% 
  diag() %>% 
  sqrt()

### Estimate regression of mpg on disp, wt, and qsec using MLE
## Maximize the OLS log-=likelihood function
mle_ols_2 <- optim(par = c(rep(0, 5), 1), fn = ll_ols, 
                   data = mtcars, y_var = 'mpg', 
                   x_vars = c('hp', 'disp', 'wt', 'qsec'),
                   method = 'BFGS', hessian = TRUE)
## Show parameter estimates
mle_ols_2$par
## Calculate MLE standard errors
mle_ols_2$hessian %>% 
  solve() %>% 
  diag() %>% 
  sqrt()

### Estimate regression of Petal.Length on other iris variables using MLE
## Maximize the OLS log-=likelihood function
mle_ols_3 <- optim(par = c(rep(0, 4), 1), fn = ll_ols, 
                   data = iris, y_var = 'Petal.Length', 
                   x_vars = c('Petal.Width', 'Sepal.Length',
                              'Sepal.Width'),
                   method = 'BFGS', hessian = TRUE)
## Show parameter estimates
mle_ols_3$par
## Calculate MLE standard errors
mle_ols_3$hessian %>% 
  solve() %>% 
  diag() %>% 
  sqrt()
