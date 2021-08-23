########################################################
##### ResEcon 703: Topics in Advanced Econometrics #####
#####              Week 2: R Tutorial              #####
#####         Matt Woerman, UMass Amherst          #####
########################################################


### R Resources ----------------------------------------------------------------

### R swirl interactive tutorials
## Install swirl package
install.packages('swirl')
## Load swirl package
library(swirl)
## Install swirl tutorials
install_course('R Programming')
install_course('Getting and Cleaning Data')
install_course('Advanced R Programming')
## Start swirl tutorials
swirl()


### Objects in R ---------------------------------------------------------------

### Object basics
## Assign a value of 1 to an object called a
a <- 1
## Assign a value of 2 to an object called b
b <- 2
## You use these objects in operations and functions
a + b
## Assign object c to have a value equal to a + b
c <- a + b
c

### Vectors
## Create a numeric vector
c(1, 1, 2, 3, 5, 8, 13)
## Create a sequential vector
0:9
## Create a character vector
c('Hello', 'world')
## Create a vector with numeric, character, and logical elements
c(1, 'Hello', 3, 'world', TRUE)

### Matrices
## Create a 2 (rows) x 5 (columns) matrix of 1:10 arranged by row
matrix(data = 1:10, nrow = 2, byrow = TRUE)

### Lists
## Create a list with a numeric vector, matrix, and character vector
list(c(2, 4, 6, 8), matrix(1:4, 2), c('a', 'b', 'c'))

### Data frames
## Create a date frame with 4 variables and 3 observations
data.frame(x = 0:2, y = c(2, 4, 8), z = c(1, 5, 7), w = c('a', 'b', 'c'))


### Functions and packages in R ------------------------------------------------

### Function inputs
## Matrix function default inputs
matrix(data = NA, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL)
## Create matrix with default inputs
matrix()

### Function example
## Define a function that calculates the MSS from three inputs
mean_sum_squares <- function(num1, num2, num3){
  ## Calculate the mean sum of squares
  mss <- (num1^2 + num2^2 + num3^2) / 3
  ## Return the answer
  return(mss)
}
## Calculate the mean sum of squares of 1, 2, and 3
mean_sum_squares(1, 2, 3)
## Make 3 the default input for the third argument
mean_sum_squares <- function(num1, num2, num3 = 3)
  
### Using packages
## Install a few packages we will use in this course
install.packages(c('tidyverse', 'mlogit', 'gmm'))
## Load those packages
library(tidyverse)
library(mlogit)
library(gmm)


### Math and statistics in R ---------------------------------------------------

### Math operations
## Addition
a + b
## Subtraction
a - b
## Multiplication
a * b
## Division
a / b
## Exponents
a^b

### Math functions
## Absolute value
abs(a - b)
## Exponential
exp(a)
## Square root
sqrt(b)
## Natural log
log(b)
## Log base 10
log(b, base = 10)

### Statistics functions
## Create a vector 0 to 4
v <- 0:4
v
## Mean
mean(v)
## Median
median(v)
## Standard deviation
sd(v)

### Sampling functions
## Set the seed for randomization
set.seed(703)
## Draw from a random normal N(3, 2)
rnorm(n = 5, mean = 3, sd = sqrt(2))
## Draw with replacement from v
sample(v, size = 10, replace = TRUE)
# CDF of a standard normal at z = 1.96
pnorm(q = 1.96, mean = 0, sd = 1)

### Vectorization
## Addition with each element
v + a
## Multiplication with each element
v * b
## Exponential of each element
exp(v)
## Natural log of each element
log(v)

### Vector math
## Elementwise addition
v + 1:5
## Elementwise multiplication
v * 1:5
## Elementwise addition with different lengths
v + 1:4

### Indexing vectors
## Access the second element of v
v[2]
## Access the second and fourth elements of v
v[c(2, 4)]
## Access all but the first element of v
v[-1]
## Replace the first element of v with 5
v[1] <- 5
v

### Matrices as vectors
## Create a matrix
m <- matrix(1:4, nrow = 2)
m
## Mean
mean(m)
## Natural log of each element
log(m)

### Matrix addition
## Create a second matrix
n <- matrix(c(2, 4, 6, 8), nrow = 2)
n
## Matrix addition
m + n

### Matrix multiplication
## Elementwise matrix multiplication
m * n
## Matrix product
m %*% n

### Matrix functions
## Transpose
t(m)
## Inverse
solve(m)

### Indexing matrices
## Access the element in the second row and first column of m
m[2, 1]
## Access the first row of m
m[1, ]
## Access the second column of m
m[, 2]


### Data in R ------------------------------------------------------------------

### Example data frame
## Show an example data frame, mtcars
head(mtcars)

### Indexing data frames
## Access the third observation of mtcars
mtcars[3, ]
## Access the second variable of mtcars
mtcars[, 2]
## Access the cyl variable of mtcars
mtcars$cyl

### Adding new variables
## Add an id variable to mtcars
mtcars$id <- 1:nrow(mtcars)
## Add a variable that is the power-to-weight ratio (hp / wt)
mtcars$ptw <- mtcars$hp / mtcars$wt
head(mtcars)

### Adding new variables with dplyr
## Add id and power-to-weight ratio variables
mtcars <- mutate(mtcars, id = 1:n(), ptw = hp / wt)
head(mtcars)

### Example tibble
## Show an example tibble, starwars
starwars

### select() example
## Select name, homeworld, and species in starwars
select(starwars, name, homeworld, species)

### filter() example
## Filter to show only droids in starwars
filter(starwars, species == 'Droid')

### arrange() example
## Arrange alphabetically by name in starwars
arrange(starwars, name)

### Multiple dplyr functions
## Select, filter, and arrange
arrange(filter(select(starwars, name, homeworld, species), species == 'Droid'), name)
## Alternative code for those functions
arrange(
  filter(
    select(starwars, name, homeworld, species),
    species == 'Droid'
  ),
  name
)

### Pipes
## Filter with pipes
starwars %>% 
  filter(species == 'Droid')

### Multiple dplyr functions with pipes
## Select, filter, and arrange using pipes
starwars %>% 
  select(name, homeworld, species) %>% 
  filter(species == 'Droid') %>% 
  arrange(name)

### summarize() example
## Calculate mean height and mass by species
starwars %>% 
  group_by(species) %>% 
  summarize(mean_height = mean(height), mean_mass = mean(mass))

### Skipping NAs
## Calculate non-missing mean height and mass by species
starwars %>% 
  group_by(species) %>% 
  summarize(mean_height = mean(height, na.rm = TRUE), 
            mean_mass = mean(mass, na.rm = TRUE))


### R Example ------------------------------------------------------------------

### Examine the mtcars dataset
## Look at the mtcars data
tibble(mtcars)
## Summarize the mtcars dataset
mtcars %>% 
  select(mpg, disp, hp, wt, qsec) %>% 
  summary()
## Plot the mtcars dataset
ggplot(data = mtcars, mapping = aes(x = hp, y = mpg)) +
  geom_point()

### Run OLS regression using the lm() function
## See the help file for lm()
?lm
lm(formula, data, subset, weights, na.action,
   method = "qr", model = TRUE, x = FALSE, y = FALSE, qr = TRUE,
   singular.ok = TRUE, contrasts = NULL, offset, ...)
## Run OLS regression
reg_lm <- lm(formula = mpg ~ hp, data = mtcars)
## Summarize OLS regression results
summary(reg_lm)

### Run OLS regression by hand
## Add column of ones for the constant term
reg_data <- mtcars %>% 
  mutate(constant = 1)
## Select data for X and convert to a matrix
X <- reg_data %>% 
  select(constant, hp) %>% 
  as.matrix()
## Select data for y and convert to a matrix
y <- reg_data %>% 
  select(mpg) %>% 
  as.matrix()
## Make sure matrices look correct
head(X)
head(y)
## Estimate beta parameters
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
beta_hat
## Calculate fitted y values
y_hat <- X %*% beta_hat
head(y_hat)
## Calculate residuals
resid <- y - y_hat
head(resid)
## Estimate variance of error term
sigma2_hat <- t(resid) %*% resid / (nrow(X) - ncol(X))
sigma2_hat
## Estimate variance-covariance matrix of beta estimates
vcov_hat <- c(sigma2_hat) * solve(t(X) %*% X)
vcov_hat
## Calculate standard errors of beta estimates
std_err <- sqrt(diag(vcov_hat))
std_err
## Calculate t stats of beta estimates
t_stat <- beta_hat / std_err
t_stat
## Calculate p values of beta estimates
p_value <- 2 * pt(q = -abs(t_stat), df = nrow(X) - ncol(X))
p_value
## Organize regression results into matrix
results <- cbind(beta_hat, std_err, t_stat, p_value)
results
## Name columns of results matrix
colnames(results) <- c('Estimate', 'Std. Error', 't stat', 'p value')
results
## Compare to lm() results
summary(reg_lm)
results

### Create function to perform OLS regression
## Function to perform OLS regression
ols <- function(data, y_var, x_vars){
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
  ## Estimate beta parameters
  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
  ## Calculate fitted y values
  y_hat <- X %*% beta_hat
  ## Calculate residuals
  resid <- y - y_hat
  ## Estimate variance of error term
  sigma2_hat <- t(resid) %*% resid / (nrow(X) - ncol(X))
  ## Estimate variance-covariance matrix of beta estimates
  vcov_hat <- c(sigma2_hat) * solve(t(X) %*% X)
  ## Calculate standard errors of beta estimates
  std_err <- sqrt(diag(vcov_hat))
  ## Calculate t stats of beta estimates
  t_stat <- beta_hat / std_err
  ## Calculate p values of beta estimates
  p_value <- 2 * pt(q = -abs(t_stat), df = nrow(X) - ncol(X))
  ## Organize regression results into matrix
  results <- cbind(beta_hat, std_err, t_stat, p_value)
  ## Name columns of results matrix
  colnames(results) <- c('Estimate', 'Std. Error', 't stat', 'p value')
  return(results)
}

### Run OLS regressions using user-defined OLS function
## Regress mpg on hp in mtcars dataset
ols(data = mtcars, y_var = 'mpg', x_vars = 'hp')
## Regress mpg on disp, hp, wt, and qsec in mtcars dataset
ols(data = mtcars, 
    y_var = 'mpg', 
    x_vars = c('hp', 'disp', 'wt', 'qsec'))
## Regress Petal.Length on Sepal.Length, Sepal.Width, and Petal.Width
## in iris dataset
ols(data = iris, 
    y_var = 'Petal.Length', 
    x_vars = c('Petal.Width', 'Sepal.Length', 'Sepal.Width',))
