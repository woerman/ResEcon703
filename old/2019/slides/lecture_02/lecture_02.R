### ResEcon 703: Topics in Advanced Econometrics
### Lecture 2: R Tutorial
### Matt Woerman, UMass Amherst

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
c("Hello", "world")
## Create a vector with numeric, character, and logical elements
c(1, "Hello", 3, "world", TRUE)

### Matrices
## Create a 2 (rows) x 5 (columns) matrix of 1:10 arranged by row
matrix(data = 1:10, nrow = 2, byrow = TRUE)

### Lists
## Create a list with a numeric vector, matrix, and character vector
list(c(2, 4, 6, 8), matrix(1:4, 2), c("a", "b", "c"))

### Data frames
## Create a date frame with 4 variables and 3 observations
data.frame(x = 0:2, y = c(2, 4, 8), z = c(1, 5, 7), w = c("a", "b", "c"))

### Functions and packages in R ------------------------------------------------

### Function inputs
## Matrix function defualt inputs
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
mean_sum_squares <- function(num1, num2, num3 = 3){}

### Using packages
## Install a few packages we will use in this course
install.packages(c("tidyverse", "mlogit", "gmm"))
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
set.seed(321)
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
filter(starwars, species == "Droid")

### arrange() example
## Arrange alphabetically by name in starwars
arrange(starwars, name)

### Multiple dplyr functions
## Select, filter, and arrange
arrange(filter(select(starwars, name, homeworld, species), species == "Droid"), name)
## Alternative code for those functions
arrange(
  filter(
    select(starwars, name, homeworld, species),
    species == "Droid"
  ),
  name
)

### Pipes
## Filter with pipes
starwars %>% 
  filter(species == "Droid")

### Multiple dplyr functions with pipes
## Select, filter, and arrange using pipes
starwars %>% 
  select(name, homeworld, species) %>% 
  filter(species == "Droid") %>% 
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
