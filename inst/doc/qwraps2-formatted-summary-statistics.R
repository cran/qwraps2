## ----label = "setup", include = FALSE-----------------------------------------
knitr::opts_chunk$set(collapse = TRUE)

## -----------------------------------------------------------------------------
set.seed(42)
library(qwraps2)
# define the markup language we are working in.
# options(qwraps2_markup = "latex") is also supported.
options(qwraps2_markup = "markdown")

## -----------------------------------------------------------------------------
library(qwraps2)

## -----------------------------------------------------------------------------
data(mtcars2)
str(mtcars2)

## -----------------------------------------------------------------------------
mean_sd(mtcars2$mpg)
mean_sd(mtcars2$mpg, denote_sd = "paren")

## -----------------------------------------------------------------------------
mci <- mean_ci(mtcars2$mpg)
str(mci)
mci
print(mci, show_level = TRUE)

## -----------------------------------------------------------------------------
median_iqr(mtcars2$mpg)

## -----------------------------------------------------------------------------
n_perc(mtcars2$cyl == 4)
n_perc0(mtcars2$cyl == 4)

n_perc(mtcars2$cyl_factor == 4)  # this returns 0 (0.00%)
n_perc(mtcars2$cyl_factor == "4 cylinders")
n_perc(mtcars2$cyl_factor == levels(mtcars2$cyl_factor)[2])

# The count and percentage of 4 or 6 cylinders vehicles in the data set is
n_perc(mtcars2$cyl %in% c(4, 6))

## -----------------------------------------------------------------------------
x <- runif(6, min = 4, max = 70)

# geometric mean
mu_g <- prod(x) ** (1 / length(x))
mu_g
exp(mean(log(x)))
1.2 ** mean(log(x, base = 1.2))

# geometric standard deviation
exp(sd(log(x)))  ## This is wrong

# these equations are correct
sigma_g <- exp(sqrt(sum(log(x / mu_g) ** 2) / length(x)))
sigma_g

exp(sqrt((length(x) - 1) / length(x)) * sd(log(x)))

## -----------------------------------------------------------------------------
gmean(x)
all.equal(gmean(x), mu_g)

gvar(x)
all.equal(gvar(x), sigma_g^2)  # This is supposed to be FALSE
all.equal(gvar(x), exp(log(sigma_g)^2))

gsd(x)
all.equal(gsd(x), sigma_g)

## -----------------------------------------------------------------------------
gmean_sd(x)

## ----eval = FALSE-------------------------------------------------------------
#  vignette("qwraps2-summary-table")

## -----------------------------------------------------------------------------
print(sessionInfo(), local = FALSE)

