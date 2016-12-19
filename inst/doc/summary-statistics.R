## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE) 

## ------------------------------------------------------------------------
library(dplyr)
library(qwraps2)

# define the markup language we are working in.
# options(qwraps2_markup = "latex") is also supported.
options(qwraps2_markup = "markdown")

data(mtcars)

mtcars2 <- 
  dplyr::mutate(mtcars,
                cyl_factor = factor(cyl, 
                                    levels = c(6, 4, 8), 
                                    labels = paste(c(6, 4, 8), "cylinders")),
                cyl_character = paste(cyl, "cylinders"))

str(mtcars2)

## ------------------------------------------------------------------------
with(mtcars2, table(cyl_factor, cyl_character))
with(mtcars2, all.equal(factor(cyl_character), cyl_factor))

## ------------------------------------------------------------------------
mean_sd(mtcars2$mpg)
mean_sd(mtcars2$mpg, denote_sd = "paren") 

## ------------------------------------------------------------------------
mci <- mean_ci(mtcars2$mpg)
mci
print(mci, show_level = TRUE)

## ------------------------------------------------------------------------
median_iqr(mtcars2$mpg)

## ------------------------------------------------------------------------
n_perc(mtcars2$cyl == 4)
n_perc0(mtcars2$cyl == 4)

n_perc(mtcars2$cyl_factor == 4)  # this returns 0 (0.00%) 
n_perc(mtcars2$cyl_factor == "4 cylinders")
n_perc(mtcars2$cyl_factor == levels(mtcars2$cyl_factor)[2]) 

# The count and percentage of 4 or 6 cylinders vehicles in the data set is
n_perc(mtcars2$cyl %in% c(4, 6))

## ------------------------------------------------------------------------
args(summary_table)

## ------------------------------------------------------------------------
our_summary1 <- 
  list("Miles Per Gallon" = 
       list("min" = ~ min(mpg), 
            "max" = ~ max(mpg),
            "mean (sd)" = ~ qwraps2::mean_sd(mpg)),
       "Displacement" = 
       list("min" = ~ min(disp), 
            "max" = ~ max(disp),
            "mean (sd)" = ~ qwraps2::mean_sd(disp)),
       "Weight (1000 lbs)" = 
       list("min" = ~ min(wt), 
            "max" = ~ max(wt),
            "mean (sd)" = ~ qwraps2::mean_sd(wt)),
       "Forward Gears" = 
       list("Three" = ~ qwraps2::n_perc0(gear == 3),
            "Four"  = ~ qwraps2::n_perc0(gear == 4),
            "Five"  = ~ qwraps2::n_perc0(gear == 5))
       ) 

## ---- results = "asis"---------------------------------------------------
summary_table(mtcars2, our_summary1)
summary_table(mtcars2, our_summary1)

## ---- results = "asis"---------------------------------------------------
summary_table(dplyr::group_by(mtcars2, cyl_factor), our_summary1)

## ---- results = "asis"---------------------------------------------------
print(summary_table(dplyr::group_by(mtcars2, cyl_factor), our_summary1), 
      rtitle = "Summary Statistics",
      cnames = c("Col 1", "Col 2", "Col 3"))

## ------------------------------------------------------------------------
args(tab_summary)

## ------------------------------------------------------------------------
tab_summary(mtcars2$mpg)

tab_summary(mtcars2$gear) # gear is a numeric vector!
tab_summary(factor(mtcars2$gear)) 

## ------------------------------------------------------------------------
our_summary2 <- 
  with(mtcars2, 
       list("Miles Per Gallon" = tab_summary(mpg)[c(1, 4, 3)],
            "Displacement (default summary)" = tab_summary(disp),
            "Displacement" = c(tab_summary(disp)[c(1, 4, 3)],
                               "mean (95% CI)" = ~ frmtci(qwraps2::mean_ci(disp))),
            "Weight (1000 lbs)" = tab_summary(wt)[c(1, 4, 3)],
            "Forward Gears" = tab_summary(as.character(gear))
            ))

## ---- results = "asis"---------------------------------------------------
whole <- summary_table(mtcars2, our_summary2)
whole

## ---- results = "asis"---------------------------------------------------
grouped <- summary_table(dplyr::group_by(mtcars2, am, vs),  our_summary2)
grouped

## ---- results = "asis"---------------------------------------------------
both <- cbind(whole, grouped)
both

## ------------------------------------------------------------------------
# tab_summary(mpg)[[3]] ## this errors
tab_summary(mtcars$mpg)[[3]]
with(mtcars, tab_summary(mpg))[[3]]

## ---- results = "asis"---------------------------------------------------
# The same tables:
summary_table(mtcars, list("MPG 1" = with(mtcars, tab_summary(mpg)[[3]])))
summary_table(mtcars, list("MPG 2" = tab_summary(mtcars$mpg)[[3]]))

## ---- results = "asis"---------------------------------------------------
# Different tables
summary_table(dplyr::filter(mtcars, am == 0), list("MPG 3" = with(mtcars, tab_summary(mpg)[[3]])))
summary_table(dplyr::filter(mtcars, am == 0), list("MPG 4" = tab_summary(mtcars$mpg)[[3]]))

