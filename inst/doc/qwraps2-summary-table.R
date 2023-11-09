## ----label = "setup", include = FALSE-----------------------------------------
knitr::opts_chunk$set(collapse = TRUE)

## -----------------------------------------------------------------------------
set.seed(42)
library(qwraps2)
options(qwraps2_markup = "markdown")

## ----label = 'build_example_data_for_qable'-----------------------------------
d <- data.frame(
       group = sample(size = 15, paste0("grp", 1:5), replace = TRUE)
     , id = sample(size = 15, x = LETTERS)
     , V2 = rnorm(15)
     , V3 = rep(c(1, 2, NA), times = 5)
     )
d <- d[order(d$group, d$id), ]

## ----label = "kable1", results = "asis"---------------------------------------
knitr::kable(d, row.names = FALSE)

## -----------------------------------------------------------------------------
c(table(d$group))

## ----label = "qable1", results = "asis"---------------------------------------
qable(  x = d[, c("V2", "V3")]
      , rgroup = c(table(d$group)) # row group
      , rnames = d$id              # row names
)

## ----results = "asis"---------------------------------------------------------
model <-
  glm(spam ~
        word_freq_your + word_freq_conference + word_freq_business +
        char_freq_semicolon + char_freq_exclamation_point +
        capital_run_length_total + capital_run_length_longest
    , data = spambase
    , family = binomial()
  )

model_summary <-
  data.frame(
    parameter = names(coef(model))
  , odd_ratio = frmt(exp(coef(model)), digits = 3)
  , lcl       = frmt(exp(coef(model) + qnorm(0.025) * sqrt(diag(vcov(model)))), digits = 3)
  , ucl       = frmt(exp(coef(model) + qnorm(0.975) * sqrt(diag(vcov(model)))), digits = 3)
  , pval      = frmtp(summary(model)$coef[, 4])
  )

qable(model_summary[-1, c('odd_ratio', 'lcl', 'ucl', 'pval')]
      , rtitle = "Parameter"
      , rgroup = c("Word Frequency" = 3, "Character Frequency" = 2, "Capital Run Length" = 2)
      , rnames = c("Your", "Conference", "Business", ";", "!", "Total", "Longest")
      , kable_args = list(align = "lrrrr", caption = "Regression Model Summary")
      , cnames = c("Odds Ratio", "Lower Conf. Limit", "Upper Conf. Limit", "P-value")
      )

## -----------------------------------------------------------------------------
our_summary1 <-
  list("Miles Per Gallon" =
       list("min"       = ~ min(mpg),
            "max"       = ~ max(mpg),
            "mean (sd)" = ~ qwraps2::mean_sd(mpg)),
       "Displacement" =
       list("min"       = ~ min(disp),
            "median"    = ~ median(disp),
            "max"       = ~ max(disp),
            "mean (sd)" = ~ qwraps2::mean_sd(disp)),
       "Weight (1000 lbs)" =
       list("min"       = ~ min(wt),
            "max"       = ~ max(wt),
            "mean (sd)" = ~ qwraps2::mean_sd(wt)),
       "Forward Gears" =
       list("Three" = ~ qwraps2::n_perc0(gear == 3),
            "Four"  = ~ qwraps2::n_perc0(gear == 4),
            "Five"  = ~ qwraps2::n_perc0(gear == 5))
       )

## ----label = "mtcars2_whole", results = "asis"--------------------------------
whole <-
  summary_table(
    x = mtcars2
  , summaries = our_summary1
  , qable_args = list(kable_args = list(caption = "mtcars2 data summary"))
  )
whole

## ----label = "mtcars2_by_cylf", results = "asis"------------------------------
by_cylf <-
  summary_table(
    x = mtcars2
  , summaries = our_summary1
  , by = c("cyl_factor")
  , qable_args = list(rtitle = "Summary Statistics"
                      , kable_args = list(caption = "mtcars2 data summary by cyl_factor"))
  )
by_cylf


## ----label = "mtcars2_by_cylc", results = "asis"------------------------------
by_cylc <-
  summary_table(
    x = mtcars2
  , summaries = our_summary1
  , by = c("cyl_character")
  , qable_args = list(rtitle = "Summary Statistics"
                      , kable_args = list(caption = "mtcars2 data summary by cyl_character"))
  )
by_cylc

## ----label = "mtcars2_by_cyl_transmission", results = "asis"------------------
by_cyl_am <-
  summary_table(
    x = mtcars2
  , summaries = our_summary1
  , by = c("cyl_factor", "transmission")
  )
by_cyl_am

## ----label = "mtcars2_cbind", results = "asis"--------------------------------
both <- cbind(whole, by_cylf)
both

## ----label = "updated_both", results = "asis"---------------------------------

print(both,
      qable_args = list(
        rtitle = "ROW-TITLE",
        cnames = c("Col 0", "Col 1", "Col 2", "Col 3"),
        kable_args = list(
          align = "lcrcr",
          caption = "mtcars2 data summary - new caption"
        )
      ))

## -----------------------------------------------------------------------------
str(both)

## -----------------------------------------------------------------------------
# difference in means
mpvals <-
  sapply(
         list(mpg  = lm(mpg  ~ cyl_factor, data = mtcars2),
              disp = lm(disp ~ cyl_factor, data = mtcars2),
              wt   = lm(wt   ~ cyl_factor, data = mtcars2)),
         extract_fpvalue)

# Fisher test
fpval <- frmtp(fisher.test(table(mtcars2$gear, mtcars2$cyl_factor))$p.value)

## -----------------------------------------------------------------------------
both <- cbind(both, "P-value" = "")
both[grepl("mean \\(sd\\)", both[, 1]), "P-value"] <- mpvals
both[grepl("Forward Gears", both[, 1]), "P-value"] <- fpval

## ----label = "both_with_pvals", results = "asis"------------------------------
print(both, qable_args = list(kable_args = list(caption = "mtcars2 summary with p-values")))

## ----results = "asis"---------------------------------------------------------
gear_summary <-
  list("Forward Gears" =
       list("Three" = ~ qwraps2::n_perc0(gear == 3),
            "Four"  = ~ qwraps2::n_perc0(gear == 4),
            "Five"  = ~ qwraps2::n_perc0(gear == 5)),
       "Transmission" =
       list("Automatic" = ~ qwraps2::n_perc0(am == 0),
            "Manual"    = ~ qwraps2::n_perc0(am == 1))
       )

gear_summary <-
setNames(gear_summary,
         c(
         paste("Forward Gears: ", frmtp(fisher.test(xtabs( ~ gear + cyl_factor, data = mtcars2))$p.value)),
         paste("Transmission: ",  frmtp(fisher.test(xtabs( ~ am + cyl_factor, data = mtcars2))$p.value)))
         )

summary_table(mtcars2, gear_summary, by = "cyl_factor")

## -----------------------------------------------------------------------------
t_mpg  <- summary_table(mtcars2, summaries = our_summary1["Miles Per Gallon"], by = "cyl_factor")
t_disp <- summary_table(mtcars2, summaries = our_summary1["Displacement"], by = "cyl_factor")
t_wt   <- summary_table(mtcars2, summaries = our_summary1["Weight (1000 lbs)"], by = "cyl_factor")

t_mpg  <- cbind(t_mpg,  "pvalue" = "")
t_disp <- cbind(t_disp, "pvalue" = "")
t_wt   <- cbind(t_wt,   "pvalue" = "")

t_mpg[ grepl("mean", t_mpg[, 1]),  "pvalue"] <- "mpg-pvalue"
t_disp[grepl("mean", t_disp[, 1]), "pvalue"] <- "disp-pvalue"
t_wt[  grepl("mean", t_wt[, 1]),   "pvalue"] <- "wt-pvalue"

## -----------------------------------------------------------------------------
rbind(t_mpg, t_disp, t_wt)
rbind(t_wt, t_disp, t_mpg)

## -----------------------------------------------------------------------------
new_data_frame <-
  data.frame(age = c(18, 20, 24, 17, 43),
             edu = c(1, 3, 1, 5, 2),
             rt  = c(0.01, 0.04, 0.02, 0.10, 0.06))

# Set a label for the variables
attr(new_data_frame$age, "label") <- "Age in years"
attr(new_data_frame$rt,  "label") <- "Reaction time"

# mistakenly set the attribute to name instead of label
attr(new_data_frame$edu, "name") <- "Education"

## -----------------------------------------------------------------------------
qsummary(new_data_frame)

## ----results = "asis"---------------------------------------------------------
summary_table(new_data_frame)

## -----------------------------------------------------------------------------
qsummary(mtcars2[, c("mpg", "cyl_factor", "wt")])

## ----label="summary_table_mtcars2_default", results = "asis"------------------
summary_table(mtcars2[, c("mpg", "cyl_factor", "wt")])

## -----------------------------------------------------------------------------
new_summary <-
  qsummary(mtcars2[, c("mpg", "cyl_factor", "wt")],
           numeric_summaries = list("Minimum" = "~ min(%s)",
                                    "Maximum" = "~ max(%s)"),
           n_perc_args = list(digits = 1, show_symbol = TRUE, show_denom = "always"))
str(new_summary)

## ----results = "asis"---------------------------------------------------------
summary_table(mtcars2, new_summary)

## -----------------------------------------------------------------------------
print(sessionInfo(), local = FALSE)

