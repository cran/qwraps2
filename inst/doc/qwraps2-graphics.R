## ----label = "setup", include = FALSE-----------------------------------------
knitr::opts_chunk$set(collapse = TRUE)
library(qwraps2)
packageVersion("qwraps2")

## -----------------------------------------------------------------------------
set.seed(42)
n <- 250
x1 <- x2 <- x3 <- x4 <- vector('numeric', length = n)
x1[1] <- runif(1)
x2[1] <- runif(1)
x3[1] <- runif(1)
x4[1] <- runif(1)

# white noise
Z.1 <- rnorm(n, 0, 1)
Z.2 <- rnorm(n, 0, 2)
Z.3 <- rnorm(n, 0, 5)

for(i in 2:n)
{
  x1[i] <- x1[i-1] + Z.1[i] - Z.1[i-1] + x4[i-1] - x2[i-1]
  x2[i] <- x2[i-1] - 2 * Z.2[i] + Z.2[i-1] - x4[i-1]
  x3[i] <- x3[i-1] + x2[i-1] + 0.2 * Z.3[i] + Z.3[i-1]
  x4[i] <- x4[i-1] + runif(1, 0.5, 1.5) * x4[i-1]
}
testdf <- data.frame(x1, x2, x3, x4)

# Base acf plot for one variable
acf(testdf$x1)

# qacf plot for one variable
qacf(testdf$x1)
qacf(testdf$x1, show_sig = TRUE)


## ----fig.width = 5, fig.height = 5--------------------------------------------
# more than one variable
acf(testdf)
qacf(testdf)
qacf(testdf, show_sig = TRUE)

## -----------------------------------------------------------------------------
acf_plot_data <- qacf(testdf)$data
head(acf_plot_data)

## -----------------------------------------------------------------------------
pefr_m1 <-
  cbind("Large" = pefr[pefr$measurement == 1 & pefr$meter == "Wright peak flow meter", "pefr"],
        "Mini"  = pefr[pefr$measurement == 1 & pefr$meter == "Mini Wright peak flow meter", "pefr"])

## -----------------------------------------------------------------------------
cor(pefr_m1)

ggplot2::ggplot(data = as.data.frame(pefr_m1)) +
  ggplot2::aes(x = Large, y = Mini) +
  ggplot2::geom_point() +
  ggplot2::xlab("Large Meter") +
  ggplot2::ylab("Mini Meter") +
  ggplot2::xlim(0, 800) +
  ggplot2::ylim(0, 800) +
  ggplot2::geom_abline(slope = 1)

## -----------------------------------------------------------------------------
# default plot
qblandaltman(pefr_m1)

# modified plot
ggplot2::last_plot() +
  ggplot2::xlim(0, 800) +
  ggplot2::ylim(-100, 100) +
  ggplot2::xlab("Average of two meters") +
  ggplot2::ylab("Difference in the measurements")

## -----------------------------------------------------------------------------
pefr_mini <-
  cbind(m1 = pefr[pefr$measurement == 1 & pefr$meter == "Mini Wright peak flow meter", "pefr"],
        m2 = pefr[pefr$measurement == 2 & pefr$meter == "Mini Wright peak flow meter", "pefr"])

qblandaltman(pefr_mini)

## -----------------------------------------------------------------------------
# create a survfit object
require(survival)
leukemia.surv <- survival::survfit(survival::Surv(time, status) ~ x, data = survival::aml)

# base R km plot
survival:::plot.survfit(leukemia.surv, conf.int = TRUE, lty = 2:3, col = 1:2)


## ----fig.width = 5------------------------------------------------------------
# qkmplot
qkmplot(leukemia.surv, conf_int = TRUE)

## -----------------------------------------------------------------------------
leukemia_km_data <- qkmplot_bulid_data_frame(leukemia.surv)
head(leukemia_km_data, 3)


## ----fig.width = 5------------------------------------------------------------
qkmplot(leukemia_km_data)

## ----fig.width = 5------------------------------------------------------------
intonly_fit <- survival::survfit(survival::Surv(time, status) ~ 1, data = survival::aml)
survival:::plot.survfit(intonly_fit, conf.int = TRUE)
qkmplot(intonly_fit, conf_int = TRUE)

## -----------------------------------------------------------------------------
set.seed(42)
tidx <- runif(nrow(spambase)) <= 0.80
xidx <- which(names(spambase) != "spam")
yidx <- which(names(spambase) == "spam")
training_set   <- spambase[tidx, ]
validating_set <- spambase[!tidx, ]

## -----------------------------------------------------------------------------
logistic_model <-
  glm(
    spam ~ .
  , data = training_set
  , family = binomial()
  )

ridge_model <-
  glmnet::cv.glmnet(
    y = training_set[, yidx]
  , x = as.matrix(training_set[, xidx])
  , family = binomial()
  , alpha = 0
  )

lasso_model <-
  glmnet::cv.glmnet(
    y = training_set[, yidx]
  , x = as.matrix(training_set[, xidx])
  , family = binomial()
  , alpha = 1
  )

## -----------------------------------------------------------------------------
validating_set$logistic_model_prediction <-
  predict(
    logistic_model
  , newdata = validating_set
  , type = "response"
  )

validating_set$ridge_model_prediction <-
  as.numeric(
    predict(
      ridge_model
    , newx = as.matrix(validating_set[, xidx])
    , type = "response"
    , s = "lambda.1se"
    )
  )

validating_set$lasso_model_prediction <-
  as.numeric(
    predict(
      lasso_model
    , newx = as.matrix(validating_set[, xidx])
    , type = "response"
    , s = "lambda.1se"
    )
  )

## -----------------------------------------------------------------------------
cm1 <- confusion_matrix(spam ~ logistic_model_prediction, data = validating_set)
cm2 <- confusion_matrix(spam ~ ridge_model_prediction, data = validating_set)
cm3 <- confusion_matrix(spam ~ lasso_model_prediction, data = validating_set)

## -----------------------------------------------------------------------------
qroc(cm1) + ggplot2::ggtitle("Logisitic Model")
qroc(cm2) + ggplot2::ggtitle("Ridge Regression Model")
qroc(cm3) + ggplot2::ggtitle("LASSO Regression Model")

## -----------------------------------------------------------------------------
roc_plot_data <-
  rbind(
      cbind(Model = paste("Logisitic; AUROC =", frmt(cm1$auroc, 3)), cm1$cm_stats)
    , cbind(Model = paste("Ridge; AUROC =",     frmt(cm2$auroc, 3)), cm2$cm_stats)
    , cbind(Model = paste("LASSO; AUROC =",     frmt(cm3$auroc, 3)), cm3$cm_stats)
    )

qroc(roc_plot_data) +
  ggplot2::aes(color = Model) +
  ggplot2::theme(legend.position = "bottom")

## -----------------------------------------------------------------------------
qprc(cm1) + ggplot2::ggtitle("Logisitic Model")
qprc(cm2) + ggplot2::ggtitle("Ridge Regression Model")
qprc(cm3) + ggplot2::ggtitle("LASSO Regression Model")

prc_plot_data <-
  rbind(
      cbind(Model = paste("Logisitic; AUPRC =", frmt(cm1$auprc, 3)), cm1$cm_stats)
    , cbind(Model = paste("Ridge; AUPRC =",     frmt(cm2$auprc, 3)), cm2$cm_stats)
    , cbind(Model = paste("LASSO; AUPRC =",     frmt(cm3$auprc, 3)), cm3$cm_stats)
    )

qprc(prc_plot_data) +
  ggplot2::aes(color = Model) +
  ggplot2::geom_hline(yintercept = cm1$prevalence) +
  ggplot2::theme(legend.position = "bottom")

## ----label = "sessioninfo"----------------------------------------------------
sessionInfo()

