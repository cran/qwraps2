% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/confusion_matrix.R
\name{confusion_matrix}
\alias{confusion_matrix}
\alias{confusion_matrix.default}
\alias{confusion_matrix.formula}
\alias{is.confusion_matrix}
\alias{print.confusion_matrix}
\title{Confusion Matrices (Contingency Tables)}
\usage{
confusion_matrix(x, ...)

\method{confusion_matrix}{default}(
  x,
  y,
  positive = NULL,
  boot = FALSE,
  boot_samples = 1000L,
  alpha = 0.05,
  ...
)

\method{confusion_matrix}{formula}(
  formula,
  data = parent.frame(),
  positive = NULL,
  boot = FALSE,
  boot_samples = 1000L,
  alpha = 0.05,
  ...
)

is.confusion_matrix(x)

\method{print}{confusion_matrix}(x, ...)
}
\arguments{
\item{x}{prediction condition vector, a two level factor variable or a
variable that can be converted to one.}

\item{...}{not currently used}

\item{y}{True Condition vector with the same possible values as x.}

\item{positive}{the level of x and y which is the positive outcome.  If
\code{NULL} the first level of \code{factor(y)} will be used as the positive
level.}

\item{boot}{boolean, should bootstrapped confidence intervals for the
sensitivity and specificity be computed?  Defaults to FALSE.}

\item{boot_samples}{number of bootstrapping sample to generate, defaults to
1000L.  Ignored if \code{boot == FALSE}.}

\item{alpha}{100(1-alpha)% confidence intervals for specificity and
sensitivity.  Ignored if \code{boot == FALSE}.}

\item{formula}{column (known) ~ row (test) for building the confusion matrix}

\item{data}{environment containing the variables listed in the formula}
}
\value{
The sensitivity and specificity functions return numeric values.
\code{confusion_matrix} returns a list with elements:
\itemize{
  \item \code{tab} the confusion matrix,
  \item \code{cells}
  \item \code{stats} a matrix of summary statistics and confidence intervals.
}
}
\description{
Construction of confusion matrices, accuracy, sensitivity,
specificity, confidence intervals (Wilson's method and (optional
bootstrapping)).
}
\details{
Sensitivity and Specificity:
For the sensitivity and specificity function we expect the 2-by-2 confusion
matrix (contingency table) to be of the form:

\tabular{lccc}{
                    \tab      \tab True \tab Condition \cr
                    \tab      \tab +    \tab -         \cr
Predicted Condition \tab +    \tab TP   \tab FP        \cr
Predicted Condition \tab -    \tab FN   \tab TN        \cr
}
where
\itemize{
  \item FN: False Negative, and
  \item FP: False Positive,
  \item TN: True Negative,
  \item TP: True Positive.
}

The statistics returned in the \code{stats} element are:
\itemize{
  \item accuracy    = (TP + TN) / (TP + TN + FP + FN)
  \item sensitivity = TP / (TP + FN)
  \item specificity = TN / (TN + FP)
  \item positive predictive value (PPV) = TP / (TP + FP)
  \item negative predictive value (NPV) = TN / (TN + FN)
  \item false negative rate (FNR) = 1 - Sensitivity
  \item false positive rate (FPR) = 1 - Specificity
  \item false discovery rate (FDR) = 1 - PPV
  \item false omission rate (FOR) = 1 - NPV
  \item F1 score
  \item Matthews Correlation Coefficient (MCC) =
    ((TP * TN) - (FP * FN)) / sqrt((TP + FP) (TP+FN) (TN+FP) (TN+FN))
}

Synonyms for the statistics:
\itemize{
\item Sensitivity: true positive rate (TPR), recall, hit rate
\item Specificity: true negative rate (TNR), selectivity
\item PPV: precision
\item FNR: miss rate
}

Sensitivity and PPV could, in some cases, be indeterminate due to division by
zero.  To address this we will use the following rule based on the DICE group
\url{https://github.com/dice-group/gerbil/wiki/Precision,-Recall-and-F1-measure}:
If TP, FP, and FN are all 0, then PPV, sensitivity, and F1 will be defined to
be 1.  If TP are 0 and FP + FN > 0, then PPV, sensitivity, and F1 are all
defined to be 0.
}
\examples{
################################################################################
## Example 1
test  <- c(rep(1, 53), rep(0, 47))
truth <- c(rep(1, 20), rep(0, 33), rep(1, 10), rep(0, 37))
con_mat <- confusion_matrix(x = test, y = truth, positive = "1")
str(con_mat)

con_mat

con_mat$cells$true_positives  # 20
con_mat$cells$true_negatives  # 37
con_mat$cells$false_positives # 33
con_mat$cells$false_negatives # 10

con_mat_with_boot <- confusion_matrix(test, truth, positive = "1", boot = TRUE)
con_mat_with_boot

# only one value in one of the vectors
a <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0)  # all zeros
b <- c(1,0,1,0,1,0,0,0,0,0,0,0,0,1)  # some zeros and ones

confusion_matrix(a, b)
confusion_matrix(b, a)
confusion_matrix(a, b, positive = 1)
confusion_matrix(b, a, positive = 1)


################################################################################
## Example 2: based on an example from the wikipedia page:
# https://en.wikipedia.org/wiki/Confusion_matrix

animals <-
  data.frame(Predicted = c(rep("Cat",    5 + 2 +  0),
                           rep("Dog",    3 + 3 +  2),
                           rep("Rabbit", 0 + 1 + 11)),
             Actual    = c(rep(c("Cat", "Dog", "Rabbit"), times = c(5, 2,  0)),
                           rep(c("Cat", "Dog", "Rabbit"), times = c(3, 3,  2)),
                           rep(c("Cat", "Dog", "Rabbit"), times = c(0, 1, 11))),
             stringsAsFactors = FALSE)

table(animals)

cats <- apply(animals, 1:2, function(x) ifelse(x == "Cat", "Cat", "Non-Cat"))

# Default calls, note the difference based on what is set as the 'positive'
# value.
confusion_matrix(cats[, "Predicted"], cats[, "Actual"], positive = "Cat")
confusion_matrix(cats[, "Predicted"], cats[, "Actual"], positive = "Non-Cat")

# Using a Formula
confusion_matrix(formula = I(Actual == "Cat") ~ I(Predicted == "Cat"),
                 data = animals,
                 positive = "TRUE")

confusion_matrix(formula = I(Actual == "Cat") ~ I(Predicted == "Cat"),
                 data = animals,
                 positive = "TRUE",
                 boot = TRUE)

################################################################################
## Example 3
russell <-
  data.frame(Pred  = c(rep(0, 2295), rep(0, 118), rep(1, 1529), rep(1, 229)),
             Truth = c(rep(0, 2295), rep(1, 118), rep(0, 1529), rep(1, 229)))

# The values for Sensitivity, Specificity, PPV, and NPV are dependent on the
# "positive" level.  By default, the first level of y is used.
confusion_matrix(x = russell$Pred, y = russell$Truth, positive = "0")
confusion_matrix(x = russell$Pred, y = russell$Truth, positive = "1")

confusion_matrix(Truth ~ Pred, data = russell, positive = "0")
confusion_matrix(Truth ~ Pred, data = russell, positive = "1")

}
