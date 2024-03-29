#' @title Qable: an extended version of knitr::kable
#'
#' @description Create a simple table via \code{\link[knitr]{kable}} with row
#' groups and rownames similar to those of \code{\link[Hmisc]{latex}} from the
#' Hmisc package or \code{\link[htmlTable]{htmlTable}} from the htmlTable
#' package.
#'
#' @details
#'
#' \code{rtitle} can be used to add a title to the column constructed by the
#' \code{rgroup} and \code{rnames}.  The basic layout of a table generated by
#' \code{qable} is:
#'
#' \tabular{lcc}{
#'  rtitle    \tab cnames[1] \tab cnames[2] \cr
#'  rgroup[1] \tab           \tab           \cr
#'  rnames[1] \tab x[1, 1]   \tab x[1, 2]   \cr
#'  rnames[2] \tab x[2, 1]   \tab x[2, 2]   \cr
#'  rnames[3] \tab x[3, 1]   \tab x[3, 2]   \cr
#'  rgroup[2] \tab           \tab           \cr
#'  rnames[4] \tab x[4, 1]   \tab x[4, 1]   \cr
#'  rnames[5] \tab x[5, 1]   \tab x[5, 1]   \cr
#' }
#'
#' Passing arguments to \code{link[knitr]{kable}} is done via the list
#' \code{kable_args}.  This is an improvement in 0.6.0 to address arguments with
#' different use between qable and kable but the same name, notably
#' \code{format}.  Within the print method for \code{qwraps2_qable} objects,
#' some default arguments for knitr::kable are created.
#'
#' Defaults if the named element of \code{kable_args} is missing:
#' \code{kable_args$format} will be "latex" if \code{markup = "latex"} and will
#' be \code{"pipe"} if \code{markup = "markdown"}.
#'
#' \code{kable_args$escape = !(markup = "latex")}
#'
#' \code{kable_args$row.names} defaults to \code{FALSE}
#'
#' \code{kable_args$col.names} defaults to \code{colnames(x)}
#'
#' @seealso
#' \code{\link[knitr]{kable}}
#'
#' \code{\link{summary_table}}, for an example of build a data summary table.
#'
#' For more detail on arguments you can pass via \code{kable_args} look at the
#' non-exported functions form the knitr package \code{knitr:::kable_latex},
#' \code{knitr:::kable_markdown}, or others.
#'
#' @param x \code{matrix} or \code{data.frame} to be turned into a qable
#' @param rtitle a row grouping title. See Details.
#' @param rgroup a named numeric vector with the name of the row group and the
#' number of rows within the group.  \code{sum(rowgroup) == nrow(x)}.
#' @param rnames a character vector of the row names
#' @param cnames column names
#' @param markup the markup language to use expected to be either "markdown" or
#' "latex"
#' @param kable_args a list of named arguments to send to
#' \code{\link[knitr]{kable}}.  See Details.
#' @param ... pass through
#'
#' @return \code{qable} returns a \code{qwraps2_qable} object that is just a character matrix with
#' some additional attributes and the print method returns, invisibly, the
#' object passed to print.
#'
#' @examples
#' data(mtcars)
#' x <- qable(mtcars)
#' x
#' qable(mtcars, markup = "markdown")
#'
#' # by make
#' make <- sub("^(\\w+)\\s?(.*)$", "\\1", rownames(mtcars))
#' make <- c(table(make))
#'
#' # A LaTeX table with a vertical bar between each column
#' qable(mtcars[sort(rownames(mtcars)), ], rgroup = make)
#'
#' # A LaTeX table with no vertical bars between columns
#' qable(mtcars[sort(rownames(mtcars)), ], rgroup = make, kable_args = list(vline = ""))
#'
#' # a markdown table
#' qable(mtcars[sort(rownames(mtcars)), ], rgroup = make, markup = "markdown")
#'
#' # define your own column names
#' qable(mtcars[sort(rownames(mtcars)), ],
#'       rgroup = make,
#'       cnames = toupper(colnames(mtcars)),
#'       markup = "markdown")
#'
#' # define your own column names and add a title
#' qable(mtcars[sort(rownames(mtcars)), ],
#'       rtitle = "Make & Model",
#'       rgroup = make,
#'       cnames = toupper(colnames(mtcars)),
#'       markup = "markdown")
#'
#' @export
#' @rdname qable
qable <- function(x, rtitle = "", rgroup = numeric(0), rnames = rownames(x), cnames = colnames(x), markup = getOption("qwraps2_markup", "latex"), kable_args = list(), ...) {
  UseMethod("qable")
}

#' @export
qable.data.frame <- function(x, rtitle = "", rgroup = numeric(0), rnames = rownames(x), cnames = colnames(x), markup = getOption("qwraps2_markup", "latex"), kable_args = list(), ...) {
  qable(as.matrix(x), rtitle = rtitle, rgroup = rgroup, rnames = rnames, cnames = cnames, markup = markup, kable_args = kable_args, ...)
}

#' @export
qable.matrix <- function(x, rtitle = "", rgroup = numeric(0), rnames = rownames(x), cnames = colnames(x), markup = getOption("qwraps2_markup", "latex"), kable_args = list(), ...) {

  if (!(markup %in% c("latex", "markdown"))) {
    stop("markup is either 'latex' or 'markdown'")
  }

  xmat <- matrix("~", nrow = nrow(x) + length(rgroup), ncol = 1 + ncol(x))

  if (length(rgroup) > 0) {
    rg_idx <- cumsum(c(1, 1 + rgroup[-length(rgroup)]))
    xmat[-rg_idx, -1] <- as.matrix(x)

    if (markup == "latex") {
      xmat[rg_idx, 1] <- paste0("\\bf{", names(rgroup), "}")
      xmat[-rg_idx, 1] <- paste("~~", rnames)
    } else {
      xmat[rg_idx, 1] <- paste0("**", names(rgroup), "**")
      xmat[-rg_idx, 1] <- paste("&nbsp;&nbsp;", rnames)
    }
  } else {
    xmat[, 1] <- rnames
    xmat[, -1] <- as.matrix(x)
  }

  if (markup == "markdown") {
    xmat <- apply(xmat, 1:2, function(x) gsub("~", "&nbsp;&nbsp;", x))
  }

  if (missing(rtitle)) {
    cnames <- c("", cnames)
  } else {
    cnames <- c(rtitle, cnames)
  }
  colnames(xmat) <- cnames
  class(xmat) <- "qwraps2_qable"
  attr(xmat, "qable_args") <- list(rtitle = rtitle, rgroup = rgroup, rnames = rnames, cnames = cnames, markup = markup, kable_args = kable_args, ...)
  xmat
}

#' @export
print.qwraps2_qable <- function(x, ...) {
  kargs <- attr(x, "qable_args")[["kable_args"]]

  if (!("format" %in% names(kargs))) {
    if (attr(x, "qable_args")[["markup"]] == "markdown") {
      kargs$format <- "pipe"
    } else {
      kargs$format <- "latex"
    }
  }

  if (!("escape" %in% names(kargs))) {
    kargs$escape <- !(attr(x, "qable_args")[["markup"]] == "latex")
  }

  if (!("row.names" %in% names(kargs))) {
    kargs$row.names <- FALSE
  }

  if (!("col.names" %in% names(kargs))) {
    if (ncol(x) == length(attr(x, "qable_args")[["cnames"]])) {
      kargs$col.names <- c(attr(x, "qable_args")[["cnames"]])
    } else {
      kargs$col.names <- c(attr(x, "qable_args")[["rtitle"]], attr(x, "qable_args")[["cnames"]])
    }
  }

  k <- do.call(what = knitr::kable, args = c(list(x = x), kargs, ...))
  print(k)
  invisible(x)
}

#' @export
#' @export
cbind.qwraps2_qable <- function(...) {
  tabs <- list(...)

  # verify number of rows are same for all inputs
  n <-
    sapply(tabs, function(x) {
             if (is.matrix(x)) {
               n <- nrow(x)
             } else if (is.vector(x)) {
               n <- length(x)
             }
             n
  })
  if (any(n != 1 & n != n[1])) {
    stop(sprintf("nrow, or length of a vector, for all inputs needs to be %s, the nrow of the first object.", n[1]))
  }

  # verify the first column is the same for all the qables, error if not
  rgroups <- lapply(tabs, function(x) { if (inherits(x, "qwraps2_qable")) x[, 1]} )
  rgroups <- Filter(function(x) !is.null(x), x = rgroups)
  rgroups <- sapply(rgroups, identical, y = rgroups[[1]])
  if (length(rgroups) > sum(rgroups)) {
    stop("the rgroup are not identical for all the inputed qables.")
  }


  # Get all the columns to cbind together
  clnms <- colnames(tabs[[1]])

  for (i in 2:length(tabs)) {
    if (inherits(tabs[[i]], "qwraps2_qable")) {
      clnms <- c(clnms, colnames(tabs[[i]])[-1])
      tabs[[i]] <- matrix(tabs[[i]][, -1], nrow = nrow(tabs[[1]]))
    } else {
      nms <- colnames(tabs[[i]])
      if (is.null(nms)) {
        nms <- names(tabs)[i]
      }
      if (!is.null(ncol(tabs[[i]])) && length(nms) < ncol(tabs[[i]])) {
        nms <- paste0(nms, 1:ncol(tabs[[i]]))
      }
      clnms <- c(clnms, nms)
      tabs[[i]] <- matrix(tabs[[i]], nrow = nrow(tabs[[1]]))
    }
  }

  rtn <- do.call(cbind, lapply(tabs, unclass))

  colnames(rtn) <- clnms
  attr(rtn, "qable_args") <- attr(tabs[[1]], "qable_args")
  attr(rtn, "qable_args")[["cnames"]] <- clnms
  class(rtn) <- class(tabs[[1]])
  rtn
}

#' @seealso \code{rbind}
#' @export
rbind.qwraps2_qable <- function(..., deparse.level = 1) {
  tabs <- list(...)

  out <- do.call(rbind, args = c(lapply(tabs, unclass), list(deparse.level = deparse.level)))

  attr(out, "qable_args") <- attr(tabs[[1]], "qable_args")
  attr(out, "qable_args")[["rgroup"]] <- do.call(c, lapply(tabs, function(x) attr(x, "qable_args")[["rgroup"]]))
  attr(out, "qable_args")[["rnames"]] <- do.call(c, lapply(tabs, function(x) attr(x, "qable_args")[["rnames"]]))

  class(out) <- class(tabs[[1]])
  out
}
