% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat-stepribbon.R
\name{stat_stepribbon}
\alias{stat_stepribbon}
\title{Step ribbon statistic}
\usage{
stat_stepribbon(
  mapping = NULL,
  data = NULL,
  geom = "ribbon",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  direction = "hv",
  ...
)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[ggplot2:aes]{aes()}}. If specified and
\code{inherit.aes = TRUE} (the default), it is combined with the default mapping
at the top level of the plot. You must supply \code{mapping} if there is no plot
mapping.}

\item{data}{The data to be displayed in this layer. There are three
options:

If \code{NULL}, the default, the data is inherited from the plot
data as specified in the call to \code{\link[ggplot2:ggplot]{ggplot()}}.

A \code{data.frame}, or other object, will override the plot
data. All objects will be fortified to produce a data frame. See
\code{\link[ggplot2:fortify]{fortify()}} for which variables will be created.

A \code{function} will be called with a single argument,
the plot data. The return value must be a \code{data.frame}, and
will be used as the layer data. A \code{function} can be created
from a \code{formula} (e.g. \code{~ head(.x, 10)}).}

\item{geom}{which geom to use; defaults to \code{ribbon}}

\item{position}{Position adjustment, either as a string naming the adjustment
(e.g. \code{"jitter"} to use \code{position_jitter}), or the result of a call to a
position adjustment function. Use the latter if you need to change the
settings of the adjustment.}

\item{na.rm}{If \code{FALSE}, the default, missing values are removed with
a warning. If \code{TRUE}, missing values are silently removed.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2:borders]{borders()}}.}

\item{direction}{\code{hv} for horizontal-vertical steps, \code{vh} for
vertical-horizontal steps}

\item{...}{Other arguments passed on to \code{\link[ggplot2:layer]{layer()}}. These are
often aesthetics, used to set an aesthetic to a fixed value, like
\code{colour = "red"} or \code{size = 3}. They may also be parameters
to the paired geom/stat.}
}
\description{
Provides stair step values for ribbon plots (Copied this from the
https://github.com/hrbrmstr/ggalt version 0.6.0, which is not yet on CRAN.
Some minor modifications to the file have been made).
}
\examples{
x <- 1:10
df <- data.frame(x=x, y=x+10, ymin=x+7, ymax=x+12)

# horizontal-vertical steps (default)
gg <- ggplot2::ggplot(df, ggplot2::aes(x, y))
gg <- gg + ggplot2::geom_ribbon(ggplot2::aes(ymin=ymin, ymax=ymax),
                                stat="stepribbon", fill="#b2b2b2",
                                direction="hv")
gg <- gg + ggplot2::geom_step(color="#2b2b2b")
gg

# vertical-horizontal steps (default)
gg <- ggplot2::ggplot(df, ggplot2::aes(x, y))
gg <- gg + ggplot2::geom_ribbon(ggplot2::aes(ymin=ymin, ymax=ymax),
                                stat="stepribbon", fill="#b2b2b2",
                                direction="vh")
gg <- gg + ggplot2::geom_step(color="#2b2b2b")
gg

# The same plot calling stat_stepribbon directly
gg <- ggplot2::ggplot(df, ggplot2::aes(x, y))
gg <- gg + stat_stepribbon(mapping = ggplot2::aes(ymin=ymin, ymax=ymax),
                           fill="#b2b2b2", direction="vh")
gg <- gg + ggplot2::geom_step(color="#2b2b2b")
gg

}
\references{
\url{https://groups.google.com/forum/?fromgroups=#!topic/ggplot2/9cFWHaH1CPs}
}
