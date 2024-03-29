% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ticks.R
\name{ticks}
\alias{ticks}
\title{Retrieve the timings associated with a ticker}
\usage{
ticks(x, ...)
}
\arguments{
\item{x}{Either a ticker or the value passed to the \code{fn} argument of \code{\link[=tick]{tick()}}.}

\item{...}{Unused, should be empty.}
}
\description{
Given a \link[=tick]{ticker} or a function (and package) name,
\code{ticks()} will return the timings associated with the ticker.
}
\section{Workflow}{


The typical workflow of the package is as follows:
\itemize{
\item Establish a ticker for a function using \code{\link[=tick]{tick()}} like
\code{fn_enticked <- tick("fn", "pkg")}.
\item Run the code that will call \code{"fn"}.
\item Retrieve the timings for the call to \code{"fn"} with \code{\link[=ticks]{ticks()}}, like
\code{ticks(fn_enticked)}.
\item Restore the previous definition of \code{"fn"} with \code{\link[=untick]{untick()}}, like
\code{untick(fn_enticked)} (or \code{untick("fn", "pkg")}).
}
}

\examples{
# set up a "ticker" for `stats::lm()`:
lm_ticker <- tick("lm", "stats")
lm_ticker

# the function definition is temporarily modified:
stats::lm

# ...but it works as usual:
lm_res <- stats::lm(mpg ~ ., mtcars)
coef(lm_res)

# retrieve the timings associated with the ticker
ticks(lm_ticker)

# timings are logged for every call to the function
lm_res2 <- stats::lm(mpg ~ ., mtcars)
lm_res3 <- stats::lm(mpg ~ ., mtcars)

ticks(lm_ticker)

# restore the function to its original definition and clear timings
untick(lm_ticker)
stats::lm

}
\seealso{
\code{\link[=tick]{tick()}} and \code{\link[=untick]{untick()}}.
}