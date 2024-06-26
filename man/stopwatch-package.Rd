% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stopwatch-package.R
\docType{package}
\name{stopwatch-package}
\alias{stopwatch}
\alias{stopwatch-package}
\title{stopwatch: High Precision Timings Using Mocking}
\description{
Introduces ticker objects, which temporarily mock an inputted function with a wrapper of the function that records the elapsed time of the call to the inputted function and stores it for later exploration.
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

\seealso{
Useful links:
\itemize{
  \item \url{https://github.com/simonpcouch/stopwatch}
  \item Report bugs at \url{https://github.com/simonpcouch/stopwatch/issues}
}

}
\author{
\strong{Maintainer}: Simon Couch \email{simonpatrickcouch@gmail.com} (\href{https://orcid.org/0000-0001-5676-5107}{ORCID})

}
\keyword{internal}
