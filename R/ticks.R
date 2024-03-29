#' Retrieve the timings associated with a ticker
#'
#' @description
#' Given a [ticker][stopwatch::tick()] or a function (and package) name,
#' `ticks()` will return the timings associated with the ticker.
#'
#' @inheritParams untick
#'
#' @inheritSection tick Workflow
#'
#' @inherit tick examples
#'
#' @seealso [tick()] and [untick()].
#'
#' @export
ticks <- function(x, ...) {
  UseMethod("ticks")
}

#' @rdname ticks
#' @export
ticks.default <- function(x, ...) {
  cli_abort("No known {.fun ticks} method for {.obj_type_friendly {x}}.")
}

#' @rdname ticks
#' @export
ticks.ticker <- function(x, ...) {
  unlist(ticks_[[as.character(x)]])
}

#' @rdname ticks
#' @export
ticks.character <- function(x, pkg = NULL, ...) {
  id <- make_id(x, pkg)
  unlist(ticks_[[id]])
}

# `ticks_` is an internal environment that the package uses to track
# `entick()`ed functions.
ticks_ <- rlang::new_environment()

tickers_ <- rlang::new_environment()
