#' @export
ticks <- function(x, ...) {
  UseMethod("ticks")
}

#' @export
ticks.default <- function(x, ...) {
  cli_abort("No known {.fun ticks} method for {.obj_type_friendly {x}}.")
}

#' @export
ticks.ticker <- function(x, ...) {
  ticks_[[as.character(x)]]
}

#' @export
ticks.character <- function(x, pkg = NULL, ...) {
  id <- make_id(x, pkg)
  ticks_[[id]]
}

# `ticks_` is an internal environment that the package uses to track
# `entick()`ed functions.
ticks_ <- rlang::new_environment()

tickers_ <- rlang::new_environment()
