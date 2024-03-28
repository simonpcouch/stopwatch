#' @export
ticks <- function(ticker) {
  ticks_[[as.character(ticker)]]
}

# `ticks_` is an internal environment that the package uses to track
# `entick()`ed functions.
ticks_ <- rlang::new_environment()
