new_ticker <- function(id, fn_name, fn_loc) {
  res <-
    structure(
      list(id = id, fn_name = fn_name, fn = fn_loc[[1]], fn_env = fn_loc[[2]]),
      class = "ticker"
    )

  rlang::env_bind(ticks_, !!id := list())
  rlang::env_bind(tickers_, !!id := res)

  res
}

is_ticker <- function(ticker) {
  isTRUE(inherits(ticker, "ticker"))
}

ticker_id <- function(ticker) {
  as.character(ticker)
}

ticker_fn_name <- function(ticker) {
  ticker[["fn_name"]]
}

ticker_fn <- function(ticker) {
  ticker[["fn"]]
}

ticker_fn_env <- function(ticker) {
  ticker[["fn_env"]]
}

entick <- function(ticker, measure) {
  function(...) {
    timings <-
      bench::system_time({
        res <- eval(call2(ticker_fn(ticker), ...))
      })[[measure]]

    ticks_[[as.character(ticker)]] <- c(ticks_[[as.character(ticker)]], timings)

    res
  }
}

check_enticked <- function(id, call = caller_env()) {
  if (id %in% names(ticks_)) {
    cli_abort(
      c(
        "!" = "{.arg fn} is already enticked.",
        "i" = 'Untick with `untick("{id}")` to clear timings.'
      ),
      .envir = call
    )
  }
}

#' @export
print.ticker <- function(x, ...) {
  cat(format(x, ...))
}

#' @export
format.ticker <- function(x, ...) {
  cli::cli_text(
    "A {.cls ticker} for {.fun {as.character(x)}}."
  )
}

#' @export
as.character.ticker <- function(x) {
  as.character(x[[1]])
}
