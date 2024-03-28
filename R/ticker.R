new_ticker <- function(id, fn) {
  ticker_env <- new_environment()
  rlang::env_bind(ticks_, !!id := ticker_env)

  structure(list(id = id, fn = fn), class = "ticker")
}

is_ticker <- function(ticker) {
  isTRUE(inherits(ticker, "ticker"))
}

ticker_id <- function(ticker) {
  as.character(ticker)
}

ticker_fn <- function(ticker) {
  ticker[["fn"]]
}

entick <- function(ticker, measure) {
  function(...) {
    timings <-
      bench::system_time({
        res <- eval_bare(call2(ticker_fn(ticker), ...))
      })[[measure]]

    cur_ticker <- ticks_[[as.character(ticker)]]
    env_bind(cur_ticker, !!as.character(length(cur_ticker) + 1) := timings)

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
  as.character(x[["id"]])
}
