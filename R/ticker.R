new_ticker <- function(id, fn) {
  ticker <- list()
  rlang::env_bind(ticks_, !!id := ticker)

  structure(list(id, fn), class = "ticker")
}

entick <- function(fn_unmocked, ticker, measure) {
  function(...) {
    timings <-
      bench::system_time({
        res <- eval(call2(fn_unmocked, ...))
      })[[measure]]

    env_bind(
      ticks_,
      !!as.character(ticker) := c(env_get(ticks_, as.character(ticker)), timings)
    )

    res
  }
}

check_enticked <- function(id, env = caller_env()) {
  if (id %in% names(ticks_)) {
    cli_abort(
      c(
        "!" = "{.arg fn} is already enticked.",
        "i" = 'Untick with `untick("{id}")` to clear timings.'
      ),
      .envir = env
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
