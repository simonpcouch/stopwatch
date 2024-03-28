#' @export
tick <- function(fn, ..., .measure = "real", .env = caller_env()) {
  fn_sub <- fn_name(substitute(fn))
  arg_match(.measure, values = c("process", "real"))

  ticker <- new_ticker(fn_sub)

  fn_ns_env <- ns_env_name_safe(fn)
  check_enticked(fn_ns_env, fn_sub)

  testthat::local_mocked_bindings(
    !!fn_sub := entick(fn, ticker, .measure),
    .package = ,
    .env = .env
  )

  ticker
}

#' @export
untick <- function(ticker) {
  env_unbind(ticks_, as.character(ticker))

  invisible(ticker)
}

entick <- function(fn, ticker, measure) {
  id <- as.character(ticker)

  new_fn <-
    function(...) {
      timings <-
        bench::system_time({
          res <- fn(...)
        })[[measure]]


      ticks_$id <- c(ticks_$id, timings)

      res
    }

  new_fn
}

new_ticker <- function(fn_sub) {
  ticker <- list()
  rlang::env_bind(ticks_, !!fn_sub := ticker)

  structure(fn_sub, class = "ticker")
}

check_enticked <- function(fn_ns_env, fn_sub, env = caller_env()) {
  if (identical(fn_ns_env, "stopwatch")) {
    cli_abort(
      c(
      "!" = "{.arg fn} is already enticked.",
      "i" = 'Untick with `untick("{fn_sub}")` to clear timings.'
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





