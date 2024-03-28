#' @export
tick <- function(fn, pkg = NULL, ..., .measure = "process", .env = caller_env()) {
  id <- make_id(fn, pkg)
  arg_match(.measure, values = c("process", "real"))

  check_enticked(id)

  fn_unmocked <- env_get(ns_env_safe(pkg) %||% .env, fn)

  ticker <- new_ticker(id, fn_unmocked)

  testthat::local_mocked_bindings(
    !!fn := entick(fn_unmocked, ticker, .measure),
    .package = pkg,
    .env = .env
  )

  ticker
}

#' @export
untick <- function(x) {
  env_unbind(ticks_, as.character(x))

  invisible(x)
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

new_ticker <- function(id, fn) {
  ticker <- list()
  rlang::env_bind(ticks_, !!id := ticker)

  structure(list(id, fn), class = "ticker")
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




