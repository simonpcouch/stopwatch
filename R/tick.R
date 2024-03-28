#' @export
tick <- function(fn, pkg = NULL, ..., .measure = "process", .env = caller_env()) {
  id <- make_id(fn, pkg)
  check_enticked(id)
  arg_match(.measure, values = c("process", "real"))

  fn_unmocked <- env_get(ns_env_safe(pkg) %||% .env, fn)

  ticker <- new_ticker(id, fn_unmocked)

  testthat::local_mocked_bindings(
    !!fn := entick(ticker, .measure),
    .package = pkg,
    .env = .env
  )

  ticker
}

#' @export
untick <- function(x) {
  check_ticker(x)
  ticker_id <- as.character(x)

  if (ticker_id %in% names("ticks_")) {
    cli_abort("Could not find registered ticker {.arg {ticker_id}}")
  }

  env_unbind(ticks_, ticker_id)

  invisible(x)
}

check_ticker <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (!is_ticker(x)) {
    cli_abort(
      "{.arg arg} must be a ticker but is {.obj_type_friendly {x}}",
      call = call
    )
  }
}
