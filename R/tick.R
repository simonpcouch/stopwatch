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
