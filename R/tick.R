#' @export
tick <- function(fn, pkg = NULL, ..., .measure = "process", .env = caller_env()) {
  id <- make_id(fn, pkg)
  check_enticked(id)
  arg_match(.measure, values = c("process", "real"))

  fn_unmocked <-
    env_get(
      ns_env_safe(pkg) %||% .env,
      fn,
      inherit = TRUE,
      last = base::topenv()
    )

  ticker <- new_ticker(id, fn_unmocked)

  local_mocked_bindings(
    !!fn := entick(ticker, .measure),
    .package = pkg,
    .env = ns_env_safe(pkg) %||% .env,
    .frame = ticks_[[as.character(ticker)]]
  )

#   local_bindings_rebind(
#     !!fn := entick(ticker, .measure),
#     .env = ns_env(pkg),
#     .frame = ticker
#   )

  ticker
}

#' @export
untick <- function(x, ...) {
  UseMethod("untick")
}

#' @export
untick.default <- function(x, ...) {
  cli_abort("No known {.fun untick} method for {.obj_type_friendly {x}}.")
}

#' @export
untick.character <- function(x, pkg = NULL, ...) {
  id <- make_id(x, pkg)

  if (id %in% names("ticks_")) {
    cli_abort("Could not find registered ticker for {.arg fn} {x} and \\
               {.arg pkg} {pkg}.")
  }

  env_unbind(ticks_, id)

  invisible(x)
}

#' @export
untick.ticker <- function(x, ...) {
  id <- as.character(x)
  if (id %in% names("ticks_")) {
    cli_abort("Could not find registered ticker for {.arg {id}}.")
  }

  env_unbind(ticks_, id)

  invisible(x)
}

# check_ticker <- function(x, arg = caller_arg(x), call = caller_env()) {
#   if (!is_ticker(x)) {
#     cli_abort(
#       "{.arg arg} must be a ticker but is {.obj_type_friendly {x}}.",
#       call = call
#     )
#   }
# }
