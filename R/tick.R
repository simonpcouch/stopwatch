#' @export
tick <- function(fn, pkg = NULL, ..., .measure = "process", .env = caller_env()) {
  id <- make_id(fn, pkg)
  check_enticked(id)
  arg_match(.measure, values = c("process", "real"))

  fn_loc <- get_unmocked_fn(fn, pkg)

  ticker <- new_ticker(id, fn, fn_loc)

  env_really_bind(
    fn_loc[[2]],
    !!fn := entick(ticker, .measure)
  )

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

  clear_ticker(tickers_[[id]])

  invisible()
}

#' @export
untick.ticker <- function(x, ...) {
  id <- as.character(x)
  if (id %in% names("tickers_")) {
    cli_abort("Could not find registered ticker for {.arg {id}}.")
  }

  clear_ticker(x)

  invisible()
}

clear_ticker <- function(ticker) {
  env_really_bind(
    ticker_fn_env(ticker),
    !!ticker_fn_name(ticker) := ticker_fn(ticker)
  )
  ticks_[[ticker_id(ticker)]] <- NULL
  tickers_[[ticker_id(ticker)]] <- NULL

  invisible()
}

# check_ticker <- function(x, arg = caller_arg(x), call = caller_env()) {
#   if (!is_ticker(x)) {
#     cli_abort(
#       "{.arg arg} must be a ticker but is {.obj_type_friendly {x}}.",
#       call = call
#     )
#   }
# }
