#' Timings for functions using mocking
#'
#' @description
#' The stopwatch package introduces "tickers," which temporarily mock an
#' inputted function with a wrapper of the function that records the
#' elapsed time of the call to the inputted function and stores it for
#' later exploration.
#'
#' @param fn The un-namespaced name of the function to be timed, as a string.
#' @param pkg The name of the package where mocked functions should be inserted.
#' @param ... Unused, should be empty.
#' @param measure One of `'process'` or `'real'`. Determines which of the
#' outputs of `bench::system_time()` (by the same names) will be logged, where
#' `'process'` is the process CPU usage of the expression evaluation and `'real'`
#' is the wallclock time.
#'
#' @section Workflow:
#'
#' The typical workflow of the package is as follows:
#'
#' * Establish a ticker for a function using [tick()] like
#' `fn_enticked <- tick("fn", "pkg")`.
#' * Run the code that will call `"fn"`.
#' * Retrieve the timings for the call to `"fn"` with [ticks()], like
#' `ticks(fn_enticked)`.
#' * Restore the previous definition of `"fn"` with [untick()], like
#' `untick(fn_enticked)` (or `untick("fn", "pkg")`).
#'
#' @examplesIf FALSE
#' # set up a "ticker" for `stats::lm()`:
#' lm_ticker <- tick("lm", "stats")
#' lm_ticker
#'
#' # the function definition is temporarily modified:
#' stats::lm
#'
#' # ...but it works as usual:
#' lm_res <- stats::lm(mpg ~ ., mtcars)
#' coef(lm_res)
#'
#' # retrieve the timings associated with the ticker
#' ticks(lm_ticker)
#'
#' # timings are logged for every call to the function
#' lm_res2 <- stats::lm(mpg ~ ., mtcars)
#' lm_res3 <- stats::lm(mpg ~ ., mtcars)
#'
#' ticks(lm_ticker)
#'
#' # restore the function to its original definition and clear timings
#' untick(lm_ticker)
#' stats::lm
#'
#' @seealso [untick()] and [ticks()].
#'
#' @export
tick <- function(fn, pkg = NULL, ..., measure = "process") {
  id <- make_id(fn, pkg)
  check_enticked(id)
  arg_match(measure, values = c("process", "real"))

  fn_loc <- get_unmocked_fn(fn, pkg)

  ticker <- new_ticker(id, fn, fn_loc)

  env_really_bind(
    fn_loc[[2]],
    !!fn := entick(ticker, measure)
  )

  ticker
}


#' Restore a function to its original binding
#'
#' @description
#' Given a [ticker][stopwatch::tick()] or a function (and package) name,
#' `untick()` will clear the timings associated with the ticker and restore
#' the definition of the enticked function.
#'
#' @param x Either a ticker or the value passed to the `fn` argument of [tick()].
#' @param pkg If `x` is the value passed to the `fn` argument of [tick()], `pkg`
#' is the value passed to the `pkg` argument of [tick()].
#' @param ... Unused, should be empty.
#'
#' @inheritSection tick Workflow
#'
#' @inherit tick examples
#'
#' @seealso [tick()] and [ticks()].
#'
#' @export
untick <- function(x, ...) {
  UseMethod("untick")
}

#' @rdname untick
#' @export
untick.default <- function(x, ...) {
  cli_abort("No known {.fun untick} method for {.obj_type_friendly {x}}.")
}

#' @rdname untick
#' @export
untick.character <- function(x, pkg = NULL, ...) {
  id <- make_id(x, pkg)

  if (!ticker_is_active(id)) {
    cli_abort("Could not find active ticker for {.arg fn} {x} and \\
               {.arg pkg} {pkg}.")
  }

  clear_ticker(tickers_[[id]])

  invisible()
}

#' @rdname untick
#' @export
untick.ticker <- function(x, ...) {
  id <- as.character(x)
  if (!ticker_is_active(id)) {
    cli_abort("Could not find active ticker for {.arg {id}}.")
  }

  clear_ticker(x)

  invisible()
}

clear_ticker <- function(ticker) {
  env_really_bind(
    ticker_fn_env(ticker),
    !!ticker_fn_name(ticker) := ticker_fn(ticker)
  )
  env_unbind(ticks_, ticker_id(ticker))
  env_unbind(tickers_, ticker_id(ticker))

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
