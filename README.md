
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stopwatch

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/stopwatch)](https://CRAN.R-project.org/package=stopwatch)
[![R-CMD-check](https://github.com/simonpcouch/stopwatch/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/simonpcouch/stopwatch/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/simonpcouch/stopwatch/branch/master/graph/badge.svg)](https://app.codecov.io/gh/simonpcouch/stopwatch?branch=master)
<!-- badges: end -->

stopwatch is an experimental R package for high-precision profiling. The
package introduces “tickers,” which temporarily mock an inputted
function with a wrapper of the function that records the elapsed time of
the call to the inputted function and stores it for later exploration.

**This package is experimental and quite unsafe. Use with caution!** I
wrote this package for my own purposes and it is quite buggy outside of
the context I’m interested in using it in.

## Installation

You can install the development version of stopwatch like so:

``` r
pak::pak("simonpcouch/stopwatch")
```

## Example

As an example, we’ll fit a linear model, timing how long the call to
`stats::lm()` takes. First, loading the package:

``` r
library(stopwatch)
```

Now, we set up a “ticker” for `stats::lm()` like so:

``` r
lm_ticker <- tick("lm", "stats")
```

The result is a ticker:

``` r
lm_ticker
#> A <ticker> for `stats::lm()`.
```

Also, though, `tick()` has provided a new binding to the `stats::lm()`
function:

``` r
stats::lm
#> function (...) 
#> {
#>     timings <- bench::system_time({
#>         res <- eval(call2(ticker_fn(ticker), ...))
#>     })[[measure]]
#>     ticks_[[as.character(ticker)]] <- c(ticks_[[as.character(ticker)]], 
#>         timings)
#>     res
#> }
#> <bytecode: 0x144266c60>
#> <environment: 0x144264c98>
```

The new binding for `stats::lm()` calls the original `stats::lm()`
definition, but wraps that call in `bench::system_time()`, which returns
high-precision `timings` for its input. It then stores the resulting
`timings` before returning the output from the original `stats::lm()`
call.

So, moving on to fitting that linear model:

``` r
lm_res <- stats::lm(mpg ~ ., mtcars)
```

Again, the result is the same as it would be if we had called
`stats::lm()` without establishing a ticker for it (albeit with a
different call):

``` r
coef(lm_res)
#> (Intercept)         cyl        disp          hp        drat          wt 
#> 12.30337416 -0.11144048  0.01333524 -0.02148212  0.78711097 -3.71530393 
#>        qsec          vs          am        gear        carb 
#>  0.82104075  0.31776281  2.52022689  0.65541302 -0.19941925
```

The benefit, though, is that we now have information on how long the
call to `stats::lm()` took:

``` r
ticks(lm_ticker)
#> [[1]]
#> [1] 0.002263
```

For as long as the function is enticked, it will record timings for
every call:

``` r
lm_res2 <- stats::lm(mpg ~ ., mtcars)
lm_res3 <- stats::lm(mpg ~ ., mtcars)

ticks(lm_ticker)
#> [[1]]
#> [1] 0.002263
#> 
#> [[2]]
#> [1] 0.000611
#> 
#> [[3]]
#> [1] 0.000562
```

To restore the function to it’s previous definition (and erase the
timings associated with the ticker), use `untick()`:

``` r
untick(lm_ticker)

stats::lm
#> function (formula, data, subset, weights, na.action, method = "qr", 
#>     model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE, 
#>     contrasts = NULL, offset, ...) 
#> {
#>     ret.x <- x
#>     ret.y <- y
#>     cl <- match.call()
#>     mf <- match.call(expand.dots = FALSE)
#>     m <- match(c("formula", "data", "subset", "weights", "na.action", 
#>         "offset"), names(mf), 0L)
#>     mf <- mf[c(1L, m)]
#>     mf$drop.unused.levels <- TRUE
#>     mf[[1L]] <- quote(stats::model.frame)
#>     mf <- eval(mf, parent.frame())
#>     if (method == "model.frame") 
#>         return(mf)
#>     else if (method != "qr") 
#>         warning(gettextf("method = '%s' is not supported. Using 'qr'", 
#>             method), domain = NA)
#>     mt <- attr(mf, "terms")
#>     y <- model.response(mf, "numeric")
#>     w <- as.vector(model.weights(mf))
#>     if (!is.null(w) && !is.numeric(w)) 
#>         stop("'weights' must be a numeric vector")
#>     offset <- model.offset(mf)
#>     mlm <- is.matrix(y)
#>     ny <- if (mlm) 
#>         nrow(y)
#>     else length(y)
#>     if (!is.null(offset)) {
#>         if (!mlm) 
#>             offset <- as.vector(offset)
#>         if (NROW(offset) != ny) 
#>             stop(gettextf("number of offsets is %d, should equal %d (number of observations)", 
#>                 NROW(offset), ny), domain = NA)
#>     }
#>     if (is.empty.model(mt)) {
#>         x <- NULL
#>         z <- list(coefficients = if (mlm) matrix(NA_real_, 0, 
#>             ncol(y)) else numeric(), residuals = y, fitted.values = 0 * 
#>             y, weights = w, rank = 0L, df.residual = if (!is.null(w)) sum(w != 
#>             0) else ny)
#>         if (!is.null(offset)) {
#>             z$fitted.values <- offset
#>             z$residuals <- y - offset
#>         }
#>     }
#>     else {
#>         x <- model.matrix(mt, mf, contrasts)
#>         z <- if (is.null(w)) 
#>             lm.fit(x, y, offset = offset, singular.ok = singular.ok, 
#>                 ...)
#>         else lm.wfit(x, y, w, offset = offset, singular.ok = singular.ok, 
#>             ...)
#>     }
#>     class(z) <- c(if (mlm) "mlm", "lm")
#>     z$na.action <- attr(mf, "na.action")
#>     z$offset <- offset
#>     z$contrasts <- attr(x, "contrasts")
#>     z$xlevels <- .getXlevels(mt, mf)
#>     z$call <- cl
#>     z$terms <- mt
#>     if (model) 
#>         z$model <- mf
#>     if (ret.x) 
#>         z$x <- x
#>     if (ret.y) 
#>         z$y <- y
#>     if (!qr) 
#>         z$qr <- NULL
#>     z
#> }
#> <bytecode: 0x144321518>
#> <environment: namespace:stats>
```

In this example, of course, we could have just called `stats::lm()`
inside of `bench::system_time()` ourselves and gone on our way. The
utility of the package arises when the enticked function is called
inside of other functions, such as if we fitted the linear model using
the parsnip wrapper `parsnip::linear_reg()` or resampled the model using
`tune::fit_resamples()`.

## Limitations

- While `stopwatch()` looks to `testthat::local_mocked_bindings()` in
  most of its principles, it diverges in a couple places. Most
  importantly, tickers do not clear themselves via exit handlers and
  must be cleared manually. It *is* possible to
  `withr::defer({untick(ticker)})` after creating a `ticker`, and I
  recommend doing so.

- The package does not play nicely with recursive functions.

- The package struggles with search paths. I use the package to entick
  functions from external, non-base packages, and it’s most effective
  for that use case.

## Alternatives

You may be interested in more principled alternatives to this package.

- The initial approach for this package was inspired by
  `testthat::local_mocked_bindings()`, which implements a principled
  mocking framework.
- Generally, I’d recommending using `profvis::profvis()` for profiling
  with R.
- When using the base R Rprof, I like to wrap it as follows:

``` r
prof_tbl <- function(expr, ..., interval = 0.01) {
  file <- withr::local_tempfile()

  evalq({
    on.exit(Rprof(NULL), add = TRUE, after = FALSE)
    Rprof(file, ..., interval = interval, filter.callframes = TRUE)
    expr
  })

  out <- summaryRprof(file)

  out_tbl <- tibble::as_tibble(out$by.total, rownames = "fn")

  out_tbl |>
    dplyr::mutate(fn = gsub("\"", "", fn)) |>
    dplyr::arrange(dplyr::desc(self.pct))
}
```
