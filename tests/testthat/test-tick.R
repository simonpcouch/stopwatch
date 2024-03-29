skip_if_not_installed("withr")

test_that("tick works (external package, ticker interface)", {
  original_def <- stats::lm
  original_res <- stats::lm(mpg ~ ., mtcars)

  withr::defer(tryCatch(untick("lm", "stats"), error = function(e) {NULL}))

  expect_silent(lm_ticker <- tick("lm", "stats"))
  expect_s3_class(lm_ticker, "ticker")
  expect_snapshot(lm_ticker)

  expect_true(ticker_is_active(lm_ticker))

  expect_snapshot(
    stats::lm,
    transform = function(lines) {
      lines[grepl("environment", "", lines, fixed = TRUE)]
    }
  )
  expect_named(environment(stats::lm), c("ticker", "measure"))
  expect_equal(
    rlang::env_name(rlang::env_parent(environment(stats::lm))),
    "namespace:stopwatch"
  )

  expect_no_condition(ticker_res <- stats::lm(mpg ~ ., mtcars))
  ticker_res$call <- NULL
  original_res$call <- NULL
  expect_equal(ticker_res, original_res)

  expect_no_condition(lm_ticker_res <- ticks(lm_ticker))
  expect_length(lm_ticker_res, 1)

  expect_no_condition({
    lm_res2 <- stats::lm(mpg ~ ., mtcars)
    lm_res3 <- stats::lm(mpg ~ ., mtcars)
  })

  expect_no_condition(lm_ticker_res <- ticks(lm_ticker))
  expect_length(lm_ticker_res, 3)

  expect_no_condition(untick(lm_ticker))

  expect_equal(original_def, stats::lm)
  expect_false(ticker_is_active(lm_ticker))

  # already unticked
  expect_snapshot(
    error = TRUE,
    untick(lm_ticker)
  )

  # expect_snapshot(
  #   error = TRUE,
  #   ticks(lm_ticker)
  # )
})
