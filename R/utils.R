make_id <- function(fn, pkg, call = caller_env()) {
  check_character(fn, call = call)
  check_character(pkg, allow_null = TRUE, call = call)

  if (is.null(pkg)) {
    return(fn)
  }

  pkg_ns_env <- ns_env_safe(pkg)

  if (is.null(pkg_ns_env)) {
    cli_abort("Unable to locate the specified {.arg pkg}", call = call)
  }

  pkg_exports <- as.list(pkg_ns_env[[".__NAMESPACE__."]][["exports"]])

  if (fn %in% pkg_exports) {
    return(paste0(pkg, "::", fn))
  }

  if (!fn %in% names(pkg_ns_env)) {
    cli_abort(
      "Unable to locate function {.fun {fn}} in package  {.pkg {pkg}}'s namespace.",
      call = call
    )
  }

  paste0(pkg, ":::", fn)
}

# adapted from testthat:::local_bindings_rebind
local_bindings_rebind <- function (..., .env, .frame) {
  bindings <- list2(...)
  bindings <- bindings[env_has(.env, names(bindings))]
  if (length(bindings) == 0) {
    return()
  }
  nms <- names(bindings)
  locked <- env_binding_unlock(.env, nms)
  withr::defer(env_binding_lock(.env, nms[locked]), envir = .frame)
  local_bindings(!!!bindings, .env = .env, .frame = .frame)
  invisible()
}

# adapted from testthat::local_mocked_bindings, where .frame is instead
# a ticker (environment) that will close on `untick()`
local_mocked_bindings <- function (..., .package = NULL, .env, .frame) {
  bindings <- list2(...)
  envs <- c(ns_env_safe(.package), search_envs())
  bindings_found <- rep_named(names(bindings), FALSE)
  for (env in envs) {
    local_bindings_rebind(!!!bindings, .env = env, .frame = .frame)
    bindings_found <- bindings_found | env_has(env, names(bindings))
  }

  if (any(!bindings_found)) {
    missing <- names(bindings)[!bindings_found]
    cli::cli_abort("Can't find binding for {.arg {missing}}")
  }
  invisible()
}

ns_env_safe <- function(pkg) {
  tryCatch(ns_env(pkg), error = function(e) NULL)
}
