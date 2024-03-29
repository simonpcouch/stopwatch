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

ns_env_safe <- function(pkg) {
  tryCatch(ns_env(pkg), error = function(e) NULL)
}

env_get_safe <- function(env, nm) {
  tryCatch(env_get(env, nm), error = function(e) NULL)
}

get_unmocked_fn <- function(fn, pkg, call = caller_env()) {
  for (env in c(ns_env_safe(pkg), global_env(), base_env())) {
    res <- env_get_safe(env, fn)
    if (!is.null(res)) {
      return(list(res, env))
    }
  }

  cli_abort("Unable to locate {.fun {fn}}.", call = call)
}

env_really_bind <- function(.env, ...) {
  bindings <- list2(...)
  locked <- env_binding_are_locked(.env, names(bindings))
  env_binding_unlock(.env, names(bindings)[locked])
  env_bind(.env, !!!bindings)
  env_binding_lock(.env, names(bindings)[locked])
}

ticker_is_active <- function(ticker) {
  isTRUE(ticker_id(ticker) %in% names(ticks_))
}
