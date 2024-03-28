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
