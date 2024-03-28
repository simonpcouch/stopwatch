make_id <- function(fn, pkg) {
  if (is.null(pkg)) {
    return(fn)
  }

  pkg_ns_env <- ns_env(pkg)
  pkg_exports <- as.list(pkg_ns_env[[".__NAMESPACE__."]][["exports"]])

  if (fn %in% pkg_exports) {
    return(paste0(pkg, "::", fn))
  }

  paste0(pkg, ":::", fn)
}

fn_name <- function(fn) {
  as.character(fn[[length(fn)]])
}

ns_env_safe <- function(pkg) {
  tryCatch(ns_env(pkg), error = function(e) NULL)
}
