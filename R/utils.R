rand_id <- function(prefix = "step", len = 5) {
  candidates <- c(letters, LETTERS, paste(0:9))
  paste(prefix,
        paste0(sample(candidates, len, replace = TRUE), collapse = ""),
        sep = "_"
  )
}

fn_name <- function(fn) {
  as.character(fn[[length(fn)]])
}

ns_env_name_safe <- function(fn) {
  tryCatch(ns_env_name(fn), error = function(e) NULL)
}
