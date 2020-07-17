dir_create <- function(path, showWarnings = FALSE, recursive = TRUE, ...){
  dir.create(path = path, showWarnings = showWarnings, recursive = recursive, ...)
}

load_yaml <- function(path, ...){
  read_yaml(path, ...)
}


save_yaml <- function(x, path, ...){
  write_yaml(x, path, ...)
}


deparse1 <- function(..., collapse = ''){
  paste0(deparse(...), collapse = collapse)
}

rand_string <- function(length = 10){
  paste(sample(c(letters, LETTERS, 0:9), length, replace = TRUE), collapse = '')
}

