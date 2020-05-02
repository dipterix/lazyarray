dir_create <- function(path, showWarnings = FALSE, recursive = TRUE, ...){
  dir.create(path = path, showWarnings = showWarnings, recursive = recursive, ...)
}

load_yaml <- function(path, ...){
  yaml::read_yaml(path, ...)
}


save_yaml <- function(x, path, ...){
  yaml::write_yaml(x, path, ...)
}


deparse1 <- function(..., collapse = ''){
  paste0(deparse(...), collapse = collapse)
}
