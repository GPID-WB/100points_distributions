# source("renv/activate.R")

gca <- function(x, ...) {
  gert::git_commit_all(x, ...)
}

gp <- function(x = NULL, ...) {
  gert::git_push(x, ...)
}

ga <- function(...) {
  gert::git_add(gert::git_status(...)$file)
}

gi <- function() {
  gert::git_info()$upstream
}
gs <- function() {
  gert::git_status()
}

