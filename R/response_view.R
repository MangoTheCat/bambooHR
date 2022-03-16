#' @title View Data About a Response.
#'
#' @description This function uses the `View` function from Rstudio if available,
#' otherwise it uses the utils::View function.
#'
#' @param x An R object which can be coerced to a data frame with non-zero numbers of rows and columns.
#' @param title Title for the viewer window.
#'
#' @return NULL.
#'

response_view <- function(x, title){
  get("View", envir = as.environment("package:utils"))(x, title)
}
