#' @title View Data About a Response.
#'
#' @description This function gets the `View` function from the utils package
#' and thus prevents R studio from defaulting to their own viewer function when `View` is called.
#'
#' @param x An R object which can be coerced to a data frame with non-zero numbers of rows and columns.
#' @param title Title for the viewer window.
#'
#' @return NULL.
#'
#' @examples \dontrun{
#  x <- response_view(x = df, title = "dataframe_title")
#'}

response_view <- function(x, title){
  get("View", envir = as.environment("package:utils"))(x, title)
}
