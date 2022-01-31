#' @title Retrieve An Employee File
#'
#' @description `get_employee_file` takes file_id, (string), id (string) and
#' url_args (list) and then requests
#' and returns data about the corresponding file from the BambooHR API.
#'
#' @param url_args A list containing the required arguments to call `build_url`.
#' company domain (string), api version (string), and base_url (string).
#' @param file_id The file id (string) of the file to be returned.
#' @param id The employee id (string) of the employee.
#'
#' @return returns a response object.
#'
#' @examples
#' response <- get_employee_file(
#' id = 0,
#' file_id = "480",
#' url_args = list("ascent", "v1",
#' "https://api.bamboohr.com/api/gateway.php")
#' )
#'
#' @author Mark Druffel, \email{mdruffel@propellerpdx.com}
#'
get_employee_file <- function(id, file_id,
                             url_args = NULL){
  if (!is.null(url_args)) {
    stopifnot(is.list(url_args))
  } else {
    url_args <- list()
  }url <- do.call(build_url, url_args)
  # default to 0 if employee id is not specified
  id <- rlang::maybe_missing(id, default = "0")
  # Default to directory if an individual employee is not specified
  file_id <- rlang::maybe_missing(file_id, default = "view")
  # Glues "/files/file_id" to url returned from build_url call
  url <- glue::glue("{url}/employees/{id}/files/{file_id}")
  response <- get_request(url)
  return(response)
}
