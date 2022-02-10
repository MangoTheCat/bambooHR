#' @title Retrieve An Employee File
#'
#' @description `get_employee_file` takes 'id' (string), 'file_id', (string), and
#' 'api_version' (string) and then requests and returns data about
#' the corresponding file from the BambooHR API.
#' The file will then be written to the user's working directory if possible.
#'
#' @param id The employee id of the employee.
#' @param file_id The file id of the file to be returned.
#' @param api_version The version of BambooHR API to be used.
#'
#' @return returns a response object.
#'
#' @examples
#' response <- get_employee_file(
#' id = 0,
#' file_id = "480",
#' api_version = "v1"
#' )
#'
#' @author Harry Alexander, \email{harry.alexander@ascent.io}
#'
get_employee_file <- function(id, file_id,
                              api_version = "v1") {
  url <- build_url(api_version = api_version)
  # default to 0 if employee id is not specified
  id <- rlang::maybe_missing(id, default = "0")
  # Default to directory if an individual employee is not specified
  file_id <- rlang::maybe_missing(file_id, default = "view")
  # Glues "/files/file_id" to url returned from build_url call
  url <- glue::glue("{url}/employees/{id}/files/{file_id}")
  response <- get_request(url)
  # Extract file name from 'content-disposition' variable
  file_name <- substring(
    text = response$headers$`content-disposition`,
    first = 23,
    last = nchar(response$headers$`content-disposition`) - 1
  )
  # Attempt to parse content of file as raw binary and write it in wd
  tryCatch({
    response %>%
      httr::content(as = "raw") %>%
      writeBin(file_name)
  }, error = function(e)
    warning("Failed to download file."),
  finally = {
    return(response)
  })
}
