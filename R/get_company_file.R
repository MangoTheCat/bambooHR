#' @title Retrieve A Company File.
#'
#' @description `get_company_file` takes a file_id (string) and
#' api version (string) as arguments and then requests
#' and returns data about the corresponding file from the BambooHR API,
#' the file is then downloaded if possible.
#'
#' @param file_id The ID of the file to get from BambooHR.
#' @param api_version version of the BambooHR API.
#'
#' @return returns a response object.
#'
#' @examples
#' response <- get_company_file(
#' "480",
#' api_version = "v1",
#' )
#'
#'@author Harry Alexander, \email{harry.alexander@ascent.io}

get_company_file <- function(file_id,
                              api_version = "v1") {
  url <- build_url(api_version = api_version)
  # Default to directory if an individual employee is not specified
  file_id <- rlang::maybe_missing(file_id, default = "view")
  # Glues "/files/file_id" to url returned from build_url call
  url <- glue::glue("{url}/files/{file_id}")
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
