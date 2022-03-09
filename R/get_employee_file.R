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
#' @param suppress_view prevent display of results when file_id = "view", default is FALSE.
#'
#' @return returns a response object.
#'
#' @examples \dontrun{
#' #' response <- get_employee_file(
#' id = 0,
#' file_id = "480",
#' api_version = "v1"
#' )
#'}
#'
#' @importFrom rlang .data
#' @author Harry Alexander, \email{harry.alexander@ascent.io}

get_employee_file <- function(id = "0",
                              file_id = "view",
                              api_version = "v1",
                              suppress_view = FALSE) {
  url <- build_url(api_version = api_version)
  # If ID is given as 0, then use ID associated with API key
  if (as.character(id) == "0") {
    id <- get_employee(0)$id
  }
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
  if (file_id != "view") {
    tryCatch({
      response %>%
        httr::content(as = "raw") %>%
        writeBin(file_name)
      message("Successfully downloaded file to working directory.")
    }, error = function(e)
      warning("Failed to download file."),
    finally = {
      return(response)
    })
  }
  # else file_id is "view" return a dataframe containing all files and categories
  else if (suppress_view == FALSE) {
    content <- httr::content(response)
    if(!length(content$categories) == 0) {
      categories <- content %>%
        purrr::pluck("categories", 1) %>%
        tibble::as_tibble() %>%
        tidyr::unnest_wider(.data$files, names_sep = "_")

      employee <- content %>%
        purrr::pluck("employee", 1) %>%
        tibble::as_tibble()

      # Combine tibbles
      output <- dplyr::left_join(employee, categories, by = character())

      # Rename column names that are ambiguous to user and view table
      output %>%
        dplyr::rename(employee_id = .data$value,
                      category_id = .data$id,
                      category_name = .data$name) %>%
        response_view(title = "Response Data")
    }
    else{
      message("Response from the API returned no files")
    }
  }
  return(response)
}
