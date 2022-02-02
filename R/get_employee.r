#' Get employee data by specifying a set of fields.
#'
#' This is suitable for getting basic employee information, including current
#' values for fields that are part of a historical table, like job title,
#' or compensation information. See the fields endpoint for a list of possible
#'  fields.
#'
#'
#' @param id Employee IDs (character or numeric). The default of "directory"
#' will give a directory of all employees, along with a table of possible fields.
#' The special employee ID of zero (0) means to use the employee ID associated
#' with the API key (if any).
#'
#' @param fields Character vector of fields to return. To see all available fields,
#' see the column names of \code{get_employee()}.
#'
#' @param only_current Setting to false will return future dated values from
#' history table fields.
#'
#' @param verbose Logical, default FALSE. Whether to return the fields dataframe
#' when [id = "directory"].
#'
#' @return Tibble (dataframe), unless [verbose = TRUE] and [id = "directory"],
#'  in which case a list with the fields dataframe and the directory dataframe.
#'
#' @examples \dontrun{
#' get_employee()
#' get_employee(0, fields = c("firstName", "lastName"))
#' get_employee(c(0, 1, 2), fields = c("firstName", "lastName", "jobTitle"))
#' }
#'
#' @export
get_employee <- function(
  id = "directory",
  fields = NULL,
  only_current = FALSE,
  verbose = FALSE,
  api_version = "v1"
) {

  # Define endpoint
  api <- "employees"

  # Error handling ---
  if (id == "directory" && !is.null(fields)) {
    warning("fields supplied, however id is given as 'directory'. All fields will be returned.", call. = FALSE)
  }


  # If fields are given, encode special characters to be URL-friendly
  # Also give fields in specified way (comma-separated list)
  if (!rlang::is_null(fields)) {
    query <- list(fields = URLencode(paste(fields, collapse = ",")),
                  onlyCurrent = URLencode(stringr::str_to_lower(only_current)))
  } else {
    query <- NULL
  }

  # Build up URL with query
  urls <- purrr::map(id, function(id) {
    url <- build_url(api_version = api_version)
    url <- glue::glue("{url}/{api}/{id}")
    url <- httr::modify_url(url = url, query = query)
    url
  })


  # Get and parse responses
  responses <- purrr::map(urls, function(url) {
    get_request(url) %>%
      httr::content("parsed")
  })

  # Convert response to a dataframe
  if (identical(id, "directory")) {
    responses <- responses %>%
      purrr::flatten() %>%
      purrr::map(~ .x %>%
                   purrr::transpose() %>%
                   tibble::as_tibble() %>%
                   tidyr::unnest(cols = everything())
      ) %>%
      purrr::when(!verbose ~ .[["employees"]],
                  ~ .
      )
  } else {
    responses <- responses %>%
      dplyr::bind_rows()
  }

  return(responses)

}
