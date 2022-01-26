#' Get employee data by specifying a set of fields.
#'
#' This is suitable for getting basic employee information, including current
#' values for fields that are part of a historical table, like job title,
#' or compensation information. See the fields endpoint for a list of possible
#'  fields.
#'
#'
#' @param id Employee ID (character or numeric). The default of "directory" will give a directory of all
#' employees, along with a table of possible fields. The special employee ID of
#' zero (0) means to use the employee ID associated with the API key (if any).
#'
#' @param fields Character vector of fields to return. To see available fields, run
#' \code{get_employee()}.
#'
#' @param config File path to config file. This may be useful if you want to swap
#' between multiple company domains and API keys. Note that this will set the
#' config file for future calls.
#'
#' @return List. If \code{id} is given then a list of the fields and their values. If
#' \code{id = 'directory'} then a list of (1) the available fields dataframe and (2)
#' the directory dataframe.
#'
#' @author Mark Druffel, \email{mdruffel@propellerpdx.com}
#'
#' @export
get_employee <- function(
  id = "directory", fields = NULL,
  config = .get_config_file()
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
    query <- list(fields = URLencode(paste(fields, collapse = ",")))
  } else {
    query <- NULL
  }

  # Use given config file
  .parse_config(config)

  # Build up URL with query
  url <- build_url()
  url <- glue::glue("{url}/{api}/{id}")
  url <- httr::modify_url(url = url, query = query)

  # Get and parse response
  response <- get_request(url) %>%
    httr::content("parsed")

  # Convert response to a dataframe if using full directory
  if (id == "directory") {
    response <- response %>%
      purrr::map(
        ~ .x %>%
          purrr::transpose() %>%
          tibble::as_tibble() %>%
          tidyr::unnest(cols = everything())
      )
  }

  return(response)

}
