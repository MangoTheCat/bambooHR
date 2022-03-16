#' @title Bamboo API get request wrapper for BambooHR tables
#'
#' @description Returns a data structure representing all the table rows for
#'  a given employee and table combination.
#'
#' @param employee_id Character of employee ID to return. A special employee ID of "all",
#'  can also be used which will cause the API to return all rows for all employees in the table.
#'   When specifying "all" employees, the result will include inactive and terminated employees.
#'   In addition, the special employee ID of 0 is the employee ID associated
#'   with the API key being used.
#'
#' @param table_name Character of table name. Valid table names can be found in
#' the "parent_alias" column return by \code{get_meta("tables")}.
#'
#' @param api_version (optional) - Version of API to use to make request. Default is "v1".
#'
#' @return A [tibble::tibble()] object.
#' @md
#' @references \url{https://documentation.bamboohr.com/reference#get-employee-table-row-1}
#'
#' @export
#'
#' @examples \dontrun{
#' get_table(employee_id = "all", table_name = "jobInfo")
#'
#' get_table(employee_id = 0, table_name = "jobInfo")
#'
#' get_table(0, "compensation")
#' }
get_table <- function(employee_id, table_name, api_version = "v1") {

  # Error handling
  if (length(employee_id) > 1) {
    stop(paste0("Please give a single employee ID - you gave ",
                paste(employee_id, collapse = ", ")), call. = FALSE)
  } else if (length(table_name) > 1) {
    stop(paste0("Please give a single table name - you gave ",
                paste(table_name, collapse = ", ")), call. = FALSE)
  }

  # If ID is given as 0, then use ID associated with API key
  if(employee_id == 0) {
    employee_id <- get_employee(0)$id
  }

  # Build up URL ensuring it is formatted correctly and characters are
  # url-friendly
  url <- build_url(api_version = api_version)
  url <- glue::glue("{url}/employees/{employee_id}/tables/{table_name}")
  url <- httr::modify_url(url)

  # Get API response
  response <- get_request(url)

  # Unnest response into a clean dataframe for easy use
  response_df <- response %>%
    httr::content(as='text', type='json', encoding='UTF-8') %>%
    jsonlite::fromJSON(simplifyDataFrame=TRUE) %>%
    tibble::tibble() %>%
    tidyr::unnest(tidyr::everything(), names_sep = "_") %>%
    janitor::clean_names()

  return(response_df)
}
