#' @title Bamboo API get request wrapper for BambooHR tables
#'
#' @description Returns a data structure representing all the table rows for
#'  a given employee and table combination.
#'
#' @param employee_id Character of employee ID to return. A special employee ID of "all",
#'  can also be used which will cause the API to return all rows for all employees in the table.
#'   When specifying "all" employees, the result will include inactive and terminated employees.
#' @param table_name Character of table name. Valid table names can be found in
#' the "table_alias" column return by \code{get_meta("tables")}.
#' @param api_version (optional) - Version of API to use to make request. Default is "v1".
#'
#' @return A [tibble::tibble()] object.
#' @md
#' @references \url{https://documentation.bamboohr.com/reference#get-employee-table-row-1}
#'
#' @export
#'

get_table <- function(employee_id, table_name, api_version = "v1") {

  url <- build_url(
    api_version = api_version,
  )
  url <- glue::glue("{url}/employees/{employee_id}/tables/{table_name}")
  response <- get_request(url)

  return(
  response %>%
    httr::content(as='text', type='json', encoding='UTF-8') %>%
    jsonlite::fromJSON(simplifyDataFrame=TRUE) %>%
    tibble::tibble()
  )
}
