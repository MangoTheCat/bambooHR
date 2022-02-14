#' @title Bamboo API get request wrapper for BambooHR tables
#'
#' @description  Submits a get request to retrieve a specified table from the system, valid tables are retreived from the
#' meta data API
#'
#' @param user Bamboo api user id, register in Bamboo "API Keys"
#' @param password Bamboo login password
#' @param verbose a logical; indicates if detailed output from httr calls should be provided; default FALSE
#' @param table_alias the a alis of the table name requested, must be valid from the meta data API
#' @return tbl_df
#'
#' @references \url{https://documentation.bamboohr.com/reference#get-employee-table-row-1}
#'
#' @export
#'

get_table <- function(table, id, since, ...) {
  dots <- rlang::list2(...)
  query <- NULL
  if (rlang::is_missing(since)) {
    id <- rlang::maybe_missing(id, default = "all")
  } else {
    id <- "changed"
    query$since <- as_ISO8601_character(since)
  }
  url <- build_url(
    company_domain = dots$company_domain,
    api_version = dots$api_version,
    base_url = dots$base_url
  )
  url <- glue::glue("{url}/employees/{id}/tables/{table}") |>
    httr::modify_url(query = query)
  response <- get_request(url)
}
