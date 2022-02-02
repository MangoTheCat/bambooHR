#' Get Company Report
#'
#' Request one of your pre-existing company reports from the reporting section.
#' At present, only reports from the Company Reports section are supported.
#'
#' @param id The report ID. You can get the report number by hovering over the
#' report name on the reports page and grabbing the ID.
#'
#' @param format The output format for the report. Supported formats: CSV, PDF,
#'  XLS, XML, JSON.
#' @param field_filtering Logical, default [TRUE]. Whether to apply duplicate
#' field filtering.
#'
#' @param only_current Logical default [TRUE]. Should the report be limited
#' to only current employees.
#'
#' @inheritParams get_employee
#'
#' @return Currently the function simply returns a parsed response object.
#'
get_report <- function(
  id,
  format = c("CSV", "PDF", "XLS", "XML", "JSON"),
  field_filtering = TRUE,
  only_current = TRUE,
  api_version = "v1"
) {

  # Define params
  fd <- ifelse(field_filtering, "yes", "no")
  format <- match.arg(format)

  # Define query
  query <- list(format = format,
                fd = fd,
                onlyCurrent = URLencode(stringr::str_to_lower(only_current)))

  # Build-up URL
  url <- build_url(api_version = api_version)
  url <- glue::glue("{url}/reports/{id}")
  url <- httr::modify_url(url = url, query = query)

  # Get and parse response
  response <- get_request(url) %>%
    httr::content(type = "text")


  return(response)
}
