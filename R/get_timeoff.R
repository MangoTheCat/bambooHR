#' Get Time Off Requests
#'
#' @param start Character in format "YYYY-MM-DD". Only show time off that occurs on/after the specified start date.
#' @param end Character in format "YYYY-MM-DD". Only show time off that occurs on/before the specified end date.
#' @param id (optional) Integer - A particular request ID to limit the response to.
#' @param action (optional) - Limit to requests that the user has a particular level of access to. Legal values are: "view" or "approve".
#' @param employee_id  (optional) Character - A particular employee ID to limit the response to.
#' @param type (optional) - A vector of time off types IDs to include limit the response to. Default is all types are included.
#' @param status (optional) - A vector of request status values to include. Legal values are "approved", "denied", "superseded", "requested", "canceled". Default is all status types.
#' @param url_args (optional) - A list of named or unnamed arguments to be passed to build_URL function.
#'
#' @references \url{https://documentation.bamboohr.com/reference/time-off-1}
#' @return A [httr::response()] object.
#' @md
#' @export
#'
#' @examples
#' \dontrun{
#' # find valid types
#'
#' type_list <- get_timeoff_types()
#' out <- httr::content(type_list, "parsed")
#' types <- sapply(out$timeOffTypes, "[[", "id")
#'
#' res <- get_timeoff("2022-01-01", "2022-02-01", type = types)
#' httr::content(res, "parsed")
#'
#'
#' res <- get_timeoff("2022-01-01", "2022-02-01", action = "approve", status = c("approved", "denied"))
#' httr::content(res, "parsed")
#'
#' res <- get_timeoff("2022-01-01", "2022-02-01", url_args = list(company_domain = "company"))
#' httr::content(res, "parsed")
#' }
get_timeoff <- function(start, end, id = NULL, action = "view", employee_id = NULL, type = NULL,
                        status = c("approved", "denied", "superseded", "requested", "canceled"),
                        url_args = NULL) {

  invalid_start <- is.na(lubridate::parse_date_time(start, orders = "ymd", quiet = TRUE))
  invalid_end <- is.na(lubridate::parse_date_time(end, orders = "ymd", quiet = TRUE))

  if (invalid_start | invalid_end) stop("Invalid date. Date formats must be YYYY-MM-DD")


  if (!is.null(id)) {
    id <- as.integer(id)
    stopifnot(!is.na(id))
  }

  action_valid <- (action == "view" | action == "approve")
  if (!all(action_valid)) stop("action must be one of: \"view\" or \"approve\".")

  if (!is.null(employee_id)) {
    stopifnot(is.character(employee_id))
  }

  if (!is.null(type)) {
    type_coerced <- as.integer(type)
    stopifnot(!is.na(type_coerced))
  }

  stopifnot(all(status %in% c("approved", "denied", "superseded", "requested", "canceled")))

  query <- as.list(match.call()[-1])

  # convert type and status to comma separated strings
  if (!is.null(query$status)) {
    query$status <- paste(status, collapse = ",")
  }
  if (!is.null(query$type)) {
    query$type <- paste(type_coerced, collapse = ",")
  }

  if (!is.null(url_args)){
    stopifnot(is.list(url_args))
    query$url_args <- NULL
  }

  url <- do.call(build_url, url_args)
  url <- glue::glue("{url}/time_off/requests") |>
    httr::modify_url(query = query)
  response <- get_request(url)
}

#' Get a list of Who's Out
#'
#' @param start (optional) - a date in the form YYYY-MM-DD - defaults to the current date.
#' @param end  (optional) - a date in the form YYYY-MM-DD - defaults to 14 days from the start date.
#' @param url_args (optional) - A list of named or unnamed arguments to be passed to build_URL function.
#'
#' @return A [httr::response()] object where the content is a JSON format that contains a list,
#'  sorted by date, of employees who will be out, and company holidays, for a period of time.
#' @export
#'
#' @examples
#' \dontrun{
#' res <- get_whos_out()
#' httr::content(res, "parsed")
#'
#' res <- get_whos_out(start = "2022-01-12")
#' httr::content(res, "parsed")
#'
#' res <- get_whos_out(start = "2022-01-01", end = "2022-04-01")
#' httr::content(res, "parsed")
#' }
#' @references \url{https://documentation.bamboohr.com/reference/get-a-list-of-whos-out-1}
#' @md

get_whos_out <- function(start = "", end = "", url_args = NULL) {
  invalid_start <- invalid_end <- FALSE
  if (!(start == "")) {
    invalid_start <- is.na(lubridate::parse_date_time(start, orders = "ymd", quiet = TRUE))
  }
  if (!(end == "")) {
    invalid_end <- is.na(lubridate::parse_date_time(end, orders = "ymd", quiet = TRUE))
  }

  if (invalid_start | invalid_end) stop("Invalid date. Date formats must be YYYY-MM-DD")

  query <- list(start = start, end = end)

  if (!is.null(url_args)){
    stopifnot(is.list(url_args))
  }

  url <- do.call(build_url, url_args)
  url <- glue::glue("{url}/time_off/whos_out") |>
    httr::modify_url(query = query)
  response <- get_request(url)
}

#' Get Time Off Types
#'
#' Wrapper to get_meta function that returns time off types
#'
#' @return A [httr::response()] object where the content is a JSON format that contains the
#' list of time off types the user has permissions on.
#' @export
#'
#' @examples
#' \dontrun{
#' res <- get_timeoff_types()
#' httr::content(res, "parsed")
#' }
#' @references \url{https://documentation.bamboohr.com/reference/get-time-off-types}
#' @md
get_timeoff_types <- function() {
  response <- get_meta("time_off/types")
}


#' Get Time Off policies
#'
#' Wrapper to get_meta function that returns a list of time off policies.
#'
#' @return A [httr::response()] object where the content is a JSON format that contains the
#' list of time off policies.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' res <- get_timeoff_policies()
#' httr::content(res, "parsed")
#' }
#' @references \url{https://documentation.bamboohr.com/reference/get-time-off-policies}
#' @md
#'
get_timeoff_policies <- function() {
  response <- get_meta("time_off/policies")
}
