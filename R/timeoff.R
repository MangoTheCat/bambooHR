#' Get Time Off Requests
#'
#' @param start Character in format "YYYY-MM-DD". Only show time off that occurs on/after the specified start date.
#' @param end Character in format "YYYY-MM-DD". Only show time off that occurs on/before the specified end date.
#' @param id (optional) Integer - A particular request ID to limit the response to.
#' @param action (optional) - Limit to requests that the user has a particular level of access to. Legal values are: "view" or "approve".
#' @param employee_id  (optional) Character - A particular employee ID to limit the response to.
#' @param type (optional) - A vector of time off types IDs to include limit the response to. Default is all types are included.
#' @param status (optional) - A vector of request status values to include. Legal values are "approved", "denied", "superseded", "requested", "canceled". Default is all status types.
#' @param api_version (optional) - Version of API to use to make request. Default is "v1".
#'
#' @references \url{https://documentation.bamboohr.com/reference/time-off-1}
#' @return A [tibble::tibble()] object.
#' @md
#' @export
#'
#' @examples
#' \dontrun{
#' # find valid valid types
#' types <- get_timeoff_types()
#' type_ids <- types %>% tidyr::drop_na(id) %>%  dplyr::pull(id)
#'
#' res <- get_timeoff_requests("2022-01-01", "2022-02-01", type = type_ids)
#'
#' res2 <- get_timeoff_requests("2022-01-01", "2022-02-01", action = "approve", status = c("approved", "denied"))
#'
#' res3 <- get_timeoff_requests("2022-01-01", "2022-02-01", employee_id = "4")
#'
#' }
get_timeoff_requests <- function(start, end, id = NULL, action = c("view", "approve"), employee_id = NULL, type = NULL,
                                 status = c("approved", "denied", "superseded", "requested", "canceled"),
                                 api_version = "v1") {

  # Type checks and Error handling ---
  valid_start <- is_ymd(start)
  valid_end <- is_ymd(end)

  if (!all(valid_start, valid_end)) stop("Invalid date. Date formats must be YYYY-MM-DD")

  if (!is.null(id)) {
    id <- as.integer(id)
    stopifnot(!is.na(id))
  }

  if (!is.null(employee_id)) {
    stopifnot(is.character(employee_id))
  }

  if (!is.null(type)) {
    type_coerced <- as.integer(type)
    stopifnot(!is.na(type_coerced))
  }

  stopifnot(all(status %in% c("approved", "denied", "superseded", "requested", "canceled")))

  # construct api query from provided arguments
  query <- list(
    start = start,
    end = end,
    id = id,
    action = action,
    employee_id = employee_id,
    type = type,
    status = status
  ) %>%  purrr::discard(is.null)

  # convert parameters with length > 1 to comma separated strings if present in query
  query <- query %>%
    purrr::map(~ paste(.x, collapse = ","))


  # build url together with arguments and make request
  url <- build_url(api_version = api_version)
  url <- glue::glue("{url}/time_off/requests")  %>%
    httr::modify_url(query = query)
  response <- get_request(url)

  # convert json response into a tibble
  return(
    response %>%
      httr::content(as='text', type='json', encoding='UTF-8') %>%
      jsonlite::fromJSON(simplifyDataFrame=TRUE) %>%
      tibble::tibble() %>%
      tidyr::unnest(tidyr::everything(), names_sep = "_") %>%
      janitor::clean_names()
  )
}

#' Get a list of Who's Out
#'
#' @param start (optional) - a date in the form YYYY-MM-DD - defaults to the current date.
#' @param end  (optional) - a date in the form YYYY-MM-DD - defaults to 14 days from the start date.
#' @param api_version (optional) - Version of API to use to make request. Default is "v1".
#'
#' @return A [tibble::tibble()] object.
#' @export
#'
#' @examples
#' \dontrun{
#' res <- get_whos_out()
#'
#' res2 <- get_whos_out(start = "2022-01-12")
#'
#' res3 <- get_whos_out(start = "2022-01-01", end = "2022-04-01")
#' }
#' @references \url{https://documentation.bamboohr.com/reference/get-a-list-of-whos-out-1}
#' @md
get_whos_out <- function(start = "", end = "", api_version = "v1") {

  # check inputs are valid
  if (start == "") {
    valid_start <- TRUE
  } else {
    valid_start <- is_ymd(start)
  }

  if (end == "") {
    valid_end <- TRUE
  } else {
    valid_end <- is_ymd(end)
  }

  if (!all(valid_start, valid_end)) stop("Invalid date. Date formats must be YYYY-MM-DD")

  query <- list(start = start, end = end)

  # send request to api with user query
  url <- build_url(api_version = api_version)
  url <- glue::glue("{url}/time_off/whos_out") %>%
    httr::modify_url(query = query)
  response <- get_request(url)

  # convert response to tibble
  return(
    response %>%
      httr::content(as='text', type='json', encoding='UTF-8') %>%
      jsonlite::fromJSON(simplifyDataFrame=TRUE) %>%
      tibble::tibble() %>%
      janitor::clean_names()
  )
}

#' Get Time Off Types
#'
#' Wrapper to get_meta function that returns time off types
#'
#' @return A vector of type ids
#' @export
#'
#' @examples
#' \dontrun{
#' types <- get_timeoff_types()
#' type_ids <- types %>% tidyr::drop_na(id) %>%  dplyr::pull(id)
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
#' @note Currently do not have the permissions to test this so returning the raw response object
#'
get_timeoff_policies <- function() {
  response <- get_meta("time_off/policies")

  response
}
