#' @title Bamboo API get request wrapper for BambooHR Account Information
#'
#' @description  Submits a get request to retrieve account meta information that
#'  can be used to help discover the types of field/table ect. available in your account
#'devto
#' @param query One of: "fields", "tables", "lists", "users", "time_off/types", "time_off/policies".
#' @param url_args (optional) - A list of named or unnamed arguments to be passed to build_URL function.
#'
#' @return data.frame
#'
#' @export
#' @examples
#' \dontrun{
#' res <- get_meta("fields")
#'
#' res <- get_meta("users", list(company_domain = "CompanyB"))
#' }
#'
#' @references \url{https://documentation.bamboohr.com/reference/account-information-1}
get_meta <- function(query = c("fields", "tables", "lists", "users", "time_off/types", "time_off/policies"), url_args = NULL) {
  if (!is.null(url_args)) {
    stopifnot(is.list(url_args))
  }

  if (length(query) > 1) stop('query must be one of c("fields", "tables", "lists", "users", "time_off/types", "time_off/policies")')

  if (!is.null(url_args)) {
    stopifnot(is.list(url_args))
  } else {
    url_args <- list()
  }
  url <- do.call(build_url, url_args)
  url <- glue::glue("{url}/meta/{query}")

  browser()
  response <- get_request(url) %>%
    httr::content(response, as = "text", type = "json", encoding = "UTF-8") %>%
    purrr::when(
      query %in% c("fields", "users", "time_off/types", "time_off/policies") ~ jsonlite::fromJSON(. , simplifyDataFrame = TRUE),
      query %in% c("tables", "lists") ~ jsonlite::fromJSON(. , simplifyDataFrame = FALSE)
    ) %>%
    purrr::when(
      query == "tables" ~ {function(x) {purrr::map_chr(., "alias") %>%
        tibble::tibble(.) %>%
        purrr::set_names("parent_alias")%>%
        dplyr::mutate(fields = purrr::map(x, "fields")) %>%
        tidyr::unnest_longer(fields) %>%
        tidyr::unnest_wider(col = "fields")}}(),
      ~ dplyr::bind_rows(.)
    )

  #browser()
  # if (query == "fields") {
  #   return(
  #     response %>%
  #       jsonlite::fromJSON(simplifyDataFrame = TRUE)
  #   )
  # } else if (query == "users") {
  #   return(
  #     response %>%
  #       jsonlite::fromJSON(simplifyDataFrame = TRUE) %>%
  #       data.table::rbindlist(fill = TRUE) %>%
  #       as.data.frame()
  #   )
  # } else if (query == "tables") {
  #   tbl <- response %>%
  #     jsonlite::fromJSON(simplifyDataFrame = FALSE)
  #
  #   return(
  #     purrr::map_chr(tbl, "alias") %>%
  #       tibble::tibble() %>%
  #       purrr::set_names("parent_alias")%>%
  #       dplyr::mutate(fields = purrr::map(tbl, "fields")) %>%
  #       tidyr::unnest_longer(fields) %>%
  #       tidyr::unnest_wider(col = "fields")
  #   )
  # } else if (query == "lists") {
  #   return(
  #     response %>%
  #       jsonlite::fromJSON(simplifyDataFrame = FALSE) %>%
  #       data.table::rbindlist(fill = TRUE) %>%
  #       as.data.frame()
  #   )
  # } else if (query == "time_off/types" | query == "time_off/policies") {
  #   response %>%
  #     jsonlite::fromJSON(simplifyDataFrame = TRUE) %>%
  #     as.data.frame()
  # } else {
  #   (return(response))
  # }
}
