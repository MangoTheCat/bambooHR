#' @title Bamboo API get request wrapper for BambooHR Account Information
#'
#' @description  Submits a get request to retrieve account meta information that
#'  can be used to help discover the types of field/table ect. available in your account
#' devto
#' @param query One of: "fields", "tables", "lists", "users", "time_off/types", "time_off/policies".
#' @param api_version (optional) - Version of API to use to make request. Default is "v1".
#'
#' @return A [tibble::tibble()] object.
#' @md
#' @export
#' @examples
#' \dontrun{
#' res <- get_meta("fields")
#'
#' res2 <- get_meta("users")
#' }
#'
#' @references \url{https://documentation.bamboohr.com/reference/account-information-1}

get_meta <- function(query = c(
                       "fields", "tables", "lists", "users",
                       "time_off/types", "time_off/policies"
                     ),
                     api_version = "v1") {
  stopifnot(query %in% c(
    "fields", "tables", "lists", "users",
    "time_off/types", "time_off/policies"
  ))

  if (length(query) > 1) stop('query must be one of c("fields", "tables","lists",
                              "users", "time_off/types", "time_off/policies")')


  url <- build_url(
    api_version = api_version
  )
  url <- glue::glue("{url}/meta/{query}")

  # Create response object depending on query
  response <- get_request(url) %>%
    httr::content(response, as = "text", type = "json", encoding = "UTF-8") %>%
    purrr::when(
      query %in% c("fields", "users", "time_off/types", "time_off/policies") ~ jsonlite::fromJSON(., simplifyDataFrame = TRUE),
      query %in% c("tables", "lists") ~ jsonlite::fromJSON(., simplifyDataFrame = FALSE)
    ) %>%
    purrr::when(
      query == "tables" ~ {
        function(x) {
          purrr::map_chr(., "alias") %>%
            tibble::tibble(.) %>%
            purrr::set_names("parent_alias") %>%
            dplyr::mutate(fields = purrr::map(x, "fields")) %>%
            tidyr::unnest_longer(fields) %>%
            tidyr::unnest_wider(col = "fields")
        }
      }(.),
      query == "lists" ~ purrr::map(., function(s) {
        s$fieldId <- as.character(s$fieldId)
        s
      }) %>%
        dplyr::bind_rows(),
      ~ dplyr::bind_rows(.)
    ) %>%
    tibble::tibble()

  return(response)
}
