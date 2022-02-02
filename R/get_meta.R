#' @title Get Meta Information
#'
#' @description  Submits a get request to retrieve account meta information that
#'  can be used to help discover the types of field/table etc. available in your account.
#'
#' @param query One of: "fields", "tables", "lists", "users", "time_off/types", "time_off/policies".
#' @param api_version (optional) - Version of API to use to make request. Default is "v1".
#'
#' @return A [tibble::tibble()] object.
#'
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

  # Define query argument as default, if not given
  query <- match.arg(query)

  # Error handling
  if (length(query) > 1) stop('query must be one of c("fields", "tables","lists",
                              "users", "time_off/types", "time_off/policies")')

  # Build up URL with query
  url <- build_url(
    api_version = api_version
  )
  url <- glue::glue("{url}/meta/{query}")

  ## Create response object depending on query
  # Essentially we just convert to a nice-looking dataframe

  # Get query and convert from JSON
  response <- get_request(url) %>%
    httr::content(response, as = "text", type = "json", encoding = "UTF-8") %>%
    purrr::when(
      query %in% c("fields", "users", "time_off/types", "time_off/policies") ~ jsonlite::fromJSON(., simplifyDataFrame = TRUE),
      query %in% c("tables", "lists") ~ jsonlite::fromJSON(., simplifyDataFrame = FALSE)
    ) %>%
    # Depending on query type, use data manipulation to tabulate results
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
        dplyr::bind_rows() %>%
        # Fix name in nested "options" column so that we can unnest it
        dplyr::mutate(options = lapply(options, function(ls) {
          ls$value <- ls$name
          ls$name <- NULL
          ls
        })) %>%
        tidyr::unnest_wider("options"),
      ~ dplyr::bind_rows(.)
    ) %>%
    tibble::tibble() %>%
    # Clean column names
    janitor::clean_names()

  return(response)
}
