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

  # Create response object depending on query
  response <- get_request(url) %>%
    httr::content(response, as = "text", type = "json", encoding = "UTF-8") %>%
    purrr::when(
      query %in% c("fields", "users", "time_off/types", "time_off/policies") ~ jsonlite::fromJSON(. , simplifyDataFrame = TRUE),
      query %in% c("tables", "lists") ~ jsonlite::fromJSON(. , simplifyDataFrame = FALSE)
    ) %>%
    purrr::when(
      query == "tables" ~ {function(x) {purrr::map_chr(., "alias") %>%
          tibble::tibble(.) %>%
          purrr::set_names("parent_alias") %>%
          dplyr::mutate(fields = purrr::map(x, "fields")) %>%
          tidyr::unnest_longer(fields) %>%
          tidyr::unnest_wider(col = "fields")}}(.),
      query == "lists" ~ purrr::map(., function(s) {
        s$fieldId <- as.character(s$fieldId)
        s
      }) %>%
        dplyr::bind_rows()
        ,
      ~ dplyr::bind_rows(.)
    ) %>%
    tibble::tibble()

  return(response)


}
