#' @title Bamboo HR API get request wrapper
#'
#' @description  Submits get requests to the Bamboo API.
#'
#' @param url Character url of the get request.
#' @param key  Character api key for bambooHR. Register in your account, see \href{https://documentation.bamboohr.com/docs#section-authentication}{authentication} for details.
#' Default value is obtained from \code{options("bambooHR.api_key")}. This can also be set in the config file using \code{config_setup("api key", "company domain")}.
#' @param verbose Logical. Should the function provide verbose messaging back on each step?
#'
#' @return A [httr::response()] object.
#'
#' @examples
#' \dontrun{
#' res <- get_request("https://api.bamboohr.com/api/gateway.php/your_company/v1/employees/0/")
#' httr::content(res, "text")
#' }
#'
#' @references
#' \url{https://github.com/r-lib/httr}
#' \url{https://curl.haxx.se/libcurl/c/CURLOPT_HTTP_VERSION.html}
#'
#' @md

get_request <- function(url,
                        key = .get_api_key(),
                        verbose = FALSE) {
  if (is.null(key)) {
    stop("Please enter your api key or set a default value using `config_setup()`.")
  }

  stopifnot(
    is.character(url),
    is.character(key),
    is.logical(verbose)
  )


  # Confirm internet works --------------------------------------------------
  check_internet()

  # Password is any random string -------------------------------------------
  # The password for bamboo api should be any random string and user should be the API key, see https://documentation.bamboohr.com/docs#section-authentication
  response <- httr::GET(
    url, httr::authenticate(user = key, password = "notapplicable"),
    httr::add_headers(Accept = "application/json"),
    if (verbose) {
      httr::verbose(data_out = TRUE, info = TRUE)
    } else {
      NULL
    }
  )
  check_status(response)
  return(response)
}

check_internet <- function() {
  if (!curl::has_internet()) {
    stop("Please check your internet connection.")
  }
}

check_status <- function(response) {
  if (httr::http_error(response)) {
    stop(httr::message_for_status(response))
  }
}
