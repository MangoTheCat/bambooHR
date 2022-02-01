#' @title Retrieve A Company File
#'
#' @description `get_company_file` takes a file_id (string), company domain (string),
#' api version (string), and base_url (string) as arguments and then requests
#' and returns data about the corresponding file from the BambooHR API.
#'
#' @param file_id The ID of the file to get from BambooHR.
#' @param company_domain The company's domain name
#' @param api_version version of the BambooHR API
#' @param base_url base url to the API gateway
#'

#' @return returns a response object.
#'
#' @examples
#'
#' response <- get_company_file(
#' "480",
#' company_domain = "ascent",
#' api_version = "v1",
#' base_url = "https://api.bamboohr.com/api/gateway.php"
#' )
#'
#'@author Mark Druffel, \email{mdruffel@propellerpdx.com}

get_company_file <- function(file_id,
                             company_domain = .get_company_name(),
                             api_version = "v1",
                             base_url = "https://api.bamboohr.com/api/gateway.php"){

    # Default to directory if an individual employee is not specified
    file_id <- rlang::maybe_missing(file_id, default = "view")
    # Call the `build_url` function parsing in each element of the dots list
    url <- build_url(
      company_domain = company_domain,
      api_version = api_version,
      base_url = base_url
    )
    # Glues "/files/file_id" to url returned from build_url call
    url <- glue::glue("{url}/files/{file_id}")
    response <- get_request(url)
    return(response)
}
