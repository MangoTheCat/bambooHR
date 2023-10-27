#' @title Build URL Endpoints with Company Domain and API Version
#'
#' @description  Used to generate the base API request URL. URLs will start with "https://api.bamboohr.com/api/gateway.php/company_domain" which forms the base URL where a query string can then be appended to make the full request URL.
#'
#' @param company_domain  The subdomain used to access BambooHR. If you access BambooHR at https://mycompany.bamboohr.com, then the company_domain is "mycompany".
#' Default value is obtained from \code{options("bambooHR.company_name")}. This can also be set in the config file using \code{config_setup("api key", "company_domain")}.
#' @param api_version Version of API to use to make request. Default is "v1".
#' @param base_url URL to BambooHR API. Default is "https://api.bamboohr.com/api/gateway.php".
#'
#' @return character vector of URL with API version and company domain to give the base URL for API requests.
#'
#' @export
#'
build_url <- function(company_domain = .get_company_name(),
                      api_version = "v1",
                      base_url = "https://api.bamboohr.com/api/gateway.php") {
  if (is.null(company_domain)) {
    stop("Please enter company_domain or set a default value using `config_setup()`.")
  }

  stopifnot(
    is.character(company_domain),
    is.character(api_version),
    is.character(base_url)
  )

  url <- glue::glue("{base_url}/{company_domain}/{api_version}")
  return(url)
}

