#' Use Config File
#'
#' Choose a specific configuration file to supply the API key and the company
#' domain.
#'
#' In rare circumstances, it may be useful to swap between multiple
#' configuration files, for example when comparing results on different accounts
#' possibly with different permissions. In this case a user may set-up two
#' config files with [config_setup] in different locations (see
#' [conffile] argument), and swap between them with [use_config]. The chosen
#' config file will then be used for subsequent API calls, as [use_config] sets
#' global options and variables in the package environment. Of course,
#' [config_setup] does all of this too, however [use_config] does not expose
#' the API key when called.
#'
#' @param config Filepath to the config file.
#'
#' @return [NULL] invisibly.
#'
#' @export
#'
#' @examples \dontrun{
#' # Setup 2 configuration files
#' config_setup(apikey = "key", companyname = "Ascent", conffile = "example.json")
#' config_setup(apikey = "key2", companyname = "Descent", conffile = "example2.json")
#'
#' # Jump between them
#' use_config("example.json")
#' use_config("example2.json")
#'
#' # Remove them when finished
#' unlink("example.json")
#' unlink("example2.json")
#' }
use_config <- function(config="~/.bambooHR_user_config.json") {
  conf <- jsonlite::fromJSON(config, simplifyVector = FALSE)
  .pkgenv[["conf"]] <- conf
  if (is.null(conf[["api_key"]])) {
    warning("Please check that the 'api_key' value is not missing or empty: ", config, call.=FALSE, immediate.=TRUE)
  }

  options("bambooHR.config_file" = config)
  options("bambooHR.api_key" = conf[["api_key"]])
  options("bambooHR.company_name" = conf[["company_name"]])

  cli::cli_alert_success(glue::glue(
    "Using config file: {normalizePath(config)}"
  ))
  invisible(NULL)
}
