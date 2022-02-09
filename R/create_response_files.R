#' Create Response Files
#'
#' Helper function to create a directory, response RDS file and
#' response-reading R script. This is useful when your GET requests return an
#' unparsed response object, which you want to be mocked using the file
#' structure required by the `{httptest}` package.
#'
#' The general workflow is to:
#' \itemize{
#'   \item{1. Create a response object - Put a [browser()] after the call to
#' [httr::GET()]. This will give you access to the reponse object, which you
#' should pass to \code{response}.}
#'   \item{2. Run [with_mock_api()] with [test_that()] inside it, and the call to
#' the function you are testing inside that, as shown in the
#' [httptest vignette](https://cran.r-project.org/web/packages/httptest/vignettes/httptest.html).
#' This will error, and give an API URL/filepath that we will use to store our
#' test fixtures. This is what we give to \code{filepath}.}
#'   \item{3. Call [create_response_files] giving the above two arguments - you
#' should now have the test fixtures needed in new folders that have been
#' generated and should now be able to run tests without the original error.}
#'}
#'
#' @param response A response object - generally returned by [httr::GET()].
#'
#' @param filepath The full filepath, ending in '.json', given", as the error
#' output from [httptest::with_mock_api()]
#'
#' @examples \dontrun{
#' create_response_files(
#'   response,
#'   "api.bamboohr.com/api/gateway.php/ascent/v1/employees/directory.json"
#' )
#' }
#'
#' @md
#' @keywords internal
create_response_files <- function(response, filepath) {

  # Ensure filepath is pointing to the test location - keep an original
  # so that we can use testthat::test_path in the .R file.
  orig_filepath_no_ext <- fs::path_ext_remove(filepath)
  filepath <- testthat::test_path(filepath)

  ## Error handling
  # Ensure filepath is taken straight from with_mock_api error
  if (!grepl("\\.json$", filepath)) {
    stop(paste0("Please give the full filepath, ending in '.json', given",
                " as the error output from httptest::with_mock_api"))
  }

  # Get and create directory of response file
  if (!dir.exists(dirname(filepath))) {
    file_dir <- dirname(filepath)
    dir.create(file_dir, recursive = TRUE)
  }

  # Create response RDS
  response_filepath <- fs::path_ext_remove(filepath)
  saveRDS(response, glue::glue("{response_filepath}_response.rds"))

  # Create R file that reads in the response
  writeLines(c("readRDS(",
               "  testthat::test_path(",
               glue::glue("    \"{orig_filepath_no_ext}_response.rds\""),
               "  )",
               ")"),
             glue::glue("{response_filepath}.R"))

  invisible(NULL)

}
