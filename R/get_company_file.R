#' @title Retrieve A Company File.
#'
#' @description `get_company_file` takes a file_id (string) and
#' api version (string) as arguments and then requests
#' and returns data about the corresponding file from the BambooHR API,
#' the file is then downloaded if possible.
#'
#' @param file_id The ID of the file to get from BambooHR.
#' @param api_version version of the BambooHR API.
#'
#' @return returns a response object.
#'
#' @examples \dontrun{
#'response <- get_company_file(
#' "480",
#' api_version = "v1",
#' )
#'}
#'
#'@author Harry Alexander, \email{harry.alexander@ascent.io}

get_company_file <- function(file_id = "view",
                             api_version = "v1") {
  url <- build_url(api_version = api_version)
  # Glues "/files/file_id" to url returned from build_url call
  url <- glue::glue("{url}/files/{file_id}")
  response <- get_request(url)
  # Extract file name from 'content-disposition' variable
  file_name <- substring(
    text = response$headers$`content-disposition`,
    first = 23,
    last = nchar(response$headers$`content-disposition`) - 1
  )
  # Attempt to parse content of file as raw binary and write it in wd
  if (file_id != "view") {
    tryCatch({
      response %>%
        httr::content(as = "raw") %>%
        writeBin(file_name)
      message("Successfully downloaded file to working directory.")
    }, error = function(e)
      warning("Failed to download file."),
    finally = {
      return(response)
    })
  }
  # else file_id is "view" return a dataframe containing all files and categories
  else{
    content <- httr::content(response)
    if(!length(content$categories) == 0){
      categories <- content %>%
        purrr::pluck("categories", 1) %>%
        tibble::as_tibble() %>%
        tidyr::unnest_wider(files, names_sep = "_") %>%
        dplyr::rename(category_id = id,
                      category_name = name)
      utils::View(categories)
    }
    else{
      message("The response from the API returned no files.")
    }
    return(response)
  }
}
