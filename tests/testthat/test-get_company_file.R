with_mock_api({
  test_that("We can see what files there are", {

    # Get employee directory
    cf <- get_company_file()

    cf <- httr::content(cf)

    cf <- cf %>%
        purrr::pluck("categories", 1) %>%
        tibble::as_tibble() %>%
        tidyr::unnest_wider(.data$files, names_sep = "_") %>%
        dplyr::rename(category_id = .data$id,
                      category_name = .data$name)

    #Check a tibble is returned
    expect_s3_class(cf, "tbl_df")
    expect_named(cf,
                 c("category_id", "canUploadFiles", "category_name", "files_id", "files_name",
                   "files_originalFileName", "files_size", "files_dateCreated", "files_createdBy", "files_shareWithEmployees",
                   "files_canRenameFile", "files_canDeleteFile"))

    expect_equal(nrow(cf), 2)
    expect_equal(cf$files_createdBy[1], "John Smith")

  })
})
