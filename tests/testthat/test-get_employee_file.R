with_mock_api({
  test_that("We can see what files there are", {

    # Get employee directory
    ef <- get_employee_file(id = 123, file_id = "view", suppress_view = TRUE)

    ef <- httr::content(ef)

    categories <- ef %>%
      purrr::pluck("categories", 1) %>%
      tibble::as_tibble() %>%
      tidyr::unnest_wider(.data$files, names_sep = "_")

    employee <- ef %>%
      purrr::pluck("employee", 1) %>%
      tibble::as_tibble()

    # Combine tibbles
    ef <- dplyr::left_join(employee, categories, by = character())

    # Rename column names that are ambiguous to user and view table
    ef <- ef %>%
      dplyr::rename(employee_id = .data$value,
                    category_id = .data$id,
                    category_name = .data$name)

    #Check a tibble is returned
    expect_s3_class(ef, "tbl_df")
    expect_named(ef,
                 c("employee_id", "category_id", "category_name", "canRenameCategory",
                   "canDeleteCategory", "canUploadFiles", "displayIfEmpty", "files_id", "files_name",
                   "files_originalFileName", "files_size", "files_dateCreated",
                   "files_createdBy", "files_shareWithEmployee",
                   "files_canRenameFile", "files_canDeleteFile", "files_canChangeShareWithEmployeeFieldValue"))

    expect_equal(nrow(ef), 1)
    expect_equal(ef$files_createdBy[1], "A. Person")

  })
})
