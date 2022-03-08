with_mock_api({
  test_that("Can get tables", {

    local_config()
    # Get tables object
    tables <- get_meta("tables")

    # Tests
    expect_s3_class(tables, "tbl_df")

    expect_equal(nrow(tables), 12)

    expect_equal(tables$parent_alias[1], "jobInfo")
  })
})
