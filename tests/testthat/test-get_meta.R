with_mock_api({
  test_that("Can get tables", {

    # Get tables object
    tables <- get_meta("tables")

    # Tests
    expect_s3_class(tables, "tbl_df")


  })
})
