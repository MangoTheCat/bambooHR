with_mock_api({
  test_that("get time off types works", {
    types <- get_timeoff_types()

    # Tests
    expect_s3_class(types, "tbl_df")

    expect_equal(nrow(types), 17)

    expect_equal(types$name[1], "Holiday")
  })
})
