

with_mock_api({
  test_that("We can get the directory", {

  directory <- get_employee()
  #expect_s3_class(directory, "tbl_df")

  })
})
