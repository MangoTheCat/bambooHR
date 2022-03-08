with_mock_api({
  test_that("get who's out works", {
    local_config()
    wo <- get_whos_out("2020-01-01", "2021-01-01")

    # Tests
    expect_s3_class(wo, "tbl_df")

    expect_equal(nrow(wo), 3)

    expect_equal(wo$name[1], "John Smith")
  })
})
