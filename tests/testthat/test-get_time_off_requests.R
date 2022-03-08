with_mock_api({
  test_that("get timeoff requests works", {

    local_config()
    requests <- get_timeoff_requests("2021-10-01", "2022-02-01", employee_id = "998")

    # Tests
    expect_s3_class(requests, "tbl_df")

    expect_equal(nrow(requests), 2)

    expect_equal(requests$name[1], "A. Person")
  })
})
