with_mock_api({
  test_that("We can get the directory", {

    # Get employee directory
    table <- get_table(employee_id = 123, table_name = "jobInfo")

    # Check a tibble is returned
    expect_s3_class(table, "tbl_df")
    expect_named(table,
                 c("id", "employee_id", "date", "location", "department",
                   "division", "job_title", "reports_to"))

    expect_equal(nrow(table), 3)
    expect_equal(table$id, c("1", "2", "3"))
    expect_equal(table$department[1], "Team")


  })
})
