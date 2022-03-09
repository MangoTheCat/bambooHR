with_mock_api({
  test_that("We can get the directory", {

    local_config()
    # Get employee directory
    directory <- get_employee()

    # Check a tibble is returned
    expect_s3_class(directory, "tbl_df")
    expect_named(directory,
                 c("id", "displayName", "firstName", "lastName", "preferredName",
                   "jobTitle", "workEmail", "department", "location", "division",
                   "pronouns", "photoUploaded", "photoUrl", "canUploadPhoto"))

    expect_equal(nrow(directory), 1)
    expect_equal(directory$firstName, "John")
    expect_equal(directory$lastName, "Smith")


  })
})
