with_mock_api({
  test_that("We can get the directory", {

    # Get employee directory
    directory <- get_employee()

    # Check a tibble is returned
    expect_s3_class(directory, "tbl_df")
    expect_named(directory,
                 c("id", "displayName", "firstName", "lastName", "preferredName",
                   "jobTitle", "workEmail", "department", "location", "division",
                   "pronouns", "photoUploaded", "photoUrl", "canUploadPhoto"))


  })
})
