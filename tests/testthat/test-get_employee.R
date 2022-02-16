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


library(encryptr)
library(dplyr)
genkeys()

set.seed(12932)
directory %>%
  select(-photoUrl) %>%
  encrypt(everything(), )


directory_response

key <- httr2::secret_make_key()
httr2::secret_write_rds(directory_response, "directory_response_secret", key = key)
httr2::secret_read_rds("directory_response_secret", key)
