local_config <- function(env = parent.frame()) {

  reslist <- list(api_key = "apikey", company_name = "ascent")
  json <- jsonlite::toJSON(reslist, auto_unbox=TRUE, pretty=TRUE)

  f <- file("~/bambooHR_user_config.json",open = "w")
  cat(json,file = f)
  close(f)

  withr::local_options("bambooHR.config_file" = "~/bambooHR_user_config.json")
  withr::defer_parent(unlink("~/bambooHR_user_config.json"))

}
