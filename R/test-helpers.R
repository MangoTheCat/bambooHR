local_config <- function(env = parent.frame()) {
  config_file <- .get_config_file()

  withr::defer(use_config(config_file, verbose = FALSE), envir = env)

  config_setup(apikey = "test", companyname = "ascent",
               conffile = "bambooconf.json", verbose = FALSE)

  withr::defer(fs::file_delete('bambooconf.json'))
}
