library(testthat)
library(bambooHR)

test_check("bambooHR")
config_setup(apikey = "test", companyname = "ascent", conffile = ".bambooconf.json")
