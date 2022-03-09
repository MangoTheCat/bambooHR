local_config <- function(env = parent.frame()) {

  op <- options("bambooHR.api_key" = "apikey", "bambooHR.company_name" = "ascent")
  defer(options(op), env = env)

}
