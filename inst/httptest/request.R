function (request) {
  require(magrittr, quietly=TRUE)
  request %>%
    gsub_request("https://api.bamboohr.com/api/gateway.php/ascent/v1/", "api/", fixed=TRUE)
}


