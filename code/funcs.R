# url <- "https://data.cdc.gov/resource/pwn4-m3yp.json"

get_cdc_data <- function(url, limit = 100000000000) {
  
  require(httr2)
  
  ret <- request(url) %>%
    req_url_query("$limit" = limit) %>%
    req_perform() %>%
    resp_body_json(simplifyVector = TRUE)
  
  return(ret)
  
}