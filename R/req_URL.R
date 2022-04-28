# # libraries
# library(glue)
# library(httr2)

req_URL <- function(api_type = "rest", file_type, method){
  URL <- "https://marineregions.org/"
  base_url <- glue::glue("{URL}/{api_type}/{method}.{file_type}")
}

# # test
# url <- req_URL(file_type = "json", method = "getGazetteerRecordsByName")
# usethis::use_test()

# # assertions:
# stopifnot(api_type == "rest" || api_type = "soap")
# stopifnot(is.character(file_type))
# stopifnot(is.character(api_type))
# stopifnot(file_type == "json" || file_type = "xml" || file_type = "ttl" || file_type = "jsonld")
# # assertion for method
# # make assertions and error messages prettier
