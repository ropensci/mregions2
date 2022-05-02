#' Create a base URL for a http request.
#'
#' @param api_type Type of API architecture. Must be one of c("rest", "soap").
#' @param file_format File format. Must be one of c("json", "xml", "ttl", "jsonld").
#' @param method RESTful method. Check the Marineregions gazetteer at https://marineregions.org/gazetteer.php?p=webservices for available methods.
#'
#' @return A base URL to append queries to.
#' @export
#'
#' @examples
#' api <- "rest"
#' file <- "json"
#' method <- "getGazetteerRecordsByName"
#' req_URL(api_type = api, file_format = file, method = method)
#' # https://marineregions.org//rest/getGazetteerRecordsByName.json/

req_URL <- function(api_type, file_format, method){
  URL <- "https://marineregions.org/"
  base_url <- glue::glue("{URL}/{api_type}/{method}.{file_format}/")

  # Assertions
  ## To Do: modify messages when test passed (default: "TRUE").
  ## Check customising error messages in checkmate package.

  assert_api_type <- api_type %>% checkmate::check_choice(c("rest", "soap"))
  assert_file_format <- file_format %>% checkmate::check_choice(c("json", "xml", "ttl", "jsonld"))
  assert_method <- method %>% checkmate::check_choice(methods)

  if(!isTRUE(checkmate::check_choice(method, methods))){
    paste("\U02139 Check https://marineregions.org/gazetteer.php?p=webservices&type=rest for available methods.")
  }

  return(c(assert_api_type, assert_file_format, assert_method, base_url))
}

# write function mr_gaz_methods() where you can see the available methods & search for strings
methods <- c("getGazetteerRecordByMRGID", "getGazetteerGeometry" , "getGazetteerTypes", "getGazetteerGeometries", "getGazetteerRecordsByName", "getGazetteerRecordsByType", "getGazetteerWMSes", "getGazetteerRecordsByLatLong", "getGazetteerRecordsByNames", "getGazetteerSources", "getGazetteerNamesByMRGID", "getGazetteerRecordsBySource", "getFeed", "getGazetteerRelationsByMRGID")

#test
api_type <- "rest"
file_format <- "json"
file_format <- 2
method <- "getGazetteerRecordsByName"
method <- "getGazetteerRecordByName"
req_URL(api_type = api_type, file_format = file_format, method = method)

