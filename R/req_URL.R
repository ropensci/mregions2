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
  # Check customising error messages in checkmate package.
  # method_coll <- makeAssertCollection() # not working yet
  # method_coll$push("\U02139 Check `https://marineregions.org/gazetteer.php?p=webservices&type=rest` for available methods.")

  checkmate::assert_choice(api_type, c("rest", "soap"))
  checkmate::assert_choice(file_format, c("json", "xml", "ttl", "jsonld"))
  # assert_method <- method %>% checkmate::assert_choice(methods, add = method_coll)
  checkmate::assert_choice(method, methods)

  return(base_url)
}

# write function mr_gaz_methods() where you can see the available methods & search for strings
methods <- c("getGazetteerRecordByMRGID", "getGazetteerGeometry" , "getGazetteerTypes", "getGazetteerGeometries", "getGazetteerRecordsByName", "getGazetteerRecordsByType", "getGazetteerWMSes", "getGazetteerRecordsByLatLong", "getGazetteerRecordsByNames", "getGazetteerSources", "getGazetteerNamesByMRGID", "getGazetteerRecordsBySource", "getFeed", "getGazetteerRelationsByMRGID")

#test
api_type <- "rest"
file_format <- "json"
file_format <- 2
method <- "getGazetteerRecordsByName"
method <- "getGazetteerRecordByNamexx"
req_URL(api_type = api_type, file_format = file_format, method = method)

