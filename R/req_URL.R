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

  stopifnot(is.character(file_format))
  stopifnot("File type must be one of the following: json, xml, ttl, jsonld." = file_format %in% c("json", "xml", "ttl", "jsonld"))

  stopifnot(is.character(api_type))
  stopifnot("API type must be one of the following: rest, soap." = api_type %in% c("rest", "soap"))

  stopifnot(is.character(method))
  stopifnot("Method unknown. Check https://marineregions.org/gazetteer.php?p=webservices&type=rest for available methods." = method %in% methods)

  return(base_url)
}

# write function mr_gaz_methods() where you can see the available methods & search for strings
methods <- c("getGazetteerRecordByMRGID", "getGazetteerGeometry" , "getGazetteerTypes", "getGazetteerGeometries", "getGazetteerRecordsByName", "getGazetteerRecordsByType", "getGazetteerWMSes", "getGazetteerRecordsByLatLong", "getGazetteerRecordsByNames", "getGazetteerSources", "getGazetteerNamesByMRGID", "getGazetteerRecordsBySource", "getFeed", "getGazetteerRelationsByMRGID")
