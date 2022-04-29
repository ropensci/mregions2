#' Create a base URL for a http request. Queries can be added to the URL.
#'
#' @param api_type Type of API architecture. Must be either "rest" or "soap".
#' @param file_format File format.
#' @param method
#'
#' @return A base URL to append queries to.
#' @export
#'
#' @examples
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

# write function mr_gaz_methods() where you can see the available methods
methods <- c("getGazetteerRecordByMRGID", "getGazetteerGeometry" , "getGazetteerTypes", "getGazetteerGeometries", "getGazetteerRecordsByName", "getGazetteerRecordsByType", "getGazetteerWMSes", "getGazetteerRecordsByLatLong", "getGazetteerRecordsByNames", "getGazetteerSources", "getGazetteerNamesByMRGID", "getGazetteerRecordsBySource", "getFeed", "getGazetteerRelationsByMRGID")

