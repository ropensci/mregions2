usethis::use_package("glue", type = "Suggests")
usethis::use_package("httr2", type = "Imports")

req_URL <- function(api_type = c("rest", "soap"), file_type = c("json", "xml", "ttl", "jsonld"), method){
  URL <- "https://marineregions.org/"
  base_url <- glue::glue("{URL}/{api_type}/{method}.{file_type}")

  # To Do: make warning messages more pretty, make assertions more compact?
  stopifnot("File type not of class character." = is.character(file_type))
  stopifnot("File type not one of the following: json, xml, ttl, jsonld." = file_type %in% c("json", "xml", "ttl", "jsonld"))

  stopifnot("API type not of class character." = is.character(api_type))
  stopifnot("API type not one of the following: rest, soap.", api_type %in% c("rest", soap))

  stopifnot("Method not of class character." = is.character(method))
  stopifnot("Method unknown. Check https://marineregions.org/gazetteer.php?p=webservices&type=rest for available methods." = method %in% methods)
}

# To Do: link methods with available output file types. E.g. "getGazetteerTypes" = c("json", "xml", ""ttl)
methods <- c("getGazetteerRecordByMRGID", "getGazetteerGeometry" , "getGazetteerTypes", "getGazetteerGeometries", "getGazetteerRecordsByName", "getGazetteerRecordsByType", "getGazetteerWMSes", "getGazetteerRecordsByLatLong", "getGazetteerRecordsByNames", "getGazetteerSources", "getGazetteerNamesByMRGID", "getGazetteerRecordsBySource", "getFeed", "getGazetteerRelationsByMRGID")
# where should the acceptable methods be defined?
