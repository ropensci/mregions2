#' Retrieve information of Marine Gazetteer Records
#'
#' @description Available Placetypes and Sources of [Marine Gazetteer](https://marineregions.org/gazetteer.php) Records.
#' Useful for retrieving information about Gazetteer Records based on Placetypes (`mr_gaz_records_by_type()`) and Sources (`mr_gaz_records_by_source`).
#'
#' @param info The information wished to retrieve. Must be one of: `c("sources", "placetypes")`.
#' @return Sources: A tibble with the source and source URL.
#' Placetypes: A tibble with the ID, name and description of each Placetype available in the [Marine Gazetteer](https://marineregions.org/gazetteer.php).
#' @export
#'
#' @examples
#' placetypes <- mr_gaz_info(info = "placetypes")
#' placetypes$type[28]
#' # [1] "Sandbank"
#'
#' sources <- mr_gaz_info(info = "sources")
#' sources$source[36]
#' # [1] "GeoNames"
mr_gaz_info <- function(info = c("sources", "placetypes")){

  checkmate::assert_character(info)
  checkmate::assert_choice(tolower(info), c("sources", "placetypes"))

  url_sources <- mr_req_URL("rest", "json", "getGazetteerSources")
  url_placetypes <- mr_req_URL("rest", "json", "getGazetteerTypes")

  ifelse(info == "sources", url <- url_sources, url <- url_placetypes)

  req <- httr2::request(url) %>%
    httr2::req_headers(
      accept = "application/json") %>%
    req_mr_user_agent()

  resp <- req %>%
    httr2::req_perform()

  res <- resp %>%
    mr_resp_to_tibble()

  return(res)
}

#' Retrieve Gazetteer Records by Name
#'
#' @param name GazetteerName of the marine region of interest.
#' @param count Number of records to retrieve.
#' @param like Formats the name into a SQL-like syntax.
#' @param fuzzy Uses Levenshtein query to find nearest matches.
#' @param offset Start record number, in order to page through next batch of results.
#'
#' @return A tibble with Gazetter records.
#' @export
#'
#' @examples
#' mr_gaz_records_by_name(name = "High Seas", count = 5)
mr_gaz_records_by_name <- function(name, count = 100, like = TRUE, fuzzy = FALSE, offset = 0){

  checkmate::assert_character(name)
  # todo: throw error when name is not in the records

  url <- mregions2::mr_req_URL("rest", "json", "getGazetteerRecordsByName")

  name <- utils::URLencode(name)

  req <- httr2::request(url) %>%
    httr2::req_headers(
      accept = "application/json")  %>%
    req_mr_user_agent() %>%
    httr2::req_url_path_append(name) %>%
    httr2::req_url_path_append("/")

  resp <- req %>%
    httr2::req_url_query(
      `like` = like,
      `fuzzy` = fuzzy,
      `offset` = offset,
      `count` = count) %>%
    req_perform()


  # tryCatch(req %>% httr2::req_perform(),
  #            if(httr2::last_response()$status == 404){
  #              message("The Marine Gazetteer entry was not found. Check that \n1) the name is spelled correctly and \n2) that it is in the Marine Gazetteer here: https://marineregions.org/gazetteer.php?p=search.")
  #            } else{
  #              resp <- req %>% httr2::req_perform()
  #            }
  # )

  res <- resp %>%
    mr_resp_to_tibble()

  res
}

#' Retrieve Gazetteer Records by Placetype
#'
#' @description
#' Every record in the [Marine Gazetteer](https://marineregions.org/gazetteer.php) has a placetype, e.g. `Sandbank` or `Marine Protected Area`.
#' All placetypes currently available in the gazetteer can be retrieved via `mr_gaz_info("placetypes")`.
#'
#' @param type The placetype of which records are retrieved. Must be one of the types in `mr_gaz_info("placetypes")` and class `character`. Case insensitive.
#' @param offset Start record number, in order to page through next batch of results.
#'
#' @return A tibble with all Gazetteer records of the specified placetype.
#' @export
#'
#' @examples
#' fao_subdivs <- mr_gaz_records_by_type("FAO Subdivisions")
#' fao_subdivs$preferredGazetteerName[1]
#' # [1] "Northern Alboran Sea"
mr_gaz_records_by_type <- function(type, offset = 0){

  # Assertions
  checkmate::assert_character(type)
  tested_type <- tolower(type)
  expected_types <- mr_gaz_info("placetypes") # takes much time to load, change somehow?
  expected_types <- expected_types$type %>%
    as.character() %>%
    tolower()
  checkmate::assert_choice(tested_type, expected_types)

  url <- mregions2::mr_req_URL(api_type = "rest", file_format = "json", method = "getGazetteerRecordsByType")

  type <- utils::URLencode(type)

  req <- httr2::request(url) %>%
    httr2::req_headers(
      accept = "application/json")  %>%
    req_mr_user_agent() %>%
    httr2::req_url_path_append(type) %>%
    httr2::req_url_path_append("/")

  resp <- req %>%
    httr2::req_url_query(`offset` = offset) %>%
    httr2::req_perform()

  res_json <- resp %>%
    httr2::resp_body_json()

  res <- resp %>%
    mr_resp_to_tibble()

  res
}

#' Retrieve Gazetteer Records by Source
#'
#' @description
#' Every record in the [Marine Gazetteer](https://marineregions.org/gazetteer.php) has a placetype, e.g. `Sandbank` or `Marine Protected Area`.
#' All placetypes currently available in the gazetteer can be retrieved via `mr_gaz_info("placetypes")`.
#'
#' @param source The source of which records are retrieved. Must be one of the types in `mr_gaz_info("placetypes")` and class `character`. Case insensitive.
#'
#' @return A tibble with all Gazetteer records of the specified placetype.
#' @export
#'
#' @examples
#' ecoregions <- mr_gaz_records_by_source("ICES Ecoregions")
#' ecoregions$preferredGazetteerName[4]
# [1] "Oceanic Northeast Atlantic"
mr_gaz_records_by_source <- function(source){

  # Assertions
  checkmate::assert_character(source)
  tested_source <- tolower(source)
  expected_sources <- mr_gaz_info("sources") # takes much time to load, change somehow?
  expected_sources <- expected_sources$source %>%
    as.character() %>%
    tolower()
  checkmate::assert_choice(tested_source, expected_sources)

  url <- mregions2::mr_req_URL(api_type = "rest", file_format = "json", method = "getGazetteerRecordsBySource")

  source <- utils::URLencode(source)

  req <- httr2::request(url) %>%
    httr2::req_headers(
      accept = "application/json")  %>%
    req_mr_user_agent() %>%
    httr2::req_url_path_append(source) %>%
    httr2::req_url_path_append("/")

  resp <- req %>%
    httr2::req_perform()

  res <- resp %>%
    mr_resp_to_tibble()

  res
}

#' Retrieve Gazetteer Records by Lat-Lon Coordinates
#'
#' @description Get the first 100 [Marine Gazetteer](https://marineregions.org/gazetteer.php) records, where their centroid is within the bounding box calculated by latitude (+/- radius) and longitude (+/- radius)

#' @param lat A decimal number which ranges from -90 to 90.
#' @param lon A decimal number which ranges from -180 to +180.
#' @param lat_radius Searches for latitudes in the range 'latitude'-'latRadius' and 'latitude'+'latRadius' default = 0.
#' @param lon_radius Searches for longitudes in the range 'longitude'-'longRadius' and 'longitude'+'longRadius' default = 0.
#'
#' @return A tibble with the first 100 Gazetteer records of the specified coordinates.
#' @export
#'
#' @examples
#' oostende_lat <- 51.21551
#' oostende_lon <- 2.927
#' oostende <- mr_gaz_records_by_latlon(oostende_lat, oostende_lon)
#'
#' some_atlantic_lat <- -37
#' some_atlantic_lon <- 37
#' some_atlantic_location <- mr_gaz_records_by_latlon(some_atlantic_lat, some_atlantic_lon)
mr_gaz_records_by_latlon <- function(lat, lon, lat_radius = 0, lon_radius = 0){

  # Assertions
  checkmate::assert_double(lat, lower = -90, upper = 90)
  checkmate::assert_double(lon, lower = -180, upper = 180)

  # check if high numbers of decimals work or not
  # assert for "," instead of "."
  ##  dec_test2 <- mr_gaz_records_by_latlon(34,3, 12,4, 5,7): error for unused arguments
  ## hint for this case:
  # dec_test3 <- mr_gaz_records_by_latlon(lat = 32,3, lon = 34,5)
  # dec_test4 <- mr_gaz_records_by_latlon(lat = 32, lon = 34, 3, 5)
  # setequal(dec_test3, dec_test4) # TRUE

  url <- mregions2::mr_req_URL(api_type = "rest", file_format = "json", method = "getGazetteerRecordsByLatLong")

  req <- httr2::request(url) %>%
    httr2::req_headers(
      accept = "application/json")  %>%
    req_mr_user_agent() %>%
    httr2::req_url_path_append(lat) %>%
    httr2::req_url_path_append(lon) %>%
    httr2::req_url_path_append("/")

  resp <- req %>%
    httr2::req_url_query(
      `lat_radius` = lat_radius,
      `lon_radius` = lon_radius) %>%
    httr2::req_perform()

  # # inform user of CPU time (request takes long compared to the other webservices)
  # req_cpu_time <- system.time(httr2::req_perform(req))
  # message(glue::glue("The CPU time for performing this http request was {round(req_cpu_time[[3]], digits = 2)} s."))

   res <- resp %>%
    mr_resp_to_tibble()

  res
}

#' Retrieve Gazetteer Records by a list of Names
#'
#' @description
#' Retrieve all Records of the [Marine Gazetteer](https://marineregions.org/gazetteer.php) that contain one or more parameters in `names` in their `preferredGazetteerName`.
#'
#' @param names Names that are retrieved from the
#' @param like Formats the name into a SQL-like syntax. Default = TRUE.
#' @param fuzzy Uses Levenshtein query to find nearest matches. Default = FALSE.
#'
#' @return Tibble with Gazetteer records.
#' @export
#'
#' @examples
#' names <- c("Atlantic Ocean", "Vanuatu", "Mariana Trench")
#' result <- mr_gaz_records_by_names(names)
mr_gaz_records_by_names <- function(names, like = TRUE, fuzzy = FALSE){

  checkmate::assert_character(names)

  url <- mregions2::mr_req_URL(api_type = "rest", file_format = "json", method = "getGazetteerRecordsByNames")

  names <- names %>%
    utils::URLencode()

  if(length(names) > 1){
    names <- names %>%
      paste(collapse = "/")
    }

  req <- httr2::request(url) %>%
    httr2::req_headers(
      accept = "application/json")  %>%
    req_mr_user_agent() %>%
    httr2::req_url_path_append(like) %>%
    httr2::req_url_path_append(fuzzy) %>%
    httr2::req_url_path_append(names) %>%
    httr2::req_url_path_append("/")

  resp <- req %>%
    httr2::req_perform()

  res <- resp %>%
    mr_resp_to_tibble(unpack = TRUE)

  res
}

#' Retrieve Gazetter Relations by MRGID
#'
#'Retrieves marine gazetteer records that are related to a given input MRGID. Relationships can be parents (`upper`), children (`lower`) oder `both`.
#'Geographic types of the relationships can be specified, for example `partof` and `adjacentto`.
#'
#' @param mrgid The [Marine Regions Geographic IDentifier](https://marineregions.org/mrgid.php).
#' @param direction The hierarchical structure. Must be one of `c("upper", "lower", "both")`. `"upper"` lists all parents of the record. `"lower"` lists all childs of the record. `"both"` lists parents and childs of the record.
#' @param type must be one of `c("partof", "partlypartof", "adjacentto", "similarto", "administrativepartof", "influencedby", "all")`. Explanations of the `types` at: https://marineregions.org/ontology/documentation.html in chapter `Object Properties`.
#' @return a `tibble` with all relations for the given `MRGID`.
#' @export
#'
#' @examples
#' mariana_trench <- mr_gaz_records_by_names("Mariana Trench")
#' mariana_trench_mrgid <- mariana_trench$MRGID
#'
#' mariana_trench_relations <- mr_gaz_relations_by_MRGID(mariana_trench_mrgid)
#' mariana_trench_relations$preferredGazetteerName
mr_gaz_relations_by_MRGID <- function(mrgid, direction = "upper", type = "partof"){

  types <- c("partof", "partlypartof", "adjacentto", "similarto", "administrativepartof", "influencedby", "all")
  checkmate::assert_character(c("type", "direction"))
  checkmate::assert_numeric(mrgid)
  checkmate::assert_choice(type, types)
  checkmate::assert_choice(direction, c("upper", "lower", "both"))

  url <- mregions2::mr_req_URL(api_type = "rest", file_format = "json", method = "getGazetteerRelationsByMRGID")

  req <- httr2::request(url) %>%
    httr2::req_headers(
      accept = "application/json")  %>%
    req_mr_user_agent() %>%
    httr2::req_url_path_append(mrgid) %>%
    httr2::req_url_path_append("/")

  resp <- req %>%
    httr2::req_url_query(
      `direction` = direction,
      `type` = type) %>%
    req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  res <- resp %>%
    mr_resp_to_tibble()
  return(res)
}
