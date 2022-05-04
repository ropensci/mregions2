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

  url <- mregions2::req_URL(api_type = "rest", file_format = "json", method = "getGazetteerRecordsByName")

  name <- utils::URLencode(name)

  # todo: get user agent from utils
  user_agent <- "1.0.8"

  req <- httr2::request(url) %>%
    httr2::req_headers(
      accept = "application/json",
      `User-Agent` = user_agent)  %>%
    httr2::req_url_path_append(name) %>%
    httr2::req_url_path_append("/")

  resp <- req %>%
    httr2::req_url_query(
      `like` = like,
      `fuzzy` = fuzzy,
      `offset` = offset,
      `count` = count) %>%
    httr2::req_perform()

  res_json <- resp %>%
    httr2::resp_body_json()

  res <- do.call(rbind, res_json) %>%
    tibble::as_tibble(res_json)

  return(res)
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

  url <- mregions2::req_URL(api_type = "rest", file_format = "json", method = "getGazetteerRecordsByType")

  type <- utils::URLencode(type)

  # todo: get user agent from utils
  user_agent <- "0.1.8"

  req <- httr2::request(url) %>%
    httr2::req_headers(
      accept = "application/json",
      `User-Agent` = user_agent)  %>%
    httr2::req_url_path_append(type) %>%
    httr2::req_url_path_append("/")

  resp <- req %>%
    httr2::req_url_query(`offset` = offset) %>%
    httr2::req_perform()

  res_json <- resp %>%
    httr2::resp_body_json()

  res <- do.call(rbind, res_json) %>%
    tibble::as_tibble(res_json)

  return(res)
}

#' Retrieve Gazetteer Records by Source
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

  url <- mregions2::req_URL(api_type = "rest", file_format = "json", method = "getGazetteerRecordsBySource")

  source <- utils::URLencode(source)

  # todo: get user agent from utils
  user_agent <- "0.1.8"

  req <- httr2::request(url) %>%
    httr2::req_headers(
      accept = "application/json",
      `User-Agent` = user_agent)  %>%
    httr2::req_url_path_append(source) %>%
    httr2::req_url_path_append("/")

  resp <- req %>%
    httr2::req_perform()

  res_json <- resp %>%
    httr2::resp_body_json()

  res <- do.call(rbind, res_json) %>%
    tibble::as_tibble(res_json)

  return(res)
}

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

  url_sources <- mregions2::req_URL(api_type = "rest", file_format = "json", method = "getGazetteerSources")
  url_placetypes <- mregions2::req_URL(api_type = "rest", file_format = "json", method = "getGazetteerTypes")

  ifelse(info == "sources", url <- url_sources, url <- url_placetypes)

  # todo: get user agent from utils
  user_agent <- "0.1.8"

  req <- httr2::request(url) %>%
    httr2::req_headers(
      accept = "application/json",
      `User-Agent` = user_agent)

  resp <- req %>%
    httr2::req_perform()

  res_json <- resp %>%
    httr2::resp_body_json()

  res <- do.call(rbind, res_json) %>%
    tibble::as_tibble(res_json)

  return(res)
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
#' oostende <- mr_gaz_records_by_latlon(ostende_lat, ostende_lon, lat_radius = 0, lon_radius = 0)
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

  url <- mregions2::req_URL(api_type = "rest", file_format = "json", method = "getGazetteerRecordsByLatLong")

  # todo: get user agent from utils
  user_agent <- "0.1.8"

  req <- httr2::request(url) %>%
    httr2::req_headers(
      accept = "application/json",
      `User-Agent` = user_agent)  %>%
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

  res_json <- resp %>%
    httr2::resp_body_json()

  res <- do.call(rbind, res_json) %>%
    tibble::as_tibble(res_json)

  return(res)
}
