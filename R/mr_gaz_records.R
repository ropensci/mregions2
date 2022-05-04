#' Retrieve Gazetteer Records by Name
#'
#' @param name GazetteerName of the marine region of interest. Search names here: https://marineregions.org/gazetteer.php?p=search
#' @param count Number of records to retrieve.
#' @param like Adds a '%'-sign before and after the GazetteerName.
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
  user_agent <- "mregions" %>%
    packageVersion() %>%
    as.character()

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
