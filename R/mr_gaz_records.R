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

type <- "FAO subdivisions"
offset <- 0

mr_gaz_records_by_type <- function(type, offset = 0){

  checkmate::assert_character(type)

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
