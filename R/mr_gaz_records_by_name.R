# Records by Name ####
#' Title
#'
#' @param name
#' @param like
#' @param fuzzy
#' @param offset
#' @param count
#'
#' @return
#' @export
#'
#' @examples
#' mr_gaz_records_by_name(name = "High Seas", count = 5)
#'
mr_gaz_records_by_name <- function(name, like = TRUE, fuzzy = FALSE, offset = 0, count = 100){

  checkmate::assert_character(name)

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
