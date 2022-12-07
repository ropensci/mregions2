#' Get WMS information for the given MRGID
#'
#' @param mrgid a valid marine regions gazetteer identifier
#'
#' @return a tibble with information from the WMS services,
#'   including the service url, the namespace, the featureType,
#'   featureName and the value to filter on
#' @export
#'
#' @examples
#' gaz_rest_wmses(3293)
gaz_rest_wmses <- function(mrgid){
  # Assertions
  mrgid = checkmate::assert_integerish(mrgid, lower = 1, any.missing = FALSE,
                                       null.ok = TRUE, coerce = TRUE, len = 1)

  # Config
  url <- glue::glue("https://marineregions.org/rest/getGazetteerWMSes.json/{mrgid}/")

  # Perform
  httr2::request(url) %>%
    httr2::req_user_agent("mregions2") %>%
    httr2::req_perform() %>%
    httr2::resp_body_json() %>%
    dplyr::bind_rows()

}
