#' Get WMS information for a given MRGID
#'
#' @param mrgid (integer) A valid Marine Regions Gazetteer Identifier ([MRGID])
#'
#' @return a data frame with information from the WMS services including:
#' - `value`: the value to filter on
#' - `MRGID` : see [MRGID]
#' - `url`: the base URL of the WMS service
#' - `namespace`: see [mrp_view()] details
#' - `featureType`: see [mrp_view()] details
#' - `featureName`: see [mrp_view()] details
#'
#' @export
#' @seealso [gaz_rest], [MRGID], [mrp_view()]
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
