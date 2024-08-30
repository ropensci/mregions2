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
#' @examples \donttest{
#' gaz_rest_wmses(3293)
#' }
gaz_rest_wmses <- function(mrgid){
  # Assertions
  mrgid <- checkmate::assert_integerish(mrgid, lower = 1, any.missing = FALSE,
                                       null.ok = TRUE, coerce = TRUE, len = 1)

  # Perform
  marineregions.org() %>%
    httr2::request() %>%
    httr2::req_url_path_append(glue::glue(
      "/rest/getGazetteerWMSes.json/{mrgid}/"
    )) %>%
    httr2::req_user_agent(mr_user_agent) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json() %>%
    dplyr::bind_rows()
}
