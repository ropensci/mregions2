#' Retrieve Gazetter Relations by MRGID
#'
#'Gazetteer Records that are related to a given input MRGID. Relationships can be parents (`upper`), children (`lower`) or both.
#'Geographic types can also be specified, for example `partof` and `adjacentto`.
#'
#' @param mrgid The [Marine Regions Geographic IDentifier](https://marineregions.org/mrgid.php).
#' @param with_geometries Logical. Add geometries to the result data frame? Default = FALSE
#' @param direction The hierarchical structure. Must be one of `c("upper", "lower", "both")`. `"upper"` lists all parents of the record. `"lower"` lists all childs of the record. `"both"` lists parents and childs of the record.
#' @param type must be one of `c("partof", "partlypartof", "adjacentto", "similarto", "administrativepartof", "influencedby", "all")`. Explanations of the `types` [here](https://marineregions.org/ontology/documentation.html) in chapter *Object Properties*.
#' @return a `tibble` with all relations for the given `MRGID`.
#' @export
#'
#' @examples
#' mariana_trench <- mr_gaz_records_by_names("Mariana Trench")
#' mariana_trench_mrgid <- mariana_trench$MRGID
#'
#' mariana_trench_relations <- mr_gaz_relations_by_MRGID(mariana_trench_mrgid)
#' mariana_trench_relations$preferredGazetteerName
mr_gaz_relations_by_MRGID <- function(mrgid, with_geometries = FALSE, direction = "both", type = "all"){

  # Assertions
  types <- c("partof", "partlypartof", "adjacentto", "similarto", "administrativepartof", "influencedby", "all")
  checkmate::assert_character(c("type", "direction"))
  checkmate::assert_integer(mrgid)
  checkmate::assert_choice(type, types)
  checkmate::assert_choice(direction, c("upper", "lower", "both"))

  # Config
  url <- glue::glue("https://marineregions.org/rest/getGazetteerRelationsByMRGID.json/{mrgid}/?direction={direction}&type={type}")

  # Perform
  resp <- httr2::request(url) %>%
    httr2::req_user_agent(mr_user_agent) %>%
    httr2::req_headers(accept = "application/json") %>%
    httr2::req_perform() %>%
    httr2::resp_body_json() %>%
    dplyr::bind_rows()

  if(with_geometries){
    resp <- resp %>% mr_add_geometry()
  }


  resp

}

