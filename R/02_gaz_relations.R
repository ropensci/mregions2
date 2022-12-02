#' Walk the hierarchy of the MarineRegions Gazetter given a Geo-Object
#'
#' @param x the object from which the relations are retrieved
#' @param ... params to be passed to the REST method [gaz_rest_relations_by_mrgid]
#'
#' @export
#'
#' @examples
#' # Get the relations of the Belgian Exclusive Economic Zone
#' gaz_relations("Belgian Exclusive Economic Zone")
#'
#' # Or via gaz_search()
#' require(magrittr)
#' gaz_search("Belgian Exclusive Economic Zone") %>% gaz_relations()
#'
#' # Or using its mrgid
#' gaz_relations(3293)
gaz_relations <- function(x, ...){
  UseMethod("gaz_relations")
}

gaz_relations.numeric <- function(x, ...){
  x <- lapply(x, gaz_rest_relations_by_mrgid, ...)
  dplyr::bind_rows(x)
}

gaz_relations.data.frame <- function(x, ...){
  stopifnot("MRGID" %in% names(x))

  mrgid = x$MRGID
  gaz_relations.numeric(mrgid)
}

gaz_relations.character <- function(x, ...){
  x <- gaz_search(x)
  gaz_relations.numeric(x)
}

#' Retrieve Gazetter Relations by MRGID
#'
#'   Gazetteer Records that are related to a given input MRGID. Relationships can be parents (`upper`), children (`lower`) or both.
#'   Geographic types can also be specified, for example `partof` and `adjacentto`.
#'
#' @param mrgid The \href{https://marineregions.org/mrgid.php}{Marine Regions Geographic IDentifier}.
#' @param with_geometries Logical. Add geometries to the result data frame? Default = FALSE
#' @param direction The hierarchical structure. Must be one of `c("upper", "lower", "both")`. `"upper"` lists all parents of the
#'   record. `"lower"` lists all childs of the record. `"both"` lists parents and childs of the record.
#' @param type must be one of `c("partof", "partlypartof", "adjacentto", "similarto", "administrativepartof", "influencedby", "all")`.
#'   Explanations of the `types` [here](https://marineregions.org/ontology/documentation.html) in chapter *Object Properties*.
#'
#' @export
#'
#' @examples
#' mariana_trench <- gaz_search("Mariana Trench")
#' mariana_trench_mrgid <- mariana_trench$MRGID
#'
#' mariana_trench_relations <- gaz_rest_relations_by_mrgid(mariana_trench_mrgid)
#' mariana_trench_relations$preferredGazetteerName
gaz_rest_relations_by_mrgid <- function(mrgid, with_geometries = FALSE, direction = "both", type = "all"){

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
    resp <- resp %>% gaz_add_geometry()
  }


  resp

}

