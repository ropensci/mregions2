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

#' @export
#' @rdname gaz_relations
gaz_relations.numeric <- function(x, ...){
  lapply(unique(x), gaz_rest_relations_by_mrgid, ...) %>%
    dplyr::bind_rows() %>%
    new_mr_df()
}

#' @export
#' @rdname gaz_relations
gaz_relations.mr_df <- function(x, ...){
  mrgid = unique(x$MRGID)
  gaz_relations.numeric(mrgid, ...)
}


#' Retrieve Gazetter Relations by MRGID
#'
#'   Gazetteer Records that are related to a given input MRGID. Relationships can be parents (`upper`), children (`lower`) or both.
#'   Geographic types can also be specified, for example `partof` and `adjacentto`.
#'
#' @param mrgid The \href{https://marineregions.org/mrgid.php}{Marine Regions Geographic IDentifier}.
#' @param  Logical. Add geometries to the result data frame? Default = FALSE
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
gaz_rest_relations_by_mrgid <- function(mrgid, with_geometry = FALSE, direction = "both", type = "all"){

  # Assertions
  types <- c("partof", "partlypartof", "adjacentto", "similarto", "administrativepartof", "influencedby", "all")
  checkmate::assert_character(c("type", "direction"))
  type = tolower(type)
  direction = tolower(direction)

  mrgid = checkmate::assert_integerish(mrgid, lower = 1, any.missing = FALSE,
                                       null.ok = TRUE, coerce = TRUE, len = 1)
  checkmate::assert_choice(type, types)
  checkmate::assert_choice(direction, c("upper", "lower", "both"))

  # Config
  url <- glue::glue("https://marineregions.org/rest/getGazetteerRelationsByMRGID.json/{mrgid}/?direction={direction}&type={type}")

  # Extra info for status 404 not found
  .is_error <- function(resp){
    if(httr2::resp_status(resp) == 404){
      httr2::resp_check_status(resp, info = c(
        "i" = glue::glue("MRGID: <{mrgid}>"),
        "i" = glue::glue("type: `{type}`"),
        "i" = glue::glue("direction: `{direction}`")
      ))
    }

    if(httr2::resp_is_error(resp)){
      TRUE
    }else{
      FALSE
    }
  }

  # Perform
  resp <- httr2::request(url) %>%
    httr2::req_user_agent(mr_user_agent) %>%
    httr2::req_headers(accept = "application/json") %>%
    httr2::req_error(is_error = .is_error) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json() %>%
    dplyr::bind_rows()

  if(with_geometry){
    resp <- resp %>% gaz_add_geometry()
  }

  resp

}
