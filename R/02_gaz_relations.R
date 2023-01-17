#' Walk the hierarchy of the MarineRegions Gazetter given a Gazetteer MRGID or Gazetteer entries
#'
#' @param x the object from which the relations are retrieved. Can be:
#'   * (integer) A valid Marine Regions Gazetteer Identifier ([MRGID]), passed to [gaz_rest_relations_by_mrgid()]
#'   * A data frame retrieved with [mregions2] via its functions [gaz_search()],
#'     [gaz_search_by_source()] or [gaz_search_by_type()].
#' @inheritDotParams gaz_rest_relations_by_mrgid -mrgid
#'
#' @details
#' You can pass the output of most `gaz_*` functions to `gaz_relations()` to retrieve the
#' related gazetteer entries
#'
#' ## Developer info
#' This is done in the method [gaz_relations.mr_df()]. `mr_df` is a class defined in
#' this package to ensure the data frame passed to gaz_relations has a variable with [MRGID].
#'
#' @export
#'
#' @examples
#' # Get the relations of the Belgian Exclusive Economic Zone
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
#'
#' @param mrgid (integer) A valid Marine Regions Gazetteer Identifier ([MRGID])
#' @param with_geometry (logical) Add geometries to the result data frame? Default = FALSE
#' @param direction (character) Must be one of `r c("upper", "lower", "both")`:
#' *  `upper`: lists all parents of the record.
#' * `lower`: lists all childs of the record.
#' * `both`: lists parents and childs of the record (default)
#' @param type (character) Must be one of `r c("partof", "partlypartof", "adjacentto", "similarto", "administrativepartof", "influencedby", "all")`.
#'
#' @export
#'
#' @seealso [List of types (Object Properties)](https://marineregions.org/ontology/documentation.html), [gaz_rest], [MRGID]
#'
#' @examples
#' gaz_rest_relations_by_mrgid(7378)
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
