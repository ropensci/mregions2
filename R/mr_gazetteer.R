
#' Create a new S3 object of class "MRGeoObject"
#'
#' Inherits the class "rdf" from the package "rdflib"
#'
#' @param x
#'
#' @return
#'
#' @examples
new_MRGeoObject <- function(x = list()){

  structure(
    x,
    class = c("MRGeoObject", "rdf")
  )

}

#' Get a Marine Regions GeoObject as RDF
#'
#' @param x a url, a mrgid or a name of a Marine Regions Geoobject as a string
#' @param ... aditional params for furter inheritance
#'
#' @return an object of class c("MRGeoObject", "rdf")
#' This is completely compatible with the rdflib package.
#' For instance, you can apply sparql queries with rdflib::rdf_query()
#'
#' Or you can serialize the file with rdflib::rdf_serialize()
#'
#' Add extra RDF documents to and create your own knowledge graph
#'
#' @export
#'
#' @examples
#' x <- mr_get("Belgium")
#' x <- mr_get("http://marineregions.org/mrgid/14)
#' x <- mr_get(14)
mr_get <- function(x, ...){
  UseMethod("mr_get")
}

#' @rdname mr_get
mr_get.default <- function(url, ...){

  resp <- mr_request(url) %>%
    httr2::req_headers(accept = "text/turtle") %>%
    httr2::req_perform("GET") %>%
    httr2::resp_body_string()

  verbose <- getOption("verbose", default = FALSE)
  if(verbose){
    cat(resp)
  }

  out <- resp %>%
    rdflib::rdf_parse("turtle")

  new_MRGeoObject(out)

}

#' @rdname mr_get
mr_get.numeric <- function(mrgid, ...){
  url <- glue::glue("http://marineregions.org/mrgid/{mrgid}")
  mr_get.default(url)
}

#' @rdname mr_get
mr_get.character <- function(string, ...){

  is_mr_url <- grepl("http://marineregions.org", string, fixed = TRUE)

  if(is_mr_url){
    out <- mr_get.default(string)
    return(out)
  }

  if(!is_mr_url){

    search <- mregions2::mr_gaz_records_by_names(string)

    if(nrow(search) == 0){
      warning(glue::glue("No matches found for \"{string}\""), .call = FALSE)
      return(invisible(NULL))
    }

    verbose <- getOption("verbose", default = FALSE)
    if(verbose){
      cli::cli_alert_warning(glue::glue("{nrow(search)} match(es) found for \"{string}\": '{paste0(search$preferredGazetteerName, collapse = '\', \'')}'"))
    }

    return(mr_get.numeric(search$MRGID))
  }
}


#' Returns all the prefixes for sparql queries
#'
#' @examples
#' mr_prefixes()
mr_prefixes <- function(){
  prefixes <- c(
  "prefix mr: <http://marineregions.org/ns/ontology#>",
  "prefix mrt: <http://marineregions.org/ns/placetypes#>",
  "prefix dc: <http://purl.org/dc/terms/>",
  "prefix xsd: <http://www.w3.org/2001/XMLSchema#>",
  "prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>",
  "prefix skos: <http://www.w3.org/2004/02/skos/core#>",
  "prefix dcat: <http://www.w3.org/ns/dcat#>",
  "prefix gsp: <http://www.opengis.net/ont/geosparql#>",
  "prefix prov: <http://www.w3.org/ns/prov#>"
  )

  paste0(prefixes, collapse = "\n")

}



#' Extract information from a Marine Regions Geo Object as tabular data
#'
#' @param x an object of class c("MRGeoObject", "rdf")
#' @param lang preferred language as 2c ISO_639_1. See details.
#' @param ... aditional params for furter inheritance
#'
#' @details
#'
#' The Marine Regions Geo Objects can be labels in several
#' languages. Choose your preferred language and, if the label exists
#' in your language, it will be returned. Otherwise the label will be in the preferred language.
#'
#' @return a tibble with one row per Marine Regions Geo Object
#' @export
#'
#' @examples
as_tibble.MRGeoObject <- function(x, lang = "en", ...){

  checkmate::assert_choice(lang, c("en", "es", "fr", "de", "nl", "it"))

  query <- paste0(
    mr_prefixes(),
    "
  SELECT ?mrgid
         (COALESCE(?altLabel, ?prefLabel) AS ?label)
         ?placetype
         ?modified
         ?source
         ?seeAlso
         ?centroid
         ?bbox
         ?the_geom

  WHERE {

    # Restrict to only the placetype
    ?mrgid a ?placetype .
      FILTER( STRSTARTS(STR(?placetype),str(mrt:)) )

    # modified is the filter - it is or should be in all mrgid
    ?mrgid dc:modified ?modified
    OPTIONAL { ?mrgid rdfs:seeAlso ?seeAlso }
    OPTIONAL { ?mrgid dcat:centroid ?centroid }
    OPTIONAL { ?mrgid dcat:bbox ?bbox }
    OPTIONAL { ?mrgid mr:hasGeometry ?the_geom }


    # Get the language
    OPTIONAL {
      ?mrgid skos:altLabel ?altLabel .
      filter langMatches(lang(?altLabel), \"nl\")
    }
    ?mrgid skos:prefLabel ?prefLabel .

    # Get the source
    OPTIONAL { ?mrgid prov:hadPrimarySource ?primarySource }
    OPTIONAL { ?primarySource prov:wasAttributedTo ?wasAttributedTo }
    OPTIONAL { ?wasAttributedTo rdfs:label ?source }

  }"
  )

  # Perform Query
  out <- rdflib::rdf_query(x, query) %>%
    suppressMessages()

  out
}

cuba_eez

tidyr::as_tibble(cuba_eez) %>% View()

mr_relations(cuba_eez) %>% View()


#' @rdname as_tibble.MRGeoObject
as.data.frame.MRGeoObject <- function(x, ...){
  x <- as_tibble.MRGeoObject(x)
  as.data.frame(x)
}


#' Extract the relationships of a given MRGeoObject
#'
#' @param x a MRGeoObject
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' cuba_eez_relationships <- mr_get(8406) %>% mr_relationships()
mr_relations <- function(x, ...){
  # 1. Get all predictes of ontology mr:
  # These are the relations plus hasGeometry
  query <- paste0(
    prefixes,
    "
  SELECT ?s ?relation ?mrgid
  WHERE {
    ?s ?relation ?mrgid .
    FILTER( STRSTARTS(STR(?relation),str(mr:)) )

  }"
  )

  suppressMessages(out <- rdflib::rdf_query(x, query, data.frame = FALSE))

  # Leave out geom, no idea how to do it in SPARQL
  # out <- subset(out, out$relation != "http://marineregions.org/ns/ontology#hasGeometry")

  # 2. Get all placetypes and labels of relations
  query2 <- paste0(
    prefixes,
    "
    SELECT ?mrgid ?placetype ?label
    WHERE {
      ?mrgid a ?placetype.
      FILTER( STRSTARTS(STR(?placetype),str(mrt:)) )

      ?mrgid skos:prefLabel ?label.


    }
    "
  )

  suppressMessages(labels <- rdflib::rdf_query(x, query2, data.frame = FALSE))


  # 3. Merge dataframes, rename and rearrange to provide human-readable dataset
  out <- merge(out, labels)

  out <- data.frame(
    relation = gsub("http://marineregions.org/ns/ontology#", "mr:",  out$relation),
    label = out$label,
    placetype = gsub("http://marineregions.org/ns/placetypes#", "mrt:", out$placetype),
    mrgid = out$mrgid
  )

  out <- out[order(out$relation, decreasing = FALSE), ]

  # End
  tidyr::as_tibble(out)

}
