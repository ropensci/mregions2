
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
    mr_req_perform() %>%
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
    "
  prefix mr: <http://marineregions.org/ns/ontology#>
  prefix mrt: <http://marineregions.org/ns/placetypes#>
  prefix dc: <http://purl.org/dc/terms/>
  prefix xsd: <http://www.w3.org/2001/XMLSchema#>
  prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  prefix skos: <http://www.w3.org/2004/02/skos/core#>
  prefix dcat: <http://www.w3.org/ns/dcat#>
  prefix gsp: <http://www.opengis.net/ont/geosparql#>
  prefix prov: <http://www.w3.org/ns/prov#>

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
    ?mrgid a ?placetype ;
             dc:modified ?modified ;
             rdfs:seeAlso ?seeAlso ;
             dcat:centroid ?centroid ;
             dcat:bbox ?bbox ;
             mr:hasGeometry ?the_geom .


    # Get the language
    OPTIONAL {
      ?mrgid skos:altLabel ?altLabel .
      filter langMatches(lang(?altLabel), \"nl\")
    }
    ?mrgid skos:prefLabel ?prefLabel .

    ?mrgid prov:hadPrimarySource ?primarySource .
    ?primarySource prov:wasAttributedTo ?wasAttributedTo .
    ?wasAttributedTo rdfs:label ?source .

    # Approach using Property Paths, but seem like they are not supported
    # ?mrgid prov:hadPrimarySource/prov:wasAttributedTo/rdfs:label ?source .

   ?placetype <http://www.w3.org/2004/02/skos/core#inScheme> <http://marineregions.org/ns/placetypes>.

  }"
  )


  # Add base_graph
  graph <- x %>%
    rdflib::rdf_serialize(format = "turtle") %>%
    c(mr_graph_base_build()) %>%
    paste0(collapse = "\n") %>%
    rdflib::rdf_parse(format = "turtle")

  # Perform Query
  out <- rdflib::rdf_query(x, query)

  # subset(out, out$placetype != "http://marineregions.org/ns/ontology#MRGeoObject")
  out
}


debug(as_tibble.MRGeoObject)
test <- tidyr::as_tibble(belgium)


as.data.frame.MRGeoObject <- function(x, ...){
  x <- as_tibble.MRGeoObject(x)
  as.data.frame(x)
}


belgium <- mr_get("http://marineregions.org/mrgid/14")







# debug(mr_get_rdf)
options(verbose = F)

belgium <- mr_get("http://marineregions.org/mrgid/14")
tidyr::as_tibble





france <- mr_get("http://marineregions.org/mrgid/17")

mr_


## CREATE GENERAL FUNCTION TO MERGE MRGIDS
## CREATE GENERAL FUNCTION TO APPLY SPARQL QUERY

## QUERIES
## GET ALL BASIC INFO





print(out, n = 100)

#
# "
# prov:hadPrimarySource [
#   prov:wasAttributedTo [
#     rdfs:label
#       \"(2001). The Times comprehensive atlas of the world. 10th ed. Times Books:
#       London. ISBN 0-7230-0792-6. 67, 220, 124 plates pp.\"^^xsd:string
#   ]
# ]
#
# "







## GET ALL RELATIONSHIPS
query <- paste0(
  prefix,
  "
  SELECT ?mrgid ?placetype ?prefLabel
  WHERE {
    ?mrgid a ?placetype ;
          skos:prefLabel ?prefLabel .

  }"
)

cat(query)
out <- rdflib::rdf_query(rdf, query, data.frame = FALSE)
out <- subset(out, out$placetype != 'http://marineregions.org/ns/ontology#MRGeoObject')
out <- subset(out, out$mrgid != 'http://marineregions.org/mrgid/14')
print(out, n = 100)

query <- paste0(
  prefix,
  "
  SELECT ?relationship ?value
  WHERE {
    <http://marineregions.org/mrgid/14> ?relationship ?value .
  }"
)

relations <- rdflib::rdf_query(rdf, query, data.frame = FALSE)
relations <- subset(relations, grepl("http://marineregions.org/ns/ontology#", relations$relationship, fixed = TRUE))

out <- subset(out, out$placetype != 'http://marineregions.org/ns/ontology#MRGeoObject')
out <- subset(out, out$mrgid != 'http://marineregions.org/mrgid/14')
print(out, n = 100)




## GET GEOMETRY








## GET PREFERRED LANGUAGE
query <- paste0(
  prefix,
  "
  SELECT (<http://marineregions.org/mrgid/14> as ?mrgid)
         (coalesce(?altLabel, ?prefLabel) as ?label)
  WHERE {


    OPTIONAL {
      <http://marineregions.org/mrgid/14> skos:altLabel ?altLabel
      filter langMatches(lang(?altLabel), \"nl\")
    }
    OPTIONAL {
      <http://marineregions.org/mrgid/14> skos:prefLabel ?prefLabel
    }

  }


  "
)

cat(query)
out <- rdflib::rdf_query(rdf, query, data.frame = FALSE)
out



#
# cat(cont)
# query <- paste0(
#   prefix,
#   "
#   SELECT ?mrgid ?name ?placetype
#   WHERE {
#     ?mrgid a ?placetype .
#     ?mrgid skos:prefLabel ?name .
#   }"
# )
#
# cat(query)
# rdflib::rdf_query(rdf, query, data.frame = FALSE)


cat(cont)
query <- paste0(
  prefix,
  "
  SELECT ?mrgid ?label ?geoobject
  WHERE {
    ?mrgid a ?geoobject .
    ?mrgid skos:altLabel ?label .
    FILTER (lang(?label) = 'en')
  }
  LIMIT 15
  "
)

cat(query)
rdflib::rdf_query(rdf, query, data.frame = FALSE)


# <http:\/\/marineregions.org\/ns\/ontology#(\w+)>
# FILTER (regex(?predicate, '/<http:\\/\\/marineregions.org\\/ns\\/ontology#(\\w+)>/g')) .

FILTER (regex(?predicate, '/mr:(\\w+)/g')) .


# Set prefixes
todelete <- gsub("@prefix(.*?)\n", "", resp)
prefix <- gsub(todelete, "", resp, fixed = TRUE)
prefix <- gsub("@", "", prefix, fixed = TRUE)
prefix <- gsub(" .", "", prefix, fixed = TRUE)
Sys.setenv(mr_prefix = prefix)



query <-
  "
  PREFIX mr: <http://marineregions.org/ns/ontology#>
  SELECT ?mrgid ?the_geom
  WHERE { ?mrgid mr:hasGeometry ?the_geom .}"


uri_geom <- rdflib::rdf_query(rdf, query)
#
# query <-
#   "
#   PREFIX mr: <http://marineregions.org/ns/ontology#>
#   SELECT ?a ?c
#   WHERE { ?a mr:contains ?c .}"
#
# contains <- rdflib::rdf_query(rdf, query, data.frame = FALSE)


query <-
  "
  PREFIX mr: <http://marineregions.org/ns/ontology#>
  PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
  SELECT ?mrgid ?contains ?name ?other_names ?the_geom
  WHERE { ?mrgid mr:contains ?contains .
          ?mrgid skos:prefLabel ?name .
          ?mrgid skos:altLabel ?other_names .
          ?mrgid mr:hasGeometry ?the_geom .
        }
"

contains <- rdflib::rdf_query(rdf, query, data.frame = T)


# How to select the skos:prefLabel from the relationships
# How to get the placetype?
# Create method to jump to next?


"http://marineregions.org/ns/ontology" # no cacheable
"http://marineregions.org/ns/placetypes.ttl" # no cacheable
"https://www.w3.org/ns/dcat2" # cache-control + expires
"http://www.w3.org/ns/prov" # cache-control + expires
"http://www.w3.org/2000/01/rdf-schema" # cache-control + expires
"http://www.opengis.net/ont/geosparql" # Last-Modified + ETag

'http://purl.org/dc/terms/'
"http://purl.org/dc/elements/1.1/"

