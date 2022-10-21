library(rdflib)
library(httr2)
library(glue)
library(magrittr)

# Check if cache dir exists and create if doesnt
mr_cache <- function(){

  path_cache <- file.path(Sys.getenv("MR_CACHE", unset = getwd()), "mr-cache/")
  # path_graph <- file.path(path_cache, "mr_graph.rdf")
  verbose <- getOption("verbose", default = TRUE)

  if(!dir.exists(path_cache)){
    dir.create(path_cache)

    if(verbose){
      cli::cli_alert_success(glue::glue("Cache dir created in: `{path_cache}`"))
    }

  }

  path_cache
}


# Creates a basic request with general settings
mr_request <- function(url, ...){
  req <- httr2::request(url) %>%
    httr2::req_user_agent("mregions2") %>%
    httr2::req_cache(mr_cache())

  # Add messages if verbose = TRUE
  verbose <- getOption("verbose", default = FALSE)
  if(verbose){
    req <- req %>%
      httr2::req_verbose(...) %>%
      httr2::req_cache(mr_cache(), debug = TRUE)
  }

  req
}

# Performs the request
mr_req_perform <- function(req, method = "GET"){
  resp <- req %>%
    httr2::req_perform(method)

  verbose <- getOption("verbose", default = FALSE)
  if(verbose){
    resp %>%
      httr2::resp_body_string() %>%
      cat()
  }

  resp
}

# Parse as RDF
mr_rdf_parse <- function(resp, ...){
  rdf <- resp %>%
    httr2::resp_body_string() %>%
    rdflib::rdf_parse(...)

  rdf
}

# Build base graph using known ontologies
mr_graph_base_build <- function(){

  # Cache control
  mr_base_graph_path = file.path(mr_cache(), "mr_graph_base.rds")

  if(file.exists(mr_base_graph_path)){
    seconds_since_creation = as.numeric(difftime(Sys.time(), file.mtime(mr_base_graph_path), units = "secs"))
    max_age = 604800

    if(seconds_since_creation < max_age){
      out <- readRDS(mr_base_graph_path)

      verbose <- getOption("verbose", default = TRUE)
      if(verbose){
        cli::cli_alert_success(glue::glue("Cached base graph is fresh. Reading from `{mr_base_graph_path}`"))
      }

      # Premature end
      return(out)

    }else if(seconds_since_creation > max_age){
      file.remove(seconds_since_creation)

    }

  }

  # If not cached, then perform

  # Get Turtle docs
  uri_turtle <- list(
    mr = "http://marineregions.org/ns/ontology",
    mrt = "http://marineregions.org/ns/placetypes",
    dcat = "https://www.w3.org/ns/dcat2",
    prov = "http://www.w3.org/ns/prov"
  )

  reqs_turtle <- lapply(uri_turtle, mr_request) %>%
    lapply(httr2::req_headers, accept = "text/turtle") %>%
    httr2::multi_req_perform(cancel_on_error = F) %>%
    lapply(mr_rdf_parse, format = "turtle")


  # Get RDFXML docs
  uri_rdfxml<- list(
    skos = 'https://www.w3.org/2009/08/skos-reference/skos.rdf',
    gsp = 'http://www.opengis.net/ont/geosparql'
  )

  reqs_rdfxml <- lapply(uri_rdfxml, mr_request) %>%
    lapply(httr2::req_headers, accept = "application/xml+rdf") %>%
    httr2::multi_req_perform(cancel_on_error = F) %>%
    lapply(mr_rdf_parse, format = "rdfxml")


  reqs <- c(reqs_turtle, reqs_rdfxml)

  # Paste all documents together
  out <- c(reqs[[1]])
  for(i in 2:length(reqs)){
    out <- c(out, reqs[[i]])
  }

  # Cache

  saveRDS(out, file = mr_base_graph_path)

  out

}

debug(mr_graph_base_build)
base_graph <- mr_graph_base_build()

readRDS("./mr-cache/mr_graph_base.rds")




mr_base_graph_path = file.path(mr_cache(), "mr_base_graph.rds")

if(file.exists(mr_base_graph_path)){
  seconds_since_creation = as.numeric(difftime(Sys.time(), file.mtime(mr_base_graph_path), units = "seconds"))
  max_age = 604800

  if(seconds_since_creation < max_age){
    out <- readRDS(mr_base_graph_path)

  }else if(seconds_since_creation > max_age){
    file.remove(seconds_since_creation)

  }

}


saveRDS(out, file = )













# debug(mr_get_rdf)
options(verbose = TRUE)
belgium <- mr_get_rdf("http://marineregions.org/mrgid/14")
france <- mr_get_rdf("http://marineregions.org/mrgid/17")

graph <- c(base_graph, belgium)



## CREATE GENERAL FUNCTION TO MERGE MRGIDS
## CREATE GENERAL FUNCTION TO APPLY SPARQL QUERY

## QUERIES
## GET ALL BASIC INFO

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

   ?placetype skos:inScheme <http://marineregions.org/ns/placetypes> .

  }"
)
# ?placetype skos:inScheme <http://marineregions.org/ns/placetypes> .
cat(query)
out <- rdflib::rdf_query(graph, query, data.frame = FALSE)


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

