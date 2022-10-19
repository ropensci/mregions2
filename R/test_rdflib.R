library(rdflib)
library(httr)
library(glue)
mrgid <- 14
url <- glue("http://marineregions.org/mrgid/{mrgid}")

# correct RFC
# req <- GET("https://marineregions.org/mrgid/26567", accept("text/turtle"))
req <- GET(url, accept("text/turtle"))

# req <- GET(url, accept("application/ld+json"))

cont <- content(req, as = "text")
cat(cont)

rdf <- rdflib::rdf_parse(cont, format = "turtle")
# rdf <- rdflib::rdf_parse(cont, format = "jsonld")

# Get all prefixes
todelete <- gsub("@prefix(.*?)\n", "", cont)
prefix <- gsub(todelete, "", cont, fixed = TRUE)
prefix <- gsub("@", "", prefix, fixed = TRUE)
prefix <- gsub(" .", "", prefix, fixed = TRUE)

cat(cont)

## CREATE GENERAL FUNCTION TO GET MRGID AS RDF
## CREATE GENERAL FUNCTION TO MERGE MRGIDS
## CREATE GENERAL FUNCTION TO APPLY SPARQL QUERY

## QUERIES
## GET ALL BASIC INFO
query <- paste0(
  prefix,
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
    ?mrgid a ?placetype ;
             dc:modified ?modified ;
             rdfs:seeAlso ?seeAlso ;
             dcat:centroid ?centroid ;
             dcat:bbox ?bbox ;
             mr:hasGeometry ?the_geom .

    OPTIONAL {
      ?mrgid skos:altLabel ?altLabel
      filter langMatches(lang(?altLabel), \"nl\")
    }
    OPTIONAL {
      ?mrgid skos:prefLabel ?prefLabel
    }

    ?mrgid prov:hadPrimarySource ?primarySource .
    ?primarySource prov:wasAttributedTo ?wasAttributedTo .
    ?wasAttributedTo rdfs:label ?source .

  }"
)

cat(query)
out <- rdflib::rdf_query(rdf, query, data.frame = FALSE)
out <- subset(out, out$placetype != 'http://marineregions.org/ns/ontology#MRGeoObject')
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
