# mregions2 as RDF

This article describes how to access the Marine Regions Gazetteer
entries and geometries as RDF, which are loaded into R via
[`rdflib::rdf`](https://docs.ropensci.org/rdflib/reference/rdf.html).
Mirroring the Marine Regions Gazetteer via [Linked Data Event
Streams](https://w3id.org/ldes/specification) is discussed.

The following libraries are required

``` r

library(mregions2)
library(rdflib)

# Use the pipe operator `%>%`
library(magrittr)

# For visualizing
library(mapview)
library(jsonlite)
```

## Retrieving the Marine Regions Gazetteer as RDF

Known an
[`?MRGID`](https://docs.ropensci.org/mregions2/reference/MRGID.md), you
can get the gazetter entry as RDF.

E.g. the Belgian Exclusive Economic Zone or Belgian Part of the North
Sea has the
[`?MRGID`](https://docs.ropensci.org/mregions2/reference/MRGID.md):
`<https://marineregions.org/mrgid/3293>`

``` r

gaz_search(3293) %>% 
  gaz_geometry() %>%
  mapview()
```

Using
[`gaz_search()`](https://docs.ropensci.org/mregions2/reference/gaz_search.md)
or the underlying service
[`gaz_rest_record_by_mrgid()`](https://docs.ropensci.org/mregions2/reference/gaz_rest_record_by_mrgid.md)
with the argument `rdf = TRUE` will:

1.  Use content negotiation to get the gazetteer record in Turtle
    format.
2.  Read this document as RDF using the `rdflib` R package
    ([`?rdflib::rdflib`](https://docs.ropensci.org/rdflib/reference/rdflib-package.html))

HTTP requests are handled with `httr2`
([`?httr2::httr2`](https://httr2.r-lib.org/reference/httr2-package.html))

``` r

bpns <- gaz_search(3293, rdf = TRUE)

class(bpns)
#> [1] "rdf"

bpns
#> Total of 99 triples, stored in hashes
#> -------------------------------
#> @base <localhost://> .
#> @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
#> 
#> <http://marineregions.org/mrgid/14>
#>     a <http://marineregions.org/ns/ontology#MRGeoObject>, <http://marineregions.org/ns/placetypes#Nation> ;
#>     <http://www.w3.org/2004/02/skos/core#prefLabel> "Belgium"@en .
#> 
#> <http://marineregions.org/mrgid/17401>
#>     a <http://marineregions.org/ns/ontology#MRGeoObject>, <http://marineregions.org/ns/placetypes#Wreck> ;
#>     <http://www.w3.org/2004/02/skos/core#prefLabel> "Birkenfels"@en .
#> 
#> ... with 89 more triples
```

There are many stores returned. This is because the returned object
includes not only info about the Gazetteer record, but also about its
hierarchy. The Marine Regions Gazetteer is hierarchical and there are
many relations between their records

The relations are explained the Marine Regions Ontology:

> `http://marineregions.org/ns/ontology`

There is also information about the Place Type. The Place Types are
defined in:

> `http://marineregions.org/ns/placetypes`

Both the ontology and the placetypes are available as HTML when using a
web browser, but also as Turtle or JSON-LD by using content negotiation.

You can then use
[`rdflib::rdflib`](https://docs.ropensci.org/rdflib/reference/rdflib-package.html)
to apply common RDF applications.

### Serialize

You can serialize the record in other formats, e.g. JSON-LD

``` r

# Create a placeholder file
bpns_as_jsonld <- tempfile(fileext = ".rdf")

# Serialize
bpns %>% rdf_serialize(
  doc = bpns_as_jsonld, 
  format = "jsonld"
)

readLines(bpns_as_jsonld) %>% jsonlite::prettify()
```

Show Output

    #> {
    #>     "@graph": [
    #>         {
    #>             "@id": "http://marineregions.org/mrgid/14",
    #>             "@type": [
    #>                 "http://marineregions.org/ns/placetypes#Nation",
    #>                 "http://marineregions.org/ns/ontology#MRGeoObject"
    #>             ],
    #>             "http://www.w3.org/2004/02/skos/core#prefLabel": {
    #>                 "@language": "en",
    #>                 "@value": "Belgium"
    #>             }
    #>         },
    #>         {
    #>             "@id": "http://marineregions.org/mrgid/17401",
    #>             "@type": [
    #>                 "http://marineregions.org/ns/placetypes#Wreck",
    #>                 "http://marineregions.org/ns/ontology#MRGeoObject"
    #>             ],
    #>             "http://www.w3.org/2004/02/skos/core#prefLabel": {
    #>                 "@language": "en",
    #>                 "@value": "Birkenfels"
    #>             }
    #>         },
    #>         {
    #>             "@id": "http://marineregions.org/mrgid/17409",
    #>             "@type": [
    #>                 "http://marineregions.org/ns/placetypes#Wreck",
    #>                 "http://marineregions.org/ns/ontology#MRGeoObject"
    #>             ],
    #>             "http://www.w3.org/2004/02/skos/core#prefLabel": {
    #>                 "@language": "en",
    #>                 "@value": "Bourrasque"
    #>             }
    #>         },
    #>         {
    #>             "@id": "http://marineregions.org/mrgid/17666",
    #>             "@type": [
    #>                 "http://marineregions.org/ns/placetypes#Wreck",
    #>                 "http://marineregions.org/ns/ontology#MRGeoObject"
    #>             ],
    #>             "http://www.w3.org/2004/02/skos/core#prefLabel": {
    #>                 "@language": "en",
    #>                 "@value": "Kilmore Shipwreck"
    #>             }
    #>         },
    #>         {
    #>             "@id": "http://marineregions.org/mrgid/17865",
    #>             "@type": [
    #>                 "http://marineregions.org/ns/placetypes#Wreck",
    #>                 "http://marineregions.org/ns/ontology#MRGeoObject"
    #>             ],
    #>             "http://www.w3.org/2004/02/skos/core#prefLabel": {
    #>                 "@language": "de",
    #>                 "@value": "Sperrbrecher"
    #>             }
    #>         },
    #>         {
    #>             "@id": "http://marineregions.org/mrgid/19673",
    #>             "@type": [
    #>                 "http://marineregions.org/ns/placetypes#InhabitedPlace",
    #>                 "http://marineregions.org/ns/ontology#MRGeoObject"
    #>             ],
    #>             "http://www.w3.org/2004/02/skos/core#prefLabel": {
    #>                 "@language": "nl",
    #>                 "@value": "Nieuwpoort-Bad"
    #>             }
    #>         },
    #>         {
    #>             "@id": "http://marineregions.org/mrgid/2350",
    #>             "@type": [
    #>                 "http://marineregions.org/ns/placetypes#IHOSeaArea",
    #>                 "http://marineregions.org/ns/ontology#MRGeoObject"
    #>             ],
    #>             "http://www.w3.org/2004/02/skos/core#prefLabel": {
    #>                 "@language": "en",
    #>                 "@value": "North Sea"
    #>             }
    #>         },
    #>         {
    #>             "@id": "http://marineregions.org/mrgid/2419",
    #>             "@type": [
    #>                 "http://marineregions.org/ns/placetypes#SandbankSystem",
    #>                 "http://marineregions.org/ns/ontology#MRGeoObject"
    #>             ],
    #>             "http://www.w3.org/2004/02/skos/core#prefLabel": {
    #>                 "@language": "en",
    #>                 "@value": "Hinder Banks"
    #>             }
    #>         },
    #>         {
    #>             "@id": "http://marineregions.org/mrgid/2420",
    #>             "@type": [
    #>                 "http://marineregions.org/ns/placetypes#SandbankSystem",
    #>                 "http://marineregions.org/ns/ontology#MRGeoObject"
    #>             ],
    #>             "http://www.w3.org/2004/02/skos/core#prefLabel": {
    #>                 "@language": "en",
    #>                 "@value": "Zeeland Banks"
    #>             }
    #>         },
    #>         {
    #>             "@id": "http://marineregions.org/mrgid/2421",
    #>             "@type": [
    #>                 "http://marineregions.org/ns/placetypes#SandbankSystem",
    #>                 "http://marineregions.org/ns/ontology#MRGeoObject"
    #>             ],
    #>             "http://www.w3.org/2004/02/skos/core#prefLabel": {
    #>                 "@language": "en",
    #>                 "@value": "Flemish Banks"
    #>             }
    #>         },
    #>         {
    #>             "@id": "http://marineregions.org/mrgid/24676",
    #>             "@type": [
    #>                 "http://marineregions.org/ns/placetypes#Sandbank",
    #>                 "http://marineregions.org/ns/ontology#MRGeoObject"
    #>             ],
    #>             "http://www.w3.org/2004/02/skos/core#prefLabel": {
    #>                 "@language": "nl",
    #>                 "@value": "Lodewijkbank"
    #>             }
    #>         },
    #>         {
    #>             "@id": "http://marineregions.org/mrgid/2550",
    #>             "@type": [
    #>                 "http://marineregions.org/ns/placetypes#Coast",
    #>                 "http://marineregions.org/ns/ontology#MRGeoObject"
    #>             ],
    #>             "http://www.w3.org/2004/02/skos/core#prefLabel": {
    #>                 "@language": "en",
    #>                 "@value": "Belgian Coast"
    #>             }
    #>         },
    #>         {
    #>             "@id": "http://marineregions.org/mrgid/26567",
    #>             "@type": [
    #>                 "http://marineregions.org/ns/placetypes#MarineRegion",
    #>                 "http://marineregions.org/ns/ontology#MRGeoObject"
    #>             ],
    #>             "http://www.w3.org/2004/02/skos/core#prefLabel": {
    #>                 "@language": "en",
    #>                 "@value": "Belgian part of the North Sea"
    #>             }
    #>         },
    #>         {
    #>             "@id": "http://marineregions.org/mrgid/27234",
    #>             "@type": [
    #>                 "http://marineregions.org/ns/placetypes#Natura2000SpecialProtectionAreaAndSiteOfCommunityImportanceSPAAndSCIEUBirdsAndHabitatsDirective",
    #>                 "http://marineregions.org/ns/ontology#MRGeoObject"
    #>             ],
    #>             "http://www.w3.org/2004/02/skos/core#prefLabel": {
    #>                 "@language": "nl",
    #>                 "@value": "Westerschelde & Saeftinghe"
    #>             }
    #>         },
    #>         {
    #>             "@id": "http://marineregions.org/mrgid/27555",
    #>             "@type": [
    #>                 "http://marineregions.org/ns/placetypes#Natura2000SiteOfCommunityImportanceSCIEUHabitatsDirective",
    #>                 "http://marineregions.org/ns/ontology#MRGeoObject"
    #>             ],
    #>             "http://www.w3.org/2004/02/skos/core#prefLabel": {
    #>                 "@language": "nl",
    #>                 "@value": "Vlakte Van de Raan"
    #>             }
    #>         },
    #>         {
    #>             "@id": "http://marineregions.org/mrgid/27637",
    #>             "@type": [
    #>                 "http://marineregions.org/ns/placetypes#Natura2000SiteOfCommunityImportanceSCIEUHabitatsDirective",
    #>                 "http://marineregions.org/ns/ontology#MRGeoObject"
    #>             ],
    #>             "http://www.w3.org/2004/02/skos/core#prefLabel": {
    #>                 "@language": "fr",
    #>                 "@value": "Dunes de La Plaine Maritime Flamande"
    #>             }
    #>         },
    #>         {
    #>             "@id": "http://marineregions.org/mrgid/27790",
    #>             "@type": [
    #>                 "http://marineregions.org/ns/placetypes#Natura2000SpecialProtectionAreaSPAEUBirdsDirective",
    #>                 "http://marineregions.org/ns/ontology#MRGeoObject"
    #>             ],
    #>             "http://www.w3.org/2004/02/skos/core#prefLabel": {
    #>                 "@language": "nl",
    #>                 "@value": "Sbz 2 / Zps 2"
    #>             }
    #>         },
    #>         {
    #>             "@id": "http://marineregions.org/mrgid/28140",
    #>             "@type": [
    #>                 "http://marineregions.org/ns/placetypes#Natura2000SpecialProtectionAreaSPAEUBirdsDirective",
    #>                 "http://marineregions.org/ns/ontology#MRGeoObject"
    #>             ],
    #>             "http://www.w3.org/2004/02/skos/core#prefLabel": {
    #>                 "@language": "nl",
    #>                 "@value": "Zwin"
    #>             }
    #>         },
    #>         {
    #>             "@id": "http://marineregions.org/mrgid/28142",
    #>             "@type": [
    #>                 "http://marineregions.org/ns/placetypes#Natura2000SiteOfCommunityImportanceSCIEUHabitatsDirective",
    #>                 "http://marineregions.org/ns/ontology#MRGeoObject"
    #>             ],
    #>             "http://www.w3.org/2004/02/skos/core#prefLabel": {
    #>                 "@language": "nl",
    #>                 "@value": "Zwin"
    #>             }
    #>         },
    #>         {
    #>             "@id": "http://marineregions.org/mrgid/28226",
    #>             "@type": [
    #>                 "http://marineregions.org/ns/placetypes#Natura2000SiteOfCommunityImportanceSCIEUHabitatsDirective",
    #>                 "http://marineregions.org/ns/ontology#MRGeoObject"
    #>             ],
    #>             "http://www.w3.org/2004/02/skos/core#prefLabel": {
    #>                 "@language": "nl",
    #>                 "@value": "Schelde- En Durmeëstuarium Van de Nederlandse Grens Tot Gent"
    #>             }
    #>         },
    #>         {
    #>             "@id": "http://marineregions.org/mrgid/28318",
    #>             "@type": [
    #>                 "http://marineregions.org/ns/placetypes#Natura2000SpecialProtectionAreaSPAEUBirdsDirective",
    #>                 "http://marineregions.org/ns/ontology#MRGeoObject"
    #>             ],
    #>             "http://www.w3.org/2004/02/skos/core#prefLabel": {
    #>                 "@language": "nl",
    #>                 "@value": "Sbz 1 / Zps 1"
    #>             }
    #>         },
    #>         {
    #>             "@id": "http://marineregions.org/mrgid/3293",
    #>             "@type": [
    #>                 "http://marineregions.org/ns/placetypes#EEZ",
    #>                 "http://marineregions.org/ns/ontology#MRGeoObject"
    #>             ],
    #>             "http://marineregions.org/ns/ontology#contains": [
    #>                 {
    #>                     "@id": "http://marineregions.org/mrgid/28318"
    #>                 },
    #>                 {
    #>                     "@id": "http://marineregions.org/mrgid/27790"
    #>                 },
    #>                 {
    #>                     "@id": "http://marineregions.org/mrgid/27555"
    #>                 },
    #>                 {
    #>                     "@id": "http://marineregions.org/mrgid/17865"
    #>                 },
    #>                 {
    #>                     "@id": "http://marineregions.org/mrgid/17666"
    #>                 },
    #>                 {
    #>                     "@id": "http://marineregions.org/mrgid/17409"
    #>                 },
    #>                 {
    #>                     "@id": "http://marineregions.org/mrgid/17401"
    #>                 },
    #>                 {
    #>                     "@id": "http://marineregions.org/mrgid/4675"
    #>                 },
    #>                 {
    #>                     "@id": "http://marineregions.org/mrgid/2421"
    #>                 },
    #>                 {
    #>                     "@id": "http://marineregions.org/mrgid/2420"
    #>                 },
    #>                 {
    #>                     "@id": "http://marineregions.org/mrgid/2419"
    #>                 }
    #>             ],
    #>             "http://marineregions.org/ns/ontology#hasGeometry": {
    #>                 "@id": "http://marineregions.org/mrgid/3293/geometries?source=79&attributeValue=3293"
    #>             },
    #>             "http://marineregions.org/ns/ontology#isAdjacentTo": [
    #>                 {
    #>                     "@id": "http://marineregions.org/mrgid/19673"
    #>                 },
    #>                 {
    #>                     "@id": "http://marineregions.org/mrgid/2550"
    #>                 }
    #>             ],
    #>             "http://marineregions.org/ns/ontology#isPartOf": [
    #>                 {
    #>                     "@id": "http://marineregions.org/mrgid/2350"
    #>                 },
    #>                 {
    #>                     "@id": "http://marineregions.org/mrgid/14"
    #>                 }
    #>             ],
    #>             "http://marineregions.org/ns/ontology#isPreferredAlternativeOf": {
    #>                 "@id": "http://marineregions.org/mrgid/26567"
    #>             },
    #>             "http://marineregions.org/ns/ontology#partlyContains": [
    #>                 {
    #>                     "@id": "http://marineregions.org/mrgid/28226"
    #>                 },
    #>                 {
    #>                     "@id": "http://marineregions.org/mrgid/28142"
    #>                 },
    #>                 {
    #>                     "@id": "http://marineregions.org/mrgid/28140"
    #>                 },
    #>                 {
    #>                     "@id": "http://marineregions.org/mrgid/27637"
    #>                 },
    #>                 {
    #>                     "@id": "http://marineregions.org/mrgid/27234"
    #>                 },
    #>                 {
    #>                     "@id": "http://marineregions.org/mrgid/24676"
    #>                 }
    #>             ],
    #>             "http://purl.org/dc/terms/modified": {
    #>                 "@type": "http://www.w3.org/2001/XMLSchema#dateTime",
    #>                 "@value": "2023-10-28T08:26:23Z"
    #>             },
    #>             "http://www.w3.org/2004/02/skos/core#altLabel": {
    #>                 "@language": "en",
    #>                 "@value": "Belgian Continental Shelf"
    #>             },
    #>             "http://www.w3.org/2004/02/skos/core#prefLabel": {
    #>                 "@language": "en",
    #>                 "@value": "Belgian Exclusive Economic Zone"
    #>             },
    #>             "http://www.w3.org/ns/dcat#bbox": {
    #>                 "@type": "http://www.opengis.net/ont/geosparql#wktLiteral",
    #>                 "@value": "<http://www.opengis.net/def/crs/OGC/1.3/CRS84> POLYGON ((4.40204 51.87611,2.23833 51.87611,2.23833 51.03976,4.40204 51.03976,4.40204 51.87611))"
    #>             },
    #>             "http://www.w3.org/ns/dcat#centroid": {
    #>                 "@type": "http://www.opengis.net/ont/geosparql#wktLiteral",
    #>                 "@value": "<http://www.opengis.net/def/crs/OGC/1.3/CRS84> POINT (2.72132 51.46321)"
    #>             },
    #>             "http://www.w3.org/ns/hydra/core#next": {
    #>                 "@id": "http://marineregions.org/mrgid/3293?page=2"
    #>             },
    #>             "http://www.w3.org/ns/prov#hadPrimarySource": {
    #>                 "@id": "http://www.marineregions.org"
    #>             }
    #>         },
    #>         {
    #>             "@id": "http://marineregions.org/mrgid/4675",
    #>             "@type": [
    #>                 "http://marineregions.org/ns/placetypes#SandbankSystem",
    #>                 "http://marineregions.org/ns/ontology#MRGeoObject"
    #>             ],
    #>             "http://www.w3.org/2004/02/skos/core#prefLabel": {
    #>                 "@language": "en",
    #>                 "@value": "Coastal Banks"
    #>             }
    #>         },
    #>         {
    #>             "@id": "http://www.marineregions.org",
    #>             "http://www.w3.org/2000/01/rdf-schema#label": "Flanders Marine Institute (2023). Maritime Boundaries Geodatabase: Maritime Boundaries and Exclusive Economic Zones (200NM), version 12. Available online at http://www.marineregions.org/. https://doi.org/10.14284/632"
    #>         }
    #>     ]
    #> }

### SPARQL Queries

[SPARQL](https://www.w3.org/TR/sparql11-query/) queries can be applied
to the RDF document. Note: The
[`?MRGID`](https://docs.ropensci.org/mregions2/reference/MRGID.md) of
the Belgian Part of the North Sea is
`<http://marineregions.org/mrgid/3293>`

#### Extract all info from the record

``` r

sparql <- "
  SELECT ?p ?o
  WHERE {
    <http://marineregions.org/mrgid/3293> ?p ?o
  }"

rdf_query(bpns, query = sparql)
#> # A tibble: 32 × 2
#>    p                                             o                              
#>    <chr>                                         <chr>                          
#>  1 http://marineregions.org/ns/ontology#contains http://marineregions.org/mrgid…
#>  2 http://marineregions.org/ns/ontology#contains http://marineregions.org/mrgid…
#>  3 http://marineregions.org/ns/ontology#contains http://marineregions.org/mrgid…
#>  4 http://marineregions.org/ns/ontology#contains http://marineregions.org/mrgid…
#>  5 http://marineregions.org/ns/ontology#contains http://marineregions.org/mrgid…
#>  6 http://marineregions.org/ns/ontology#contains http://marineregions.org/mrgid…
#>  7 http://marineregions.org/ns/ontology#contains http://marineregions.org/mrgid…
#>  8 http://marineregions.org/ns/ontology#contains http://marineregions.org/mrgid…
#>  9 http://marineregions.org/ns/ontology#contains http://marineregions.org/mrgid…
#> 10 http://marineregions.org/ns/ontology#contains http://marineregions.org/mrgid…
#> # ℹ 22 more rows
```

#### Extract all relationship

The following example extracts all the predicates with the prefix `mr:`,
pointing to the Marine Regions Ontology at
<http://marineregions.org/ns/ontology>.

``` r

sparql <- "
  PREFIX mr: <http://marineregions.org/ns/ontology#> 
  SELECT ?s ?p ?o
  WHERE {
    ?s ?p ?o .
    FILTER( STRSTARTS(STR(?p), str(mr:)) )
  }"

rdf_query(bpns, query = sparql)
#> # A tibble: 23 × 3
#>    s                                   p                                   o    
#>    <chr>                               <chr>                               <chr>
#>  1 http://marineregions.org/mrgid/3293 http://marineregions.org/ns/ontolo… http…
#>  2 http://marineregions.org/mrgid/3293 http://marineregions.org/ns/ontolo… http…
#>  3 http://marineregions.org/mrgid/3293 http://marineregions.org/ns/ontolo… http…
#>  4 http://marineregions.org/mrgid/3293 http://marineregions.org/ns/ontolo… http…
#>  5 http://marineregions.org/mrgid/3293 http://marineregions.org/ns/ontolo… http…
#>  6 http://marineregions.org/mrgid/3293 http://marineregions.org/ns/ontolo… http…
#>  7 http://marineregions.org/mrgid/3293 http://marineregions.org/ns/ontolo… http…
#>  8 http://marineregions.org/mrgid/3293 http://marineregions.org/ns/ontolo… http…
#>  9 http://marineregions.org/mrgid/3293 http://marineregions.org/ns/ontolo… http…
#> 10 http://marineregions.org/mrgid/3293 http://marineregions.org/ns/ontolo… http…
#> # ℹ 13 more rows
```

#### Extract geometries

You can see in the predicates that one of the relationships is
`mr:hasGeometry`, pointing to the object
<http://marineregions.org/mrgid/3293/geometries?source=79&attributeValue=3293>

This URI can be requested as Turtle with content negotiation and load as
an [`rdflib::rdf`](https://docs.ropensci.org/rdflib/reference/rdf.html)
object of the
[`rdflib::rdflib`](https://docs.ropensci.org/rdflib/reference/rdflib-package.html)
package

But you could also use
[`gaz_geometry()`](https://docs.ropensci.org/mregions2/reference/gaz_geometry.md),
which already takes care of this:

``` r

bpns_geom <- gaz_geometry(3293, format = "rdf")
bpns_geom
#> Total of 4 triples, stored in hashes
#> -------------------------------
#> @base <localhost://> .
#> @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
#> 
#> <http://marineregions.org/mrgid/3293>
#>     <http://marineregions.org/ns/ontology#hasGeometry> <http://marineregions.org/mrgid/3293/geometries?source=79&attributeValue=3293> .
#> 
#> <http://marineregions.org/mrgid/3293/geometries?source=79&attributeValue=3293>
#>     <http://www.opengis.net/ont/geosparql#asWKT> "<http://www.opengis.net/def/crs/OGC/1.3/CRS84> MULTIPOLYGON (((4.24204865 51.35396728, 4.23073486 51.35824973, 4.22674608 51.3597591, 4.22206664 51.36451072, 4.21992833 51.36832287, 4.21826057 51.3722099, 4.21757767 51.37388563, 4.23069032 51.37435976, 4.24281066 51.37479669, 4.24280834 51.37379777, 4.24286056 51.37125123, 4.24316621 51.37117624, 4.24347174 51.37103796, 4.24370706 51.37068141, 4.24389255 51.37008905, 4.24410796 51.36954224, 4.24431884 51.36908472, 4.24455225 51.36867034, 4.24476385 51.36816514, 4.244982 51.36747491, 4.24517226 51.36665845, 4.24540901 51.36617172, 4.24569237 51.36597717, 4.24592257 51.36578608, 4.2461102 51.36545002, 4.24635875 51.36502087, 4.24667907 51.36452293, 4.24708426 51.3640573, 4.24739075 51.36374235, 4.24766016 51.36358285, 4.24793911 51.36349225, 4.24815035 51.36336243, 4.24830627 51.36318815, 4.24852848 51.36304903, 4.24884415 51.36296606, 4.24951494 51.36254585, 4.24964416 51.36235881, 4.24986637 51.36222887, 4.25015581 51.36212027, 4.25039911 51.36190856, 4.25059891 51.36154664, 4.2508322 51.36113214, 4.25104964 51.36071563, 4.25122023 51.36036766, 4.25120437 51.36005867, 4.25105608 51.35971463, 4.25103486 51.35929966, 4.25122154 51.35886765, 4.25153267 51.35844135, 4.25197184 51.35812736, 4.25237775 51.35795462, 4.25264132 51.35774934, 4.25282514 51.35735238, 4.25306118 51.3567121, 4.25336373 51.35580945, 4.25374603 51.3549242, 4.25405955 51.35420597, 4.25446916 51.35381556, 4.25514305 51.35358524, 4.25597405 51.35329926, 4.25640941 51.35323215, 4.25703716 51.35304177, 4.25780952 51.35283232, 4.25875545 51.35251355, 4.25990212 51.35209715, 4.26115978 51.35158443, 4.26214445 51.35124135, 4.26276684 51.35104811, 4.26322973 51.35092497, 4.26375794 51.35077536, 4.2644906 51.35057676, 4.26542091 51.35031605, 4.26657116 51.35011256, 4.26806045 51.34995425, 4.26999736 51.3498317, 4.27169931 51.34968376, 4.27310896 51.34947884, 4.2739408 51.34914756, 4.27430713 51.34878492, 4.27465534 51.34853566, 4.27518356 51.34834254, 4.27567434 51.34809124, 4.27599847 51.34784865, 4.27636945 51.34769225, 4.27691984 51.34761214, 4.2774713 51.34753001, 4.27813995 51.34715617, 4.27895296 51.34700775, 4.27982485 51.34671736, 4.2804991 51.34649265, 4.28139222 51.34642828, 4.28215015 51.34636915, 4.28231776 51.34635615, 4.28515208 51.34621382, 4.28513682 51.34572017, 4.28229797 51.34590232, 4.28213406 51.34590447, 4.28046465 51.34602058, 4.27895296 51.34580612, 4.27857506 51.34544134, 4.27860951 51.34509802, 4.27946031 51.34476471, 4.27970386 51.34465575, 4.28015554 51.34445417, 4.28122044 51.34441125, 4.28208721 51.34437001, 4.28222716 51.34436846, 4.28290582 51.34433091, 4.28542697 51.34408951, 4.28530848 51.34357452, 4.28222716 51.34392774, 4.28205538 51.34393477, 4.28101432 51.34398222, 4.28029275 51.34381056, 4.27898741 51.3437891, 4.27805984 51.34370315, 4.27702594 51.34381342, 4.27623463 51.34392667, 4.27569234 51.3440814, 4.2751559 51.34425855, 4.27440667 51.34442008, 4.27335477 51.34453964, 4.27207744 51.34447253, 4.27125955 51.34413886, 4.27116966 51.34363711, 4.27156842 51.34313381, 4.27204168 51.34274328, 4.27253437 51.34247243, 4.27294171 51.34230161, 4.27320147 51.34208214, 4.27339447 51.34174252, 4.27364326 51.34137702, 4.27374482 51.33972704, 4.27402914 51.33933532, 4.27425265 51.33892775, 4.27446866 51.33858895, 4.27466142 51.33835101, 4.27491415 51.33821177, 4.27522004 51.33809876, 4.27542496 51.33789313, 4.27541244 51.33760095, 4.27517855 51.33739305, 4.27485883 51.33729255, 4.27470422 51.33721316, 4.27489185 51.33709455, 4.27540374 51.33684993, 4.27602828 51.33639824, 4.27645135 51.33590257, 4.2767477 51.3355031, 4.27703273 51.33519077, 4.27742624 51.33495975, 4.27781487 51.33482862, 4.27814567 51.33464515, 4.27855885 51.33421397, 4.27912676 51.33344913, 4.27952456 51.33256733, 4.27978921 51.33187187, 4.28017294 51.33203125, 4.28128958 51.33133376, 4.28128958 51.32882237, 4.28173625 51.32854354, 4.28173625 51.328125, 4.28194225 51.328125, 4.28206944 51.32676983, 4.28232372 51.32657051, 4.28245068 51.32633126, 4.28270507 51.32613206, 4.28283215 51.32045186, 4.28238726 51.32017302, 4.28245676 51.31258905, 4.28302884 51.31234825, 4.28347027 51.31216598, 4.28369582 51.31194925, 4.28367555 51.3116796, 4.28344142 51.31148088, 4.28311515 51.31128132, 4.28289211 51.31086075, 4.28288627 51.31023204, 4.28309417 51.30982625, 4.28340685 51.30965197, 4.28367567 51.30945551, 4.28387821 51.3090589, 4.2840327 51.3084408, 4.28401005 51.3076539, 4.28386176 51.30701995, 4.28384626 51.30654991, 4.28404784 51.30618584, 4.28437781 51.30582964, 4.28479624 51.30545294, 4.2851088 51.30516803, 4.28538215 51.30501366, 4.28566706 51.30490768, 4.28588045 51.30470395, 4.28600347 51.30435967, 4.286255 51.30385196, 4.28646374 51.3033452, 4.28692353 51.3030498, 4.28768742 51.30290902, 4.28846085 51.30278766, 4.28917432 51.30266035, 4.28979492 51.30248928, 4.29025185 51.30246162, 4.29183066 51.30220413, 4.29505706 51.30151737, 4.29744792 51.30108607, 4.29855466 51.30074072, 4.29963505 51.30048072, 4.30052555 51.30026805, 4.30148912 51.29999864, 4.30268276 51.29959095, 4.30395794 51.29917884, 4.30504417 51.29876721, 4.30575967 51.29849672, 4.30627167 51.29830325, 4.30679703 51.29810917, 4.30739582 51.29786336, 4.30793941 51.29766715, 4.30843842 51.29748905, 4.30899107 51.29728782, 4.30964255 51.29701078, 4.31030202 51.29673552, 4.31095421 51.29645264, 4.31153214 51.29624808, 4.31211853 51.29609716, 4.31280005 51.29600072, 4.31418955 51.2952373, 4.31462145 51.29514062, 4.3150959 51.29499125, 4.31569386 51.29479384, 4.31645572 51.29452276, 4.31720913 51.29424703, 4.31773508 51.29390454, 4.31804025 51.29355025, 4.31848848 51.29327714, 4.31936705 51.29296517, 4.32212484 51.29024315, 4.32233834 51.28953183, 4.32261205 51.28912735, 4.32308745 51.28884947, 4.32371461 51.28857756, 4.32435584 51.2881968, 4.32476544 51.28780746, 4.32501864 51.28743064, 4.32518864 51.28708196, 4.32540727 51.28686893, 4.32570517 51.28676856, 4.3260082 51.28669024, 4.32604635 51.28586447, 4.32632065 51.2855854, 4.32654095 51.28528106, 4.32678545 51.28491771, 4.32707763 51.28450966, 4.32748365 51.28419173, 4.3278774 51.28401291, 4.32820678 51.28381217, 4.3286159 51.28344381, 4.32918108 51.28294814, 4.32958865 51.28257763, 4.32988727 51.28240204, 4.33017695 51.28232837, 4.33042312 51.28220475, 4.33062994 51.28192413, 4.33079243 51.28142846, 4.33070648 51.28077924, 4.33030617 51.28028917, 4.33008444 51.28016555, 4.33023596 51.27978885, 4.33142006 51.27890551, 4.33191693 51.27843702, 4.33137655 51.27813661, 4.33076596 51.27853215, 4.32993603 51.27898145, 4.32886684 51.27934551, 4.32747483 51.28001416, 4.32648444 51.28055871, 4.32600415 51.28061485, 4.32759833 51.27840996, 4.32821345 51.27793491, 4.32877946 51.27749777, 4.32960582 51.27685976, 4.32903564 51.27639055, 4.32788432 51.27695, 4.32738483 51.27714145, 4.32622385 51.27770031, 4.32561386 51.27798641, 4.32471347 51.2784183, 4.32411325 51.27759206, 4.32357311 51.27676594, 4.32300281 51.27625895, 4.32267272 51.27565825, 4.32195234 51.27547038, 4.32138205 51.27532017, 4.32131732 51.27492428, 4.32094324 51.2748239, 4.32068634 51.27465236, 4.32041275 51.27432716, 4.32001305 51.27385116, 4.31961405 51.27335811, 4.31915081 51.27299714, 4.31866527 51.27279961, 4.31818235 51.27264643, 4.31770694 51.27247024, 4.31729901 51.27221251, 4.31703961 51.27196264, 4.31678176 51.27179277, 4.31642187 51.27167511, 4.31600416 51.27155185, 4.31560993 51.27145112, 4.3152765 51.27135026, 4.31524766 51.27044916, 4.31470954 51.27022207, 4.31429362 51.27006757, 4.31401455 51.26988852, 4.31374025 51.26959205, 4.31334865 51.26920092, 4.31294715 51.26883328, 4.31243134 51.26851702, 4.31180513 51.26821601, 4.31117225 51.26782537, 4.31076765 51.26743627, 4.31051266 51.26706302, 4.31033206 51.26671863, 4.31009448 51.26650727, 4.30979586 51.26639414, 4.30959046 51.26626706, 4.3095957 51.26610875, 4.30897236 51.26360166, 4.30875874 51.26317155, 4.30855584 51.26273835, 4.30834055 51.26234031, 4.30812085 51.26200962, 4.30776882 51.26181555, 4.30728173 51.26161814, 4.30689311 51.26113164, 4.30663395 51.26032126, 4.30650628 51.25964391, 4.30677867 51.25914431, 4.3075577 51.25877261, 4.30843043 51.25870264, 4.30897355 51.25899351, 4.30937254 51.25952494, 4.31001663 51.25992, 4.31086648 51.25990963, 4.31151605 51.25946975, 4.31185055 51.25884974, 4.31212318 51.25843966, 4.31249726 51.25820565, 4.31289101 51.25796294, 4.31314814 51.25771976, 4.31333947 51.25750935, 4.313501 51.25750387, 4.31496406 51.25722885, 4.31512177 51.25732601, 4.32374036 51.25542641, 4.32730567 51.25452506, 4.33045971 51.25375271, 4.33197474 51.25339675, 4.33256686 51.25319696, 4.33318377 51.25295031, 4.33375001 51.25275826, 4.33437288 51.25257015, 4.33521485 51.25230432, 4.33623004 51.25187945, 4.33727705 51.25140238, 4.33836043 51.25086606, 4.33916664 51.25045335, 4.33967245 51.2500633, 4.33994865 51.2496556, 4.34016383 51.24930263, 4.34034658 51.24900317, 4.34053934 51.24873793, 4.34071434 51.24851584, 4.34099126 51.24834526, 4.34147084 51.24816167, 4.34209025 51.24791348, 4.34265113 51.24771237, 4.34312904 51.24754882, 4.34359896 51.2474184, 4.34414446 51.24727857, 4.34605801 51.24636257, 4.34632301 51.24613285, 4.34681225 51.24593413, 4.34746075 51.24567616, 4.34805822 51.24542272, 4.34850562 51.24513996, 4.34876585 51.24489045, 4.34902012 51.24472666, 4.34938204 51.24461496, 4.34979832 51.24449384, 4.35015762 51.24437988, 4.35038495 51.24423146, 4.35056472 51.24405086, 4.35090125 51.24390233, 4.35144174 51.24378526, 4.35197926 51.24365485, 4.35245037 51.24352932, 4.35291922 51.24337423, 4.35350454 51.24317801, 4.35429215 51.24289477, 4.35517406 51.2425729, 4.35594201 51.24216056, 4.35644507 51.24174964, 4.35664117 51.24164426, 4.35695922 51.2416743, 4.35734057 51.24179375, 4.35765851 51.24168432, 4.36265182 51.24154794, 4.36383224 51.24120176, 4.36505771 51.24097514, 4.36617517 51.24084616, 4.36727321 51.24085963, 4.36819613 51.24099767, 4.37828815 51.24107134, 4.37917805 51.24051344, 4.38613033 51.24048555, 4.38724565 51.24030626, 4.38852978 51.24015892, 4.38962805 51.24000394, 4.39053178 51.23986244, 4.39116454 51.23974705, 4.39167655 51.23975396, 4.39231741 51.23989046, 4.39321661 51.24005771, 4.39424741 51.24018335, 4.39534616 51.24009752, 4.39610481 51.23970866, 4.39645195 51.23916185, 4.39664495 51.23872805, 4.39685881 51.23846126, 4.39733565 51.23830307, 4.39806354 51.23813343, 4.39809978 51.23812485, 4.398736 51.23780704, 4.39907742 51.23744953, 4.39932382 51.23723078, 4.39958882 51.23713291, 4.39981425 51.23701215, 4.40000093 51.23675108, 4.40020287 51.2362591, 4.40035856 51.23551321, 4.40114391 51.23465598, 4.40128243 51.23417115, 4.40152085 51.23388934, 4.40182698 51.23370421, 4.40203714 51.23337746, 4.40203881 51.23289776, 4.40183353 51.23257947, 4.40153313 51.23233533, 4.40128756 51.23178446, 4.40109491 51.23077536, 4.40084696 51.22969973, 4.40052855 51.22863746, 4.40012622 51.22789991, 4.39982235 51.22753942, 4.39955437 51.22741175, 4.39927173 51.22734153, 4.39904284 51.22715545, 4.39885771 51.22678876, 4.39864171 51.22630656, 4.39843035 51.22573173, 4.39819515 51.22516441, 4.39797962 51.22459722, 4.39776027 51.22412121, 4.39759123 51.22372866, 4.39742935 51.22343016, 4.39697266 51.22352088, 4.39683652 51.22323215, 4.3966856 51.22292864, 4.39642167 51.22271347, 4.39605176 51.22253346, 4.39576805 51.22225595, 4.39573336 51.22188652, 4.39570582 51.22147143, 4.39555717 51.22112346, 4.39537334 51.22088516, 4.39512706 51.2207576, 4.39483452 51.22066712, 4.39459586 51.2204653, 4.39440656 51.22006631, 4.39419007 51.21950686, 4.39397967 51.21884894, 4.39374721 51.21834755, 4.39353597 51.21800721, 4.39331925 51.21774685, 4.39313388 51.21754921, 4.39290845 51.21742856, 4.39263427 51.2173115, 4.39237642 51.21702135, 4.39205635 51.21648002, 4.39155054 51.21584845, 4.39134216 51.21408594, 4.39079511 51.21385348, 4.39038014 51.21369958, 4.39014232 51.21354437, 4.38998783 51.21331775, 4.3897475 51.2130146, 4.38931847 51.21263754, 4.38858724 51.21224511, 4.38780391 51.21198833, 4.38718355 51.21182275, 4.38669455 51.21171105, 4.38627052 51.21159434, 4.38592172 51.21144998, 4.38568914 51.21115911, 4.38551247 51.21069634, 4.38527083 51.21025503, 4.38495254 51.20985246, 4.38455045 51.20946264, 4.38425541 51.20917976, 4.38401008 51.20903528, 4.38374758 51.20895004, 4.38345885 51.20876861, 4.38297915 51.20839024, 4.38217533 51.20788586, 4.38135636 51.20748711, 4.38001883 51.20680261, 4.37917924 51.20659888, 4.37843823 51.20634556, 4.37784266 51.20614696, 4.37723994 51.20597172, 4.37647927 51.20578277, 4.37563181 51.20552754, 4.37485445 51.20525324, 4.37425327 51.20485985, 4.3736974 51.20435393, 4.372877 51.20389116, 4.37192595 51.20361912, 4.37108958 51.20343852, 4.37024581 51.20325375, 4.36928225 51.20299506, 4.36819172 51.20273185, 4.36707115 51.20242262, 4.36589885 51.20208287, 4.36470461 51.20165563, 4.36338365 51.2012434, 4.36159778 51.20081365, 4.35945392 51.20047534, 4.35777533 51.20018637, 4.3566277 51.19997931, 4.35583544 51.19978356, 4.35517704 51.19974732, 4.35458136 51.19956934, 4.35396612 51.19942641, 4.3534224 51.19928205, 4.35285783 51.1991502, 4.35207677 51.19900656, 4.35108471 51.19887543, 4.35031283 51.19873512, 4.34978962 51.19860315, 4.34933865 51.19843876, 4.34879506 51.19823337, 4.34807146 51.19797087, 4.34723628 51.19776464, 4.34630597 51.19760001, 4.3452785 51.19747412, 4.34451103 51.19735324, 4.34398651 51.19725013, 4.34351826 51.19708908, 4.34297216 51.19675207, 4.34235644 51.19606781, 4.34179986 51.19521606, 4.34110832 51.19464385, 4.3403486 51.19438457, 4.33987534 51.19417143, 4.33965445 51.19385624, 4.33907294 51.19344485, 4.33875155 51.19330728, 4.33851397 51.19301605, 4.33833361 51.19255424, 4.33812821 51.192137, 4.33792734 51.19180775, 4.33767986 51.19151425, 4.33737505 51.19122601, 4.33694768 51.19097412, 4.3365401 51.19081271, 4.33626926 51.19065297, 4.33611441 51.19043052, 4.33601928 51.19016612, 4.33599985 51.18989265, 4.33588195 51.18961585, 4.33569682 51.18929946, 4.33546054 51.18887484, 4.33524311 51.18833637, 4.33502281 51.18785095, 4.33483315 51.18747544, 4.33459842 51.18727183, 4.33431554 51.18715417, 4.33407974 51.18695331, 4.33387744 51.18661392, 4.33361554 51.18624437, 4.33343685 51.18574607, 4.33318257 51.18530774, 4.33305538 51.18411195, 4.33280122 51.1836735, 4.33268714 51.17926276, 4.33239424 51.17921412, 4.33209431 51.17914951, 4.33182764 51.17893326, 4.33152211 51.17839384, 4.33110452 51.17744064, 4.33076692 51.17619765, 4.33054984 51.17519355, 4.33055973 51.17441714, 4.33077335 51.17383957, 4.33089387 51.16922426, 4.331339 51.16894531, 4.331339 51.16420186, 4.33178401 51.16392303, 4.33178401 51.15234375, 4.33179975 51.15234375, 4.33167303 51.15092874, 4.33141971 51.15060985, 4.33129311 51.14989233, 4.33103967 51.14957345, 4.33091295 51.14578676, 4.33046973 51.14550781, 4.33048141 51.14160931, 4.33017814 51.14153683, 4.32987487 51.14142382, 4.32964134 51.14114296, 4.32945716 51.14065266, 4.32924342 51.14010775, 4.32903421 51.13953245, 4.32880247 51.13897252, 4.32859266 51.13832831, 4.32837653 51.13757825, 4.32818782 51.13677204, 4.32795215 51.13630474, 4.32766724 51.13602626, 4.32743156 51.13548291, 4.32724285 51.13453007, 4.32702684 51.13363302, 4.32681716 51.13287151, 4.32658577 51.13231325, 4.32637632 51.13184094, 4.32615995 51.13137591, 4.32596934 51.13092041, 4.32572925 51.13064945, 4.32544148 51.13050795, 4.32522047 51.13030577, 4.32509184 51.1299715, 4.32475746 51.12962925, 4.32461607 51.12931502, 4.32440281 51.12895274, 4.32411897 51.12853837, 4.32365823 51.12817895, 4.32303894 51.12790716, 4.32228625 51.12764764, 4.32155371 51.12745786, 4.3209511 51.12728596, 4.32034862 51.12708712, 4.31959045 51.12681055, 4.31871676 51.12653601, 4.31784225 51.12625492, 4.31709051 51.12605107, 4.31652904 51.12589133, 4.31602502 51.12576365, 4.31537795 51.12563086, 4.31450784 51.12551034, 4.31359315 51.12536228, 4.31268382 51.12515914, 4.31203532 51.12481725, 4.31169987 51.12440002, 4.3115257 51.12411606, 4.31147623 51.12403572, 4.31127715 51.12373281, 4.31101584 51.12345481, 4.31097722 51.12320077, 4.31067204 51.12313795, 4.31036186 51.12301552, 4.31011248 51.12266326, 4.30991602 51.12200356, 4.30974805 51.12122393, 4.30974853 51.12040567, 4.30991852 51.11984837, 4.31011963 51.11955917, 4.31037104 51.11943531, 4.31066275 51.11936533, 4.31089926 51.1192348, 4.31108725 51.11901164, 4.3113029 51.11873925, 4.31151187 51.11845565, 4.31174254 51.11816883, 4.31195176 51.11786187, 4.31217062 51.11751533, 4.31237006 51.11716902, 4.31262541 51.11695111, 4.31291211 51.11684406, 4.31304824 51.11675525, 4.31292021 51.11666787, 4.3126173 51.11659181, 4.31228864 51.11650264, 4.31264782 51.11634111, 4.31291366 51.11632252, 4.31309116 51.11624813, 4.31318915 51.11341262, 4.31288767 51.11333311, 4.31259131 51.11325264, 4.31237245 51.11311054, 4.31219935 51.11287785, 4.31193972 51.112571, 4.31152916 51.11220074, 4.31092167 51.11182475, 4.31040931 51.11157084, 4.31011224 51.11132312, 4.30993128 51.1109904, 4.30973065 51.11065865, 4.30952287 51.11034524, 4.30927634 51.11000538, 4.30904472 51.10961533, 4.30885077 51.10920942, 4.30884802 51.10871828, 4.30906451 51.10806084, 4.30939996 51.10722184, 4.30981767 51.10655046, 4.31012845 51.10618114, 4.31040025 51.10603035, 4.31068385 51.10595822, 4.31089628 51.10583818, 4.31101894 51.10565042, 4.31173396 51.10519254, 4.31345356 51.10508823, 4.31509125 51.1050117, 4.31681967 51.105093, 4.31806076 51.10529995, 4.31880367 51.10538042, 4.31949008 51.10527492, 4.32041621 51.10507011, 4.32144237 51.10480833, 4.32240593 51.10460377, 4.32318985 51.10438466, 4.32405305 51.10404623, 4.32515764 51.10355234, 4.32611847 51.10316014, 4.32673061 51.10283268, 4.32710958 51.10252488, 4.32761312 51.10220206, 4.32840455 51.10181618, 4.32942855 51.10131657, 4.33024776 51.10092676, 4.33080328 51.10058105, 4.33119762 51.10021532, 4.33158493 51.09982777, 4.33184814 51.09951484, 4.33204603 51.09928262, 4.33289182 51.09933674, 4.33301592 51.09914613, 4.33321023 51.09895742, 4.33367026 51.09881723, 4.33441532 51.09871066, 4.33510172 51.09856915, 4.33564162 51.09836531, 4.33604205 51.09803331, 4.33628333 51.09766734, 4.33655918 51.09740853, 4.33702517 51.09720111, 4.33763814 51.09695601, 4.33819926 51.09676278, 4.33867228 51.09660625, 4.33910811 51.0964787, 4.33956778 51.09632266, 4.34011781 51.09612107, 4.34077454 51.09584165, 4.341434 51.09557176, 4.34207034 51.09530485, 4.34260845 51.09511018, 4.34315276 51.09489918, 4.34391224 51.09456551, 4.34492707 51.0940696, 4.34586763 51.09365571, 4.34660935 51.09322107, 4.34707606 51.09311891, 4.34763312 51.09298241, 4.34836352 51.09285176, 4.34925008 51.09271896, 4.35019863 51.09260595, 4.35077226 51.09245145, 4.35102832 51.09221721, 4.35123682 51.09193051, 4.35150075 51.09163284, 4.35189652 51.09138131, 4.35228836 51.09121931, 4.35261846 51.09101546, 4.35302746 51.09064531, 4.35359132 51.09016025, 4.35399532 51.08981442, 4.35428464 51.08964741, 4.35455906 51.08946502, 4.35480297 51.08895958, 4.35508215 51.08808422, 4.3554759 51.08733416, 4.35586333 51.08686912, 4.35635698 51.08656442, 4.35700917 51.08628535, 4.35783994 51.08589077, 4.35790324 51.08548415, 4.35882998 51.08519924, 4.35979331 51.0849508, 4.36060405 51.08469355, 4.361292 51.08449864, 4.36224794 51.08430004, 4.36375284 51.08402705, 4.36544764 51.08365333, 4.36699271 51.08337915, 4.36803985 51.08319688, 4.36880732 51.08306932, 4.36953628 51.08293474, 4.37035525 51.08279657, 4.37110734 51.08262026, 4.37618434 51.08255851, 4.37710655 51.08238018, 4.37826657 51.08224022, 4.37959182 51.08210623, 4.38112044 51.08199191, 4.3824079 51.08184648, 4.38345838 51.08161604, 4.38407195 51.0811789, 4.38395035 51.08061481, 4.38380635 51.08055997, 4.38290143 51.08021617, 4.38114512 51.08012402, 4.37941766 51.08023953, 4.37773085 51.08039653, 4.37631154 51.08055675, 4.37505746 51.08069551, 4.37419271 51.08086002, 4.37345004 51.08106387, 4.3724122 51.08132505, 4.37110126 51.08152986, 4.3699789 51.0816946, 4.3690145 51.08182752, 4.36830175 51.08194745, 4.36816776 51.08196998, 4.36726153 51.08210266, 4.36625612 51.08223832, 4.36500716 51.08233953, 4.36413074 51.08256197, 4.36297226 51.08265567, 4.36220145 51.08278191, 4.36171353 51.0829128, 4.36124241 51.08309913, 4.36053896 51.08337724, 4.35950947 51.08375645, 4.35847056 51.0840311, 4.35773921 51.08420694, 4.35724032 51.08432233, 4.35681224 51.08445561, 4.35637164 51.08465421, 4.35591066 51.08502412, 4.35542083 51.08553052, 4.35478246 51.08601296, 4.35419381 51.08634281, 4.35375571 51.0866158, 4.35343301 51.08683407, 4.35290885 51.08700991, 4.35203946 51.08728266, 4.35121155 51.08780074, 4.35073555 51.08840466, 4.35047674 51.08885324, 4.35033536 51.08913755, 4.35020602 51.0893321, 4.34974468 51.08924985, 4.34960091 51.08945048, 4.34945405 51.08971417, 4.34923744 51.09006715, 4.34891713 51.09048545, 4.34834588 51.09084833, 4.34758413 51.09111905, 4.34683836 51.09137475, 4.34621835 51.09156525, 4.34556508 51.09176147, 4.34470987 51.09203148, 4.34380245 51.09239554, 4.34319842 51.09265256, 4.3429383 51.09283435, 4.34283471 51.0930078, 4.34261096 51.09316647, 4.34205401 51.09335232, 4.34108746 51.0936017, 4.33995247 51.09379578, 4.33913815 51.09394455, 4.33868074 51.09407461, 4.33845055 51.09429383, 4.33829725 51.0946362, 4.33811665 51.0949831, 4.33797073 51.09530222, 4.33726525 51.09566784, 4.33656585 51.09576344, 4.33598173 51.09590948, 4.33548975 51.09610057, 4.33509564 51.09636807, 4.33485532 51.0966202, 4.33460891 51.09678876, 4.33422101 51.09690642, 4.33367407 51.097036, 4.33303785 51.09715545, 4.33250475 51.09727704, 4.33212125 51.09739113, 4.33180356 51.09759772, 4.33132076 51.09798455, 4.33056247 51.09846401, 4.32995081 51.09877241, 4.32959914 51.09899378, 4.32931626 51.09928834, 4.3289299 51.09967351, 4.32853937 51.10003531, 4.32807493 51.10032475, 4.32758284 51.10054934, 4.32710624 51.10084116, 4.32661891 51.10124421, 4.32595408 51.10176563, 4.32528853 51.10192215, 4.32461822 51.10224593, 4.32406914 51.10245967, 4.32357574 51.10263824, 4.32299745 51.10284054, 4.32226741 51.10310495, 4.32153535 51.10331202, 4.32095361 51.10343754, 4.32046425 51.10342753, 4.31994724 51.10333502, 4.31935287 51.10332453, 4.31871772 51.10342884, 4.31786311 51.10355985, 4.31654334 51.10370171, 4.31475854 51.10383832, 4.31337655 51.10404122, 4.31332564 51.10404873, 4.31215155 51.10439301, 4.31105983 51.10484338, 4.31027782 51.10507023, 4.30973637 51.10513854, 4.30932665 51.10531235, 4.30905843 51.10559726, 4.30884182 51.10594904, 4.30868495 51.10632253, 4.30853272 51.10663545, 4.30807495 51.10655475, 4.30791342 51.10674036, 4.30774665 51.10698092, 4.30753541 51.10731685, 4.3073343 51.10780191, 4.30710375 51.10846424, 4.30683076 51.10925722, 4.30640352 51.10977185, 4.30582345 51.10988176, 4.30502677 51.10977781, 4.30410457 51.10973215, 4.30353522 51.10984266, 4.30328333 51.11004555, 4.30306447 51.11019444, 4.30279565 51.11016834, 4.30261636 51.10989237, 4.30264223 51.10941672, 4.30284142 51.10898507, 4.30306661 51.10865045, 4.30329812 51.10837591, 4.30349445 51.10815847, 4.30373502 51.10802138, 4.30402195 51.10792911, 4.30424225 51.1077944, 4.30437076 51.10759926, 4.30470955 51.10728395, 4.30487192 51.10700214, 4.30508935 51.10671735, 4.30530012 51.10643113, 4.30551755 51.10615826, 4.30570054 51.10593832, 4.30592155 51.10580826, 4.30620062 51.10572004, 4.30649805 51.10553682, 4.30690885 51.1051619, 4.30748045 51.10465956, 4.30789161 51.10428464, 4.30818903 51.1041013, 4.30846822 51.10401297, 4.30868888 51.10388315, 4.30887008 51.10366344, 4.30908465 51.10339057, 4.30929506 51.10310292, 4.30952728 51.10281372, 4.30973685 51.10252583, 4.30995071 51.10224998, 4.31013465 51.10202003, 4.31036818 51.10187376, 4.31067145 51.10179031, 4.31097484 51.10170925, 4.31118131 51.10130143, 4.31187797 51.10074925, 4.31227684 51.10038614, 4.31263852 51.10015571, 4.31334257 51.0998466, 4.31448126 51.09934282, 4.31567538 51.09886324, 4.31673265 51.09840775, 4.31753886 51.09806514, 4.31819916 51.09777856, 4.31872094 51.09758532, 4.31916654 51.09743202, 4.31962538 51.09730113, 4.32017875 51.09715092, 4.32084143 51.09700501, 4.3214848 51.09687352, 4.32203257 51.09685504, 4.32238114 51.09699523, 4.32274234 51.09726095, 4.32346737 51.09755135, 4.32450235 51.09775627, 4.32543385 51.09786522, 4.32629144 51.09779155, 4.32715201 51.09755743, 4.32735777 51.09750593, 4.32802415 51.09733915, 4.32943034 51.09657824, 4.33011556 51.09647787, 4.33070695 51.09632242, 4.33128226 51.09611833, 4.33189344 51.09585965, 4.33243334 51.09565795, 4.33292627 51.09547877, 4.33347523 51.09527802, 4.33412743 51.09500146, 4.33479643 51.0947268, 4.3354634 51.09444571, 4.33602774 51.09424472, 4.33648455 51.09409428, 4.33683264 51.09398174, 4.3370465 51.09384358, 4.33720243 51.09367383, 4.33746874 51.09352124, 4.33788574 51.09334898, 4.33826125 51.09307325, 4.33828628 51.09279036, 4.33829021 51.09274805, 4.33793688 51.09244966, 4.33739114 51.09224915, 4.33667552 51.09213436, 4.33577025 51.09218311, 4.33486795 51.09239495, 4.3350594 51.09270966, 4.33460057 51.09290814, 4.33385623 51.09323537, 4.33277512 51.09371936, 4.33173907 51.09406257, 4.33078992 51.09426117, 4.32976496 51.09438562, 4.32877433 51.09453106, 4.32755184 51.09472454, 4.32590103 51.0949949, 4.32398784 51.09525657, 4.32278061 51.09554172, 4.32230568 51.09579074, 4.32198584 51.09595323, 4.32148612 51.09606516, 4.32087374 51.09619081, 4.32019222 51.09631622, 4.31948876 51.09647536, 4.31868446 51.09667444, 4.31782401 51.09694195, 4.31704521 51.09720135, 4.3164798 51.09750485, 4.31606805 51.09782314, 4.31552756 51.09809864, 4.31482673 51.09828043, 4.31300175 51.09923017, 4.31219757 51.09962153, 4.31141198 51.10011601, 4.31093657 51.10049176, 4.31057906 51.10071683, 4.31008673 51.10094976, 4.30950224 51.10131824, 4.30910075 51.10168755, 4.30883992 51.10199404, 4.30866122 51.1022222, 4.30843973 51.10235775, 4.30816102 51.1024456, 4.30792618 51.10258424, 4.30773675 51.10280931, 4.30751955 51.10308373, 4.30730915 51.10337186, 4.30707896 51.10366011, 4.30687666 51.10394311, 4.30667567 51.10420728, 4.30649435 51.10442793, 4.30620027 51.10460544, 4.30569506 51.10483718, 4.30509245 51.10520875, 4.30465651 51.10557985, 4.30429327 51.10591161, 4.30432475 51.10617352, 4.30402148 51.10625446, 4.30371833 51.10633802, 4.30348468 51.10648417, 4.30330074 51.10671425, 4.30308676 51.10698998, 4.30287755 51.10727775, 4.30264604 51.10756648, 4.30243611 51.10785413, 4.30221975 51.10812795, 4.30203116 51.10835314, 4.3017956 51.10849285, 4.30151057 51.10858345, 4.30127502 51.10872078, 4.30108607 51.10894036, 4.30086982 51.10921025, 4.30065978 51.10951316, 4.30042923 51.10987175, 4.30022323 51.11029255, 4.30001438 51.11069715, 4.29982901 51.11103833, 4.29956305 51.11124885, 4.29916763 51.11146677, 4.29875314 51.11193943, 4.29845667 51.11269105, 4.29943383 51.11321497, 4.30024171 51.11330891, 4.30071282 51.11352575, 4.30093193 51.11391735, 4.30110145 51.11440551, 4.30125773 51.11491024, 4.30150938 51.11523736, 4.30191195 51.11538684, 4.30238783 51.11536706, 4.30244267 51.11531734, 4.30276144 51.11502933, 4.30282533 51.11439204, 4.30269694 51.11356461, 4.30272615 51.11263716, 4.30304301 51.11209703, 4.30353045 51.11205494, 4.3042649 51.11226737, 4.30547476 51.11253285, 4.30703115 51.11261332, 4.30828786 51.11252725, 4.30917311 51.11247563, 4.30962145 51.11257422, 4.30982065 51.1128726, 4.30999005 51.11341918, 4.31013227 51.11415994, 4.31028414 51.11479294, 4.31081402 51.11462665, 4.31091845 51.11502826, 4.31089294 51.11545825, 4.31068957 51.11572671, 4.31038666 51.11585367, 4.31012487 51.11599231, 4.30992866 51.11621428, 4.30977654 51.11649036, 4.30978048 51.11676598, 4.3098613 51.11698556, 4.30976677 51.11711812, 4.30951226 51.11723971, 4.30926776 51.11752236, 4.30907238 51.11802661, 4.30886722 51.11858034, 4.30868244 51.11908495, 4.30844831 51.11936808, 4.30816555 51.11949027, 4.30793035 51.11962152, 4.30772853 51.11983371, 4.30746782 51.12009394, 4.30749714 51.12273967, 4.30749965 51.12296486, 4.30710602 51.12295032, 4.30657506 51.12285244, 4.30594754 51.1226896, 4.30550051 51.12242413, 4.30535901 51.12207043, 4.30531466 51.12156868, 4.30539942 51.12090874, 4.30551934 51.12029874, 4.30531788 51.11974263, 4.3046447 51.1193279, 4.30378771 51.11923981, 4.30295014 51.1194123, 4.30289435 51.12208891, 4.30271697 51.1221261, 4.30245113 51.12199593, 4.29212153 51.12193084, 4.29167807 51.12220967, 4.28782237 51.12218177, 4.28706264 51.12200224, 4.2860949 51.12185454, 4.28489435 51.12170064, 4.28344941 51.12156475, 4.28224432 51.12145937, 4.28114724 51.12146831, 4.27994215 51.12155664, 4.27852714 51.12156487, 4.27743578 51.12145793, 4.27664816 51.12131894, 4.27600706 51.12116516, 4.27536654 51.12103331, 4.2747426 51.12090206, 4.2741096 51.12077904, 4.27347481 51.12058604, 4.27266502 51.12024713, 4.27161813 51.11972725, 4.27069437 51.11925685, 4.26999021 51.11880708, 4.26960075 51.11843443, 4.26937211 51.11813903, 4.26921356 51.11791277, 4.26899445 51.11777532, 4.26868308 51.11770618, 4.26687872 51.11667764, 4.26628697 51.11648166, 4.26568127 51.11623871, 4.26515567 51.11604238, 4.26460588 51.11582291, 4.26378071 51.11548245, 4.26257801 51.11499584, 4.26140022 51.11463416, 4.26046944 51.11436045, 4.25399113 51.11436772, 4.25355852 51.11418963, 4.25313127 51.11404705, 4.25269794 51.11390388, 4.25220215 51.11377454, 4.2515434 51.11363161, 4.25071323 51.11348891, 4.24993467 51.11331081, 4.24346685 51.1132884, 4.24280906 51.11349905, 4.24196124 51.11371601, 4.24089134 51.11397493, 4.2397691 51.11417186, 4.23897982 51.11432505, 4.23843253 51.11445224, 4.23783934 51.11463118, 4.23701954 51.11490548, 4.23611057 51.11528385, 4.23538995 51.11556077, 4.23478222 51.11574447, 4.23414516 51.1158725, 4.23349714 51.11600316, 4.23280716 51.11612773, 4.2321192 51.11628175, 4.23143971 51.11647201, 4.23092914 51.11673725, 4.2306565 51.11698806, 4.23041415 51.11715603, 4.23003805 51.11727345, 4.22950256 51.11740661, 4.22886825 51.11753654, 4.22829986 51.11767244, 4.22780073 51.11777461, 4.22739065 51.11799622, 4.22696686 51.11809361, 4.22654641 51.11822474, 4.22609937 51.11835766, 4.22552896 51.11852455, 4.22471035 51.11873054, 4.22363865 51.11899161, 4.22244275 51.11919475, 4.22141635 51.11935806, 4.22050357 51.11949873, 4.21974683 51.11967003, 4.21875 51.11969864, 4.21667516 51.11968148, 4.21509361 51.1196686, 4.21434534 51.119506, 4.21343756 51.11937451, 4.21239162 51.11920536, 4.21111202 51.11893964, 4.20988476 51.11851335, 4.20888591 51.11803961, 4.20806384 51.1175065, 4.20760441 51.11712468, 4.20729435 51.11692774, 4.20692182 51.1167804, 4.20652247 51.11653876, 4.20622754 51.11620724, 4.2059828 51.11576831, 4.2057873 51.11524796, 4.20559323 51.11484075, 4.2054162 51.11458242, 4.20512795 51.11440945, 4.20463085 51.11418545, 4.20402348 51.11381245, 4.20352745 51.11343384, 4.20300353 51.11310816, 4.20239747 51.11280405, 4.20177376 51.11241102, 4.20132184 51.11201131, 4.20074022 51.11164713, 4.20042574 51.11157775, 4.20019615 51.11143815, 4.20001543 51.11120725, 4.19977248 51.11090636, 4.19945908 51.11054385, 4.19906604 51.11015367, 4.19876993 51.10986066, 4.19847894 51.10968697, 4.19808567 51.10952735, 4.19764245 51.10925245, 4.19724214 51.10888481, 4.19684052 51.10848987, 4.19655406 51.10819292, 4.19624221 51.10799623, 4.19575715 51.10776842, 4.19517398 51.10740185, 4.19470167 51.10702646, 4.19415295 51.10667455, 4.19336116 51.10627866, 4.19229865 51.10574555, 4.19126546 51.10526836, 4.19025505 51.1048398, 4.18939614 51.10455811, 4.18871486 51.10433543, 4.18841243 51.10432315, 4.18781161 51.10414457, 4.18719172 51.1040014, 4.18665624 51.10385811, 4.18614817 51.10373092, 4.18550575 51.10359633, 4.18464565 51.10346818, 4.18369246 51.10328984, 4.18259084 51.10302246, 4.18155015 51.10264373, 4.18081474 51.10231686, 4.18029892 51.10197926, 4.17983115 51.10159683, 4.17920744 51.10120094, 4.17860436 51.10089767, 4.17814136 51.10060275, 4.17786515 51.1003021, 4.17762601 51.09999657, 4.17731047 51.09969866, 4.17676663 51.09948063, 4.17611563 51.09944153, 4.17561805 51.09946024, 4.17533624 51.09926665, 4.17514884 51.09882057, 4.17491102 51.09833991, 4.17487395 51.09671772, 4.17456913 51.09666026, 4.1742593 51.09648657, 4.17401075 51.09593856, 4.17381477 51.09494293, 4.17364466 51.09389496, 4.17363441 51.09277451, 4.17378247 51.09166801, 4.17397475 51.09056175, 4.17431593 51.08988595, 4.17493176 51.08946705, 4.1756891 51.08900845, 4.17618966 51.08865416, 4.17660618 51.08846307, 4.17721355 51.08835435, 4.17805576 51.08823657, 4.17912006 51.08811736, 4.18013525 51.08796382, 4.18119204 51.08776546, 4.18231046 51.08747375, 4.18353391 51.08713531, 4.18474746 51.08671641, 4.18578208 51.08632517, 4.18655622 51.08589816, 4.18702722 51.08549201, 4.18755007 51.08514202, 4.18786585 51.08506882, 4.18809545 51.08492303, 4.18827105 51.08468688, 4.18849814 51.08440232, 4.18879104 51.08411705, 4.18920445 51.08388031, 4.18959975 51.08369207, 4.18989825 51.08332646, 4.19020367 51.08259594, 4.1906141 51.08158815, 4.19094706 51.08037627, 4.19116235 51.07924461, 4.19115281 51.07813215, 4.19094002 51.07724416, 4.19077134 51.07508385, 4.19048834 51.0741682, 4.19026554 51.07307327, 4.19005156 51.07211614, 4.18986654 51.07134688, 4.18963265 51.07095945, 4.18934774 51.07076466, 4.18911135 51.07037783, 4.18892145 51.06964314, 4.18870533 51.06880903, 4.18850112 51.06791437, 4.18828726 51.06714225, 4.18810225 51.06650114, 4.18786824 51.06615818, 4.18756485 51.06603777, 4.1872617 51.06597424, 4.1872648 51.06096876, 4.18755186 51.06088865, 4.1878376 51.06078124, 4.18807745 51.06054962, 4.18837035 51.0601809, 4.18888915 51.05984235, 4.18953204 51.05963254, 4.19010127 51.05948055, 4.1906352 51.05935848, 4.19129086 51.05922151, 4.19211161 51.05908096, 4.19288313 51.05890322, 4.19677174 51.05883515, 4.19721282 51.05868042, 4.19773185 51.05857205, 4.19844127 51.05844796, 4.19930995 51.05820167, 4.19993591 51.05759346, 4.20008695 51.05659115, 4.19985604 51.05548024, 4.1994009 51.05430913, 4.19876325 51.05351424, 4.19824851 51.05318677, 4.19797194 51.05305052, 4.19781697 51.05292976, 4.19758356 51.05280197, 4.19721603 51.05258203, 4.19684541 51.05212605, 4.19659495 51.05153406, 4.19624591 51.05109882, 4.19551444 51.05071676, 4.19440198 51.05023694, 4.19340205 51.04985452, 4.19275165 51.04951966, 4.19235766 51.04920626, 4.19194663 51.04893947, 4.19149983 51.04876387, 4.19087744 51.04841125, 4.19073641 51.04812694, 4.19050825 51.04784024, 4.19017434 51.04755795, 4.18963504 51.04730761, 4.18904245 51.04708886, 4.18864608 51.04672337, 4.18841696 51.04616487, 4.18825865 51.04568815, 4.18827546 51.04528618, 4.18846488 51.04479003, 4.18868268 51.04416931, 4.18890476 51.04369974, 4.18909454 51.04343724, 4.18936205 51.04330182, 4.18976951 51.04311681, 4.19021904 51.04266131, 4.19055724 51.0419265, 4.19076097 51.04110026, 4.19063938 51.04025364, 4.19007957 51.03975916, 4.18998921 51.03976023, 4.1892544 51.03976881, 4.18850195 51.04012883, 4.18809032 51.04050446, 4.18785954 51.04079473, 4.18748951 51.04066825, 4.18732834 51.04107797, 4.18715966 51.04155743, 4.18694127 51.04195845, 4.18672657 51.04228497, 4.18649328 51.04263306, 4.18628395 51.04306364, 4.1860677 51.04353774, 4.18587327 51.04399705, 4.18562186 51.04427421, 4.1853193 51.04448342, 4.18511403 51.04489446, 4.18511415 51.04551744, 4.18531895 51.04592752, 4.1856184 51.04611385, 4.18586361 51.04631507, 4.18605661 51.04665864, 4.18630254 51.04708874, 4.18661022 51.04758155, 4.18698835 51.04803705, 4.18727088 51.04834914, 4.18759823 51.04855323, 4.18817425 51.04879475, 4.18898785 51.04915237, 4.18977284 51.04945338, 4.19055414 51.04988837, 4.19099641 51.04998755, 4.19154203 51.05018365, 4.19233584 51.05052853, 4.19332635 51.0510112, 4.19402385 51.05133545, 4.19434404 51.05154252, 4.1944977 51.05176663, 4.1947211 51.05205536, 4.19511402 51.0524168, 4.19574952 51.05279815, 4.19638157 51.05307734, 4.19688153 51.05336452, 4.19719768 51.05377865, 4.19740963 51.05448425, 4.19743621 51.05541408, 4.19723165 51.05599475, 4.19682085 51.05622256, 4.19625676 51.05634081, 4.19561195 51.05644143, 4.19504738 51.05657125, 4.19457173 51.05670106, 4.19413221 51.05684674, 4.19369745 51.05699074, 4.19326031 51.05716956, 4.19243634 51.05722916, 4.19181764 51.05740726, 4.19110668 51.05754745, 4.19033742 51.05768263, 4.18958032 51.05779993, 4.18912852 51.05794787, 4.18891883 51.05812526, 4.1887176 51.05826354, 4.18847024 51.05835986, 4.18827868 51.05848217, 4.18812335 51.05864513, 4.18780875 51.05884016, 4.18719172 51.05917835, 4.18641567 51.05974114, 4.18591452 51.06025004, 4.18560231 51.06052113, 4.18531442 51.06062436, 4.18504262 51.06069148, 4.18505681 51.06347644, 4.18461335 51.06375563, 4.18466592 51.06673324, 4.18492317 51.06753063, 4.18512022 51.06836832, 4.18535304 51.06885445, 4.18565345 51.06902206, 4.18595445 51.06908953, 4.18599105 51.07146394, 4.18627131 51.07215905, 4.18649471 51.07295835, 4.18672216 51.07364488, 4.18694556 51.07423425, 4.18722558 51.07471323, 4.18725872 51.07712507, 4.18756735 51.07719672, 4.18788123 51.07734501, 4.18812788 51.077749849999996, 4.18829787 51.0784378, 4.18839765 51.07911384, 4.1883291 51.07988524, 4.1881659 51.08080232, 4.18782282 51.08141506, 4.18718934 51.08183336, 4.18640888 51.08230186, 4.18591595 51.08265305, 4.18563485 51.08282161, 4.18540061 51.08290803, 4.18513215 51.08308697, 4.18458974 51.08346963, 4.18354785 51.08398676, 4.18236411 51.08439183, 4.18143094 51.08470297, 4.18082774 51.08489907, 4.18038607 51.08504808, 4.17991865 51.08517122, 4.17927754 51.08530915, 4.17845225 51.08545017, 4.17767394 51.08562815, 4.17623913 51.08577991, 4.17493141 51.08650148, 4.17387116 51.08707142, 4.17302322 51.08760238, 4.17256021 51.08795285, 4.17228425 51.08810544, 4.17202735 51.08821726, 4.17181706 51.08854091, 4.17164051 51.08920944, 4.17142701 51.09010315, 4.17120278 51.09117985, 4.17091846 51.09209621, 4.17092001 51.09456694, 4.17120206 51.09548175, 4.17142165 51.09655726, 4.17162812 51.09745216, 4.17180646 51.09813094, 4.17206097 51.09847963, 4.1724447 51.09861386, 4.17287564 51.09870243, 4.17321098 51.09878016, 4.17331791 51.09893262, 4.17326045 51.09925115, 4.17336035 51.09977734, 4.1736871 51.10045481, 4.17398262 51.10112047, 4.17427647 51.10150576, 4.17465436 51.10171366, 4.17508495 51.10197425, 4.17516935 51.10203862, 4.17556095 51.10233724, 4.17627501 51.10278428, 4.17719436 51.10325456, 4.17827046 51.10379362, 4.17916632 51.10420144, 4.17987835 51.10450995, 4.18042254 51.10471535, 4.18088317 51.10490465, 4.18171895 51.10494006, 4.18216145 51.10511816, 4.18263412 51.10526121, 4.18317974 51.10540545, 4.18378532 51.10553765, 4.18432987 51.10568094, 4.18489122 51.10581064, 4.18566442 51.10594714, 4.18666208 51.10607493, 4.18749344 51.10624826, 4.18815827 51.10650301, 4.18871558 51.10686874, 4.18904185 51.10717726, 4.18938315 51.10740817, 4.19006395 51.10773087, 4.19113958 51.10826492, 4.19227314 51.10881281, 4.19335544 51.10934663, 4.19404864 51.10966516, 4.19439316 51.10988224, 4.19466484 51.11016905, 4.19503653 51.11054337, 4.19534695 51.11089325, 4.19559395 51.11119592, 4.19575608 51.11147714, 4.19592416 51.11149001, 4.19641256 51.11173487, 4.19687223 51.11217153, 4.19725764 51.11277235, 4.19763565 51.11330295, 4.19790947 51.11363995, 4.19827664 51.11386681, 4.19898486 51.11417663, 4.19995952 51.11463332, 4.20066631 51.11494935, 4.201002 51.11515617, 4.20117044 51.11537993, 4.20138705 51.11566424, 4.20169103 51.11602092, 4.20209014 51.11641467, 4.20239413 51.11671305, 4.20266211 51.11687255, 4.20294416 51.11696136, 4.20317221 51.11709595, 4.20335674 51.11731827, 4.20357215 51.11759186, 4.20378232 51.11787975, 4.20401478 51.11816931, 4.20422602 51.11845708, 4.20444202 51.11873043, 4.20462275 51.11895216, 4.20483553 51.11908674, 4.20509076 51.11917806, 4.20535886 51.11934614, 4.20580614 51.11965716, 4.20664763 51.12004375, 4.20762432 51.12035727, 4.20832705 51.1206665, 4.20874476 51.12097943, 4.20919204 51.12127376, 4.20981216 51.12152994, 4.21067011 51.12179828, 4.21159732 51.12199795, 4.21230674 51.12214911, 4.21285033 51.12224674, 4.21320832 51.12248886, 4.21875 51.12248886, 4.21923864 51.12245917, 4.22033095 51.12228084, 4.22161841 51.12213755, 4.22281766 51.12199438, 4.22399771 51.12186754, 4.22507763 51.12173331, 4.2261548 51.12160385, 4.22708511 51.12141991, 4.22803748 51.12114286, 4.22905433 51.12076426, 4.22990561 51.12048936, 4.23049676 51.12031186, 4.23097098 51.12019145, 4.23151302 51.12005174, 4.23222625 51.11986005, 4.23304856 51.11954606, 4.23397231 51.1191287, 4.23502696 51.11859465, 4.23590124 51.11818004, 4.23659575 51.11786985, 4.23715305 51.11767554, 4.23773205 51.11750746, 4.23847496 51.11731386, 4.23933625 51.11706233, 4.24023676 51.11685884, 4.24066007 51.11666024, 4.24115515 51.11654854, 4.24171412 51.11640215, 4.24233365 51.11627722, 4.24287355 51.11621165, 4.24334133 51.11627746, 4.24384415 51.11640823, 4.24450493 51.11656785, 4.24530184 51.11668897, 4.24593711 51.1167469, 4.24641001 51.11671126, 4.24693024 51.11672056, 4.24778807 51.11682773, 4.24910367 51.11696315, 4.25054908 51.11711657, 4.25207782 51.11725545, 4.25326574 51.11740017, 4.25423765 51.11752713, 4.25517106 51.11765754, 4.25636208 51.11778331, 4.25788534 51.11798096, 4.25984585 51.11832046, 4.26151657 51.11881876, 4.26253426 51.11921954, 4.26322663 51.11957085, 4.26321697 51.11990786, 4.26370084 51.12032211, 4.26400662 51.12062156, 4.26430321 51.1208086, 4.2647624 51.12098277, 4.26535285 51.12121463, 4.26589203 51.12140465, 4.26640415 51.12159741, 4.26706851 51.12186623, 4.26802087 51.12227428, 4.26916361 51.12268913, 4.27050805 51.12312508, 4.27195573 51.12346852, 4.27330196 51.12375844, 4.27451146 51.12395263, 4.27540314 51.12410545, 4.27609837 51.12424135, 4.27667475 51.1244117, 4.28316772 51.12450671, 4.28388023 51.12463617, 4.28584361 51.12459636, 4.28677774 51.12430728, 4.29191554 51.12416995, 4.29323697 51.12437975, 4.29474652 51.12459612, 4.29589486 51.12485695, 4.29661918 51.12506306, 4.29728293 51.12523377, 4.29814017 51.12536657, 4.29910755 51.12546933, 4.30014396 51.12547684, 4.30095744 51.12544084, 4.30168748 51.12549675, 4.30256391 51.12561131, 4.30355024 51.12576175, 4.30467355 51.12590003, 4.3056668 51.12603855, 4.30647254 51.12615764, 4.30690014 51.12630558, 4.30707407 51.12648487, 4.30727506 51.12663317, 4.30764616 51.12675166, 4.30818808 51.12688625, 4.30882776 51.12701535, 4.30939317 51.12715983, 4.30987108 51.12730324, 4.31031334 51.12748182, 4.31123304 51.12759614, 4.31221688 51.12790477, 4.31305575 51.12809861, 4.31352484 51.12827086, 4.31384575 51.12851107, 4.3144114 51.12880731, 4.31530893 51.12912834, 4.31634247 51.12949336, 4.31715095 51.12974143, 4.31762373 51.12997162, 4.31802225 51.13033915, 4.31853735 51.13082206, 4.318946 51.13119185, 4.31942856 51.13142455, 4.3202095 51.13166785, 4.32110643 51.13200426, 4.32173085 51.13226461, 4.32203937 51.13252497, 4.32220745 51.13288856, 4.32239783 51.13330054, 4.32259047 51.13372505, 4.32280147 51.13407445, 4.32298636 51.13432205, 4.32322466 51.13446212, 4.32354534 51.1345278, 4.32377255 51.13451123, 4.32393324 51.13491595, 4.32415032 51.13554704, 4.32436573 51.13649464, 4.32460022 51.13748336, 4.32481074 51.13848603, 4.32502663 51.13924372, 4.32521474 51.13976455, 4.32544994 51.14001572, 4.32573438 51.14014685, 4.32597184 51.14040351, 4.32616723 51.14087653, 4.32639527 51.14142287, 4.32660627 51.14202344, 4.32677686 51.14262605, 4.32679176 51.14326394, 4.32673216 51.14386272, 4.32681835 51.14466393, 4.32701266 51.14578331, 4.32730317 51.14677906, 4.32736325 51.15026236, 4.32769215 51.15035236, 4.32799256 51.15043235, 4.32810974 51.15052927, 4.32795393 51.15062737, 4.32766545 51.15071166, 4.32750964 51.15080977, 4.32762694 51.1509068, 4.32792723 51.15098667, 4.32825613 51.15107667, 4.32825303 51.15234375, 4.32834077 51.15312946, 4.32854605 51.15414917, 4.32855785 51.15536726, 4.32838452 51.15640354, 4.32816875 51.15721583, 4.32789504 51.15762186, 4.3276031 51.15773642, 4.32747686 51.15777028, 4.32762587 51.15782595, 4.32793033 51.15802658, 4.32815266 51.15859687, 4.32815146 51.15947974, 4.32792771 51.1600517, 4.32762671 51.160262349999996, 4.32749343 51.16034436, 4.32764983 51.16040754, 4.32795095 51.16046965, 4.32809067 51.16055894, 4.32784355 51.16067088, 4.32723081 51.16082585, 4.32648861 51.16106975, 4.32633662 51.16780591, 4.32614875 51.1681931, 4.3261255 51.16859853, 4.32618904 51.16894996, 4.32612813 51.16923606, 4.32598543 51.16945756, 4.32572091 51.16961312, 4.32528782 51.16976798, 4.3247323 51.17000127, 4.32466376 51.17340946, 4.32510865 51.17368865, 4.32499754 51.17715645, 4.32477486 51.17739546, 4.32477486 51.17779422, 4.32499754 51.17803335, 4.32509696 51.18204105, 4.32540143 51.18211532, 4.32570565 51.18225324, 4.32594025 51.1826117, 4.32612503 51.18321085, 4.32633972 51.18376911, 4.32655001 51.18422651, 4.32678246 51.18457747, 4.32699275 51.18488908, 4.32720757 51.18522668, 4.32739234 51.18556714, 4.32762682 51.18578291, 4.32793117 51.18588626, 4.32823563 51.18596685, 4.32827306 51.18900204, 4.32854092 51.18938625, 4.32875276 51.18981135, 4.32899535 51.19024146, 4.32932603 51.19066584, 4.32986081 51.19098806, 4.33044672 51.19118381, 4.33082855 51.19140995, 4.33104682 51.19174647, 4.33125246 51.19208956, 4.33144355 51.19239676, 4.33164775 51.19266534, 4.33182704 51.19287765, 4.33207595 51.19302762, 4.33245373 51.19320965, 4.33291185 51.19356692, 4.33344531 51.19406414, 4.33421504 51.19453025, 4.33502233 51.19483685, 4.33561361 51.19512355, 4.33593261 51.19551206, 4.3361423 51.1961416, 4.33630323 51.19695103, 4.33652806 51.19746327, 4.33684421 51.1976974, 4.33857405 51.19860935, 4.33927965 51.19881594, 4.34014952 51.19907355, 4.34109914 51.19927382, 4.34190083 51.19943261, 4.34258485 51.19956231, 4.34316075 51.19972205, 4.34373033 51.19992495, 4.34433961 51.20018625, 4.34486318 51.20039165, 4.34531057 51.20055616, 4.34581435 51.20068812, 4.34658015 51.20082855, 4.34770262 51.20096052, 4.34893525 51.20110524, 4.35019934 51.20123744, 4.35107195 51.20137644, 4.35166347 51.20150113, 4.35231864 51.20165133, 4.35320795 51.20185065, 4.35414743 51.20217121, 4.35510683 51.20258784, 4.35612047 51.20309174, 4.35684085 51.20341647, 4.35729671 51.20360804, 4.35703754 51.20382786, 4.35782444 51.20390904, 4.35868478 51.20399666, 4.35924613 51.20415711, 4.35965085 51.20440912, 4.36027777 51.20467651, 4.36110234 51.20486784, 4.36178565 51.20502186, 4.36228991 51.20514643, 4.36271036 51.20527732, 4.36312592 51.20540535, 4.36362088 51.20558858, 4.36431754 51.20586324, 4.36523342 51.20623684, 4.36607015 51.20650888, 4.36670613 51.20670664, 4.36734295 51.20690286, 4.36820555 51.20716953, 4.3692317 51.20743895, 4.37014651 51.20773864, 4.37087667 51.20801425, 4.37152505 51.20829332, 4.37206197 51.20849526, 4.37252212 51.20864582, 4.37295723 51.20874214, 4.37307096 51.20887935, 4.37323284 51.20907724, 4.37357283 51.20922136, 4.37412524 51.20933485, 4.37471724 51.20948803, 4.37532902 51.20968771, 4.37595654 51.20994103, 4.37642646 51.21013236, 4.37668347 51.21030033, 4.37683165 51.21047282, 4.37703133 51.2106024, 4.37729776 51.2107017, 4.37754142 51.21085894, 4.37780726 51.21110344, 4.37821674 51.21135914, 4.3786931 51.21154082, 4.37920546 51.21171987, 4.37979197 51.21197176, 4.38041055 51.21236384, 4.38089383 51.21275401, 4.38139844 51.21307826, 4.38198066 51.21333516, 4.38260663 51.21358883, 4.38311064 51.21375871, 4.38348353 51.21388423, 4.38339543 51.21415424, 4.38369834 51.21423423, 4.38399887 51.21431494, 4.38422632 51.21445656, 4.38440871 51.21468627, 4.3846544 51.21498656, 4.38497317 51.21535134, 4.38537574 51.21574652, 4.38567603 51.21604204, 4.38593614 51.21619606, 4.38621581 51.21628165, 4.38647616 51.21643353, 4.38677645 51.21672452, 4.38717794 51.21711755, 4.38749373 51.21749985, 4.38773406 51.21786547, 4.38791811 51.21820283, 4.38817835 51.21841824, 4.38857198 51.21857715, 4.38898683 51.21882176, 4.38927984 51.21919346, 4.38953495 51.21977925, 4.38981915 51.22054577, 4.39024353 51.22110724, 4.39072835 51.22141397, 4.39092386 51.22145808, 4.39108515 51.22188377, 4.39130294 51.22221935, 4.39151895 51.22248042, 4.39175451 51.22274375, 4.39196575 51.22315884, 4.39218247 51.22392726, 4.39237165 51.22496545, 4.39260805 51.22560334, 4.39289415 51.22584844, 4.39313066 51.22603583, 4.39331996 51.22632074, 4.39353704 51.22662413, 4.39374745 51.22697127, 4.39397967 51.22752976, 4.39419043 51.22841525, 4.39440775 51.22946668, 4.39459765 51.23057616, 4.39483404 51.23120165, 4.39511597 51.23144472, 4.39534152 51.23169625, 4.39551806 51.23211312, 4.39575505 51.23252738, 4.39609635 51.23292446, 4.39662445 51.23336351, 4.39637816 51.233711, 4.39627075 51.23391223, 4.39604342 51.23406124, 4.39564443 51.23417616, 4.39509654 51.23430991, 4.39447045 51.23444092, 4.39391923 51.23458612, 4.3933959 51.23471916, 4.39273286 51.23485994, 4.39185524 51.23498917, 4.390944 51.23514426, 4.39005494 51.23533404, 4.38943923 51.2355988, 4.38908672 51.23584902, 4.38862574 51.23601711, 4.38785541 51.23613775, 4.38699257 51.23627877, 4.38613832 51.23641658, 4.38548505 51.23653436, 4.38488591 51.23653793, 4.38402212 51.23642313, 4.38281834 51.23627114, 4.38171196 51.23607886, 4.37634921 51.23611891, 4.37603116 51.23625827, 4.37564981 51.23625827, 4.37533188 51.23611891, 4.36107314 51.23611522, 4.36044908 51.23615777, 4.35955703 51.23612523, 4.35834122 51.23614323, 4.35723186 51.23626196, 4.35348654 51.23635745, 4.35253692 51.23653626, 4.35147727 51.23667967, 4.3506 51.2368226, 4.34991074 51.23694742, 4.34939873 51.23707688, 4.34893525 51.23720264, 4.34830201 51.23740125, 4.34730113 51.2377423, 4.34604895 51.23824084, 4.34501076 51.2386353, 4.34438336 51.23896587, 4.34410322 51.23928332, 4.34393585 51.23962188, 4.34374344 51.23995793, 4.34327221 51.24019086, 4.34239864 51.24038947, 4.33293676 51.24520457, 4.32791841 51.24739027, 4.3272171 51.24757135, 4.32643938 51.24771571, 4.32562053 51.24784052, 4.32496548 51.24798048, 4.32434416 51.24811304, 4.32345557 51.24825716, 4.32228243 51.24838924, 4.32138956 51.24853361, 4.32081962 51.24867642, 4.32038081 51.24885428, 4.3183459 51.24891174, 4.3175832 51.24909151, 4.31661165 51.24923944, 4.31540632 51.2493937, 4.31395435 51.24953008, 4.31274354 51.24963415, 4.31164527 51.24961817, 4.31045175 51.24951553, 4.30906415 51.24950063, 4.30798471 51.24966454, 4.30706751 51.25001442, 4.30609465 51.25051534, 4.30534804 51.25086725, 4.30479085 51.25109065, 4.30423892 51.25128174, 4.30364466 51.25150526, 4.30318022 51.25166941, 4.30288422 51.25185442, 4.30261433 51.25218916, 4.30224085 51.25271726, 4.30185926 51.25331831, 4.30131948 51.25380671, 4.30051792 51.25423372, 4.29947424 51.25473082, 4.29851937 51.25512326, 4.29809117 51.25564146, 4.29762554 51.25665236, 4.29701543 51.25732315, 4.29651046 51.2576282, 4.29621744 51.25785208, 4.29603624 51.25816345, 4.29583466 51.25848258, 4.29563153 51.25877023, 4.2954005 51.2590512, 4.29518855 51.25939095, 4.2949692 51.25986922, 4.29477775 51.26043141, 4.29454565 51.26078796, 4.29428411 51.26101375, 4.2940979 51.26136804, 4.29393363 51.26184917, 4.29352415 51.26204574, 4.29265797 51.26189125, 4.29147506 51.26166582, 4.29029834 51.26151347, 4.28907371 51.26156747, 4.28796101 51.26183736, 4.28691256 51.26219082, 4.28583693 51.26260245, 4.28479791 51.26292026, 4.28466213 51.26299536, 4.28491426 51.26324904, 4.28511465 51.26346183, 4.28538275 51.26362205, 4.28577435 51.26383007, 4.28617346 51.26422346, 4.28643596 51.26470113, 4.28667808 51.26500642, 4.28701031 51.26516056, 4.28746033 51.26530993, 4.28811586 51.26550758, 4.28908515 51.26577008, 4.29018915 51.26595461, 4.29100931 51.26602626, 4.29163826 51.26597917, 4.29234433 51.26597524, 4.29326212 51.2661041, 4.29441845 51.26636171, 4.29537261 51.26676607, 4.29594088 51.26716125, 4.29642844 51.26748395, 4.296983 51.26773763, 4.29760873 51.26799762, 4.29816961 51.26820397, 4.2986654 51.26840615, 4.2990998 51.26841581, 4.29957473 51.26882327, 4.30005705 51.26923311, 4.30067313 51.26962066, 4.30124784 51.26986778, 4.30176151 51.27004993, 4.30232 51.27024114, 4.30298185 51.27049875, 4.30366087 51.27074003, 4.30431402 51.27099848, 4.30478382 51.27130294, 4.30502927 51.27189326, 4.30516326 51.27276671, 4.30536926 51.27333271, 4.30571985 51.27356136, 4.30618668 51.27369881, 4.30676365 51.27386606, 4.30742526 51.27411735, 4.30802083 51.27436531, 4.30848312 51.27465165, 4.30877411 51.27496433, 4.30900955 51.27531743, 4.30921113 51.27572775, 4.30942011 51.27615345, 4.3095777 51.27661645, 4.30980933 51.27672636, 4.31013095 51.27682757, 4.31036866 51.27703333, 4.31054962 51.27735555, 4.31075442 51.27769172, 4.31095147 51.27809584, 4.31119156 51.27874112, 4.31149232 51.27961028, 4.31194651 51.28023541, 4.3124603 51.28057873, 4.31291425 51.28091276, 4.31321454 51.28132045, 4.31345391 51.28174448, 4.3136512 51.28216672, 4.31386006 51.28251028, 4.31405246 51.28275204, 4.31430495 51.28289235, 4.31460953 51.28298485, 4.31481588 51.28311503, 4.31481636 51.28328121, 4.31461155 51.28341305, 4.31431055 51.28352833, 4.31405902 51.28374124, 4.31385052 51.28408814, 4.31358755 51.28446102, 4.31352162 51.28473568, 4.3132205 51.28502727, 4.31291056 51.28531933, 4.31247425 51.28558981, 4.31197906 51.28582335, 4.3114897 51.28612685, 4.31099713 51.28650415, 4.31036401 51.28690112, 4.30976093 51.28721046, 4.30924416 51.28753901, 4.30875695 51.28792345, 4.30814326 51.28831422, 4.30758107 51.28856492, 4.30711794 51.28872406, 4.30666745 51.28884292, 4.30607426 51.28901124, 4.30517733 51.28927696, 4.30401063 51.28966463, 4.30280614 51.29000843, 4.30172861 51.29034805, 4.30070722 51.29069245, 4.29968631 51.29108286, 4.29875827 51.29135382, 4.29780102 51.29152346, 4.29659414 51.29161894, 4.29552281 51.29179442, 4.29382026 51.29198241, 4.29281592 51.29224348, 4.29243624 51.29248762, 4.29223585 51.29263866, 4.29200757 51.29273045, 4.29180765 51.29287243, 4.29165184 51.29312217, 4.29146552 51.29346192, 4.2911967 51.29381526, 4.29061365 51.29404545, 4.28969717 51.29417646, 4.28892207 51.29429853, 4.28833675 51.29441357, 4.28776205 51.29456437, 4.28713095 51.29477417, 4.28662312 51.29511106, 4.28630924 51.29547691, 4.2859565 51.29571092, 4.28543663 51.29584193, 4.28488135 51.29598236, 4.28426945 51.29616892, 4.28350544 51.29644001, 4.28263605 51.29673064, 4.28173244 51.29709482, 4.28074503 51.2972362, 4.28005648 51.29756677, 4.27949846 51.29778135, 4.27904403 51.29793406, 4.27862406 51.29805398, 4.27816915 51.29820204, 4.277583 51.29839635, 4.27679336 51.29868317, 4.27591145 51.29901063, 4.27514064 51.29941726, 4.27462018 51.29978263, 4.27413893 51.30007756, 4.27365303 51.30038703, 4.27322626 51.30094266, 4.27294087 51.30170596, 4.272686 51.30229354, 4.27239323 51.30266941, 4.27197802 51.30291295, 4.27158046 51.30304527, 4.27131283 51.30318534, 4.27112484 51.30343235, 4.27091241 51.30383134, 4.2707051 51.30437505, 4.27048814 51.30495393, 4.27032542 51.30560625, 4.26998484 51.30608368, 4.26985574 51.30657077, 4.26963365 51.30684614, 4.26934505 51.30700135, 4.26910412 51.30726826, 4.26891267 51.30772471, 4.26869547 51.30819893, 4.26848507 51.30867064, 4.26825297 51.30916584, 4.26804245 51.30973494, 4.26782572 51.31028295, 4.26763618 51.31076396, 4.26739967 51.3110311, 4.26711357 51.31117904, 4.26687705 51.31144357, 4.26668775 51.31191945, 4.26647091 51.31246531, 4.26626015 51.31305385, 4.26602805 51.31362033, 4.26581752 51.31420875, 4.26560044 51.31475437, 4.26541138 51.31523025, 4.26517534 51.31549525, 4.26488996 51.31564403, 4.26465166 51.31591213, 4.26445484 51.31639183, 4.26422298 51.31693065, 4.26400685 51.31747854, 4.26383722 51.31795561, 4.26385045 51.31848645, 4.26398945 51.31924176, 4.26399553 51.3202486, 4.26381016 51.32108021, 4.26358295 51.32177508, 4.26339817 51.32246375, 4.26340461 51.32323551, 4.26354253 51.32384956, 4.2635504 51.32431662, 4.26337004 51.32469058, 4.26325154 51.328125, 4.2629745 51.328125, 4.26301503 51.33022463, 4.26268291 51.33032405, 4.2622925 51.33042073, 4.26188076 51.33053434, 4.26151526 51.33064485, 4.26120222 51.33084416, 4.26079571 51.33122361, 4.26022661 51.33172286, 4.259817 51.33209121, 4.25952423 51.33227193, 4.25925303 51.33238375, 4.25902665 51.33259857, 4.25877666 51.33295321, 4.25837135 51.33327937, 4.25786197 51.33348107, 4.25723434 51.33365822, 4.25646341 51.33390141, 4.25573611 51.33428121, 4.25521767 51.33466291, 4.25466502 51.33501518, 4.25394785 51.33540106, 4.25315595 51.33590257, 4.25266373 51.33628845, 4.25235415 51.33648121, 4.25204861 51.33655655, 4.25058854 51.33726943, 4.24985218 51.33744764, 4.24930024 51.33771396, 4.24898434 51.3380146, 4.24875295 51.33831251, 4.24856186 51.3386029, 4.24835563 51.33887625, 4.24817073 51.33909953, 4.2479403 51.33923852, 4.24766254 51.33933353, 4.2474159 51.33948791, 4.24714327 51.33973575, 4.24671865 51.34000075, 4.24623525 51.34018517, 4.24578226 51.34032154, 4.24541402 51.34042656, 4.24514425 51.34059572, 4.24485886 51.34089863, 4.24444687 51.34129846, 4.24403584 51.341663, 4.24358141 51.34193742, 4.24318457 51.34213245, 4.24292374 51.34242272, 4.24272811 51.34288454, 4.24247718 51.34335506, 4.24237275 51.34361184, 4.24209166 51.34389544, 4.24187875 51.34417427, 4.24168515 51.34443402, 4.24150753 51.34465444, 4.24117982 51.34485614, 4.24056137 51.34516335, 4.23979282 51.34563875, 4.23929298 51.34601915, 4.23892522 51.34624875, 4.2384448 51.34648025, 4.23787117 51.3468436, 4.23739922 51.34721875, 4.23684275 51.3475728, 4.23604631 51.34796441, 4.23538172 51.34828532, 4.23512786 51.34840797, 4.24204865 51.35396728)), ((3.36945856 51.366822, 3.36854303 51.36667323, 3.36788225 51.36654127, 3.36713457 51.36642218, 3.36619604 51.36639071, 3.3653897 51.36644185, 3.36582792 51.36652195, 3.36625874 51.36670995, 3.36652005 51.36708593, 3.36633158 51.36762464, 3.36529398 51.36793852, 3.36339974 51.36790967, 3.36162066 51.36766052, 3.36023736 51.36742961, 3.35932374 51.36724806, 3.35866046 51.36711133, 3.35814714 51.36697125, 3.35767198 51.36684084, 3.35713458 51.36669683, 3.35653555 51.36656284, 3.35600865 51.36641753, 3.35548592 51.36628354, 3.35473096 51.36613834, 3.35357082 51.36600435, 3.35212195 51.36585915, 3.35039258 51.36572552, 3.34894431 51.36558151, 3.34769511 51.36544967, 3.34661114 51.36530554, 3.34553552 51.36516285, 3.34462154 51.36498475, 3.34296894 51.36492574, 3.34252775 51.36474776, 3.34208894 51.36460495, 3.34164524 51.36446094, 3.34116817 51.36432922, 3.3406111 51.36418521, 3.33998477 51.36405146, 3.33942723 51.36390626, 3.33891952 51.36377227, 3.33836591 51.36362708, 3.33774745 51.36349332, 3.33719361 51.36334825, 3.33668661 51.36321425, 3.33613265 51.36306906, 3.33551395 51.3629353, 3.3349601 51.36279023, 3.33445287 51.36265624, 3.33389914 51.36251116, 3.33328056 51.36237741, 3.33272695 51.36223197, 3.33221996 51.36209714, 3.33166385 51.3619498, 3.33103931 51.36181486, 3.33047843 51.3616786, 3.32998145 51.36157763, 3.3295747 51.36136675, 3.32880294 51.36126721, 3.32797813 51.36111033, 3.32696176 51.36089993, 3.32588363 51.36062813, 3.32489753 51.36042023, 3.32420182 51.36029172, 3.32371151 51.36028051, 3.32328773 51.36031926, 3.32287014 51.36027968, 3.31851256 51.36018252, 3.31773913 51.36000454, 3.31681538 51.35986173, 3.31581187 51.35971761, 3.31472206 51.35958576, 3.31383288 51.35944188, 3.3131299 51.35930824, 3.31258976 51.35916305, 3.31210637 51.35902905, 3.31156373 51.35888386, 3.31094694 51.35874987, 3.31039214 51.35860491, 3.30988407 51.35847104, 3.30932975 51.35832584, 3.30871105 51.35819185, 3.30815732 51.35804677, 3.30765033 51.35791302, 3.30709636 51.35776794, 3.3064779 51.35763395, 3.30592418 51.35748875, 3.30541706 51.35735381, 3.30486107 51.35720634, 3.30423665 51.35707152, 3.30367565 51.35693526, 3.30317867 51.35683417, 3.30223668 51.35633755, 3.30155671 51.35622931, 3.30099952 51.35608304, 3.30050755 51.35594404, 3.29996276 51.35583556, 3.29931462 51.35584295, 3.29864621 51.35593116, 3.29795241 51.35593986, 3.29728782 51.35583353, 3.29664695 51.35569525, 3.29610395 51.35554063, 3.29558063 51.35540175, 3.29491568 51.35525584, 3.29407311 51.35512233, 3.29329073 51.35497761, 3.29260337 51.35484374, 3.29205561 51.35469842, 3.29157066 51.35456395, 3.29103196 51.35441911, 3.29039598 51.35428774, 3.28974414 51.35414958, 3.28903627 51.35402358, 3.28823876 51.35386562, 3.28726041 51.35364914, 3.28623855 51.35331798, 3.28518546 51.35312462, 3.28423715 51.35263503, 3.28353012 51.35229111, 3.28300524 51.3519603, 3.28252387 51.3515836, 3.2819221 51.35120463, 3.28143084 51.35096562, 3.28116655 51.35079372, 3.28103101 51.35062528, 3.28082585 51.35049117, 3.28044236 51.35037863, 3.27986002 51.35022414, 3.27910185 51.35002077, 3.27834904 51.34975791, 3.27775943 51.3495512, 3.27728105 51.34938657, 3.2768085 51.34925985, 3.27627897 51.34913385, 3.27574754 51.34902, 3.27538276 51.34885263, 3.27506125 51.34860063, 3.27454174 51.348333, 3.27388966 51.34814167, 3.27330542 51.34799552, 3.27279592 51.34789991, 3.27226162 51.34768534, 3.27126861 51.34758317, 3.27046001 51.34744716, 3.26977634 51.34731722, 3.26925504 51.34718394, 3.26889455 51.34706664, 3.26865625 51.34690022, 3.26837957 51.34665251, 3.26786447 51.34638906, 3.26719356 51.34618843, 3.26654458 51.34599197, 3.26032746 51.34595275, 3.25991035 51.34578967, 3.25948715 51.34565651, 3.25899673 51.34548295, 3.25829697 51.34521282, 3.25730073 51.34479654, 3.25621092 51.34437764, 3.25521362 51.34395993, 3.25451005 51.34368682, 3.25401127 51.34351301, 3.25358295 51.34339285, 3.25319421 51.34327257, 3.25289214 51.34316075, 3.25266933 51.34299505, 3.2522521 51.34274685, 3.25119054 51.34248292, 3.24954498 51.34228206, 3.24805272 51.34208584, 3.24344575 51.34214938, 3.24268281 51.34227824, 3.24181104 51.34228528, 3.24093926 51.34217405, 3.23998654 51.34203303, 3.23896301 51.34185791, 3.23778784 51.34164822, 3.23672497 51.34138584, 3.23581684 51.34118128, 3.23491013 51.34101844, 3.23388624 51.3408885, 3.23302484 51.34074771, 3.23227835 51.34060585, 3.23152125 51.34043193, 3.23059595 51.34026265, 3.22953868 51.34016645, 3.22833514 51.34037185, 3.22739971 51.34101236, 3.22694206 51.34181094, 3.22668338 51.34228647, 3.22641754 51.34245527, 3.22614968 51.34252655, 3.22603822 51.34548342, 3.22583342 51.34597647, 3.22582173 51.34658563, 3.2259922 51.34727871, 3.22619605 51.34799874, 3.22645164 51.34841084, 3.22675276 51.34857523, 3.22700703 51.34875214, 3.22720897 51.34908056, 3.22737968 51.34954965, 3.22737944 51.35022902, 3.22720814 51.35114396, 3.22700572 51.35221136, 3.22675276 51.35283196, 3.22645927 51.35307193, 3.22621953 51.35332811, 3.2260226 51.35380983, 3.22579062 51.35442221, 3.22557366 51.35508895, 3.22540235 51.35557687, 3.2254132 51.35591435, 3.22555053 51.35624135, 3.22556281 51.35664845, 3.22540796 51.35714126, 3.22524285 51.35775626, 3.22486675 51.35821116, 3.22475195 51.35855043, 3.22455561 51.35874832, 3.22429264 51.35885584, 3.22399855 51.35904515, 3.22349918 51.35942948, 3.22261012 51.35992622, 3.22160053 51.36025918, 3.22050035 51.36045337, 3.21924734 51.36057866, 3.21827054 51.36070955, 3.21757734 51.36083674, 3.21706665 51.36098075, 3.21646082 51.36111534, 3.21537495 51.36126101, 3.21376586 51.36139512, 3.2123301 51.36154032, 3.21102285 51.36167407, 3.20971143 51.36181927, 3.20815265 51.36195314, 3.20661354 51.36209714, 3.20502675 51.36222875, 3.2037096 51.36237276, 3.20256054 51.36251557, 3.20164323 51.36269355, 3.20057845 51.36276054, 3.19962335 51.36294866, 3.19858301 51.36307955, 3.19778991 51.36314213, 3.19724607 51.36308575, 3.19686973 51.36299217, 3.19671535 51.36288095, 3.19689071 51.36275065, 3.19745648 51.36256993, 3.19842815 51.36231566, 3.19960642 51.36211038, 3.20063615 51.36194456, 3.20159864 51.36181188, 3.20256138 51.36167145, 3.20371676 51.36153984, 3.20502877 51.36139524, 3.20661616 51.36126137, 3.20817184 51.36111724, 3.20976293 51.36098552, 3.21107936 51.36084127, 3.21222687 51.36069846, 3.21314347 51.3605206, 3.21700466 51.36046076, 3.21743274 51.36028326, 3.21785104 51.36014271, 3.21827865 51.36000335, 3.21881306 51.35987544, 3.21963358 51.35971677, 3.22076821 51.35951841, 3.22178161 51.35925114, 3.22248161 51.35899436, 3.22289205 51.35870016, 3.22296095 51.35839558, 3.22280002 51.35811293, 3.22258401 51.35788202, 3.22221816 51.35772896, 3.22157872 51.35761726, 3.22069037 51.35748637, 3.21958256 51.35735655, 3.21856594 51.35721135, 3.21766603 51.35707664, 3.21700883 51.35693085, 3.21649444 51.35679555, 3.21595347 51.35664833, 3.21533895 51.3565135, 3.21478117 51.35637724, 3.2142843 51.35627615, 3.21382082 51.35585165, 3.21355104 51.35565221, 3.21296692 51.35550916, 3.21203506 51.35540688, 3.21123195 51.35528171, 3.21064174 51.35510755, 3.21019542 51.35478485, 3.20990551 51.35428715, 3.20971274 51.35357285, 3.20971346 51.35261834, 3.20991671 51.35173607, 3.21024024 51.35096407, 3.21072781 51.35051382, 3.21125781 51.35052371, 3.21165323 51.3509835, 3.21172142 51.35171795, 3.21157086 51.35245597, 3.21151257 51.35317147, 3.21166134 51.35373354, 3.21201456 51.35419178, 3.21275175 51.35456717, 3.21383953 51.35490406, 3.21490955 51.35524857, 3.21566164 51.35537922, 3.21620715 51.35530627, 3.21664143 51.35518825, 3.21702671 51.35463285, 3.21729636 51.35412657, 3.21756935 51.35383916, 3.21788764 51.35371435, 3.21810246 51.35359943, 3.21803296 51.35345197, 3.21754313 51.35330606, 3.21672583 51.35308921, 3.21610701 51.35266054, 3.2160207 51.35210514, 3.21639502 51.35172606, 3.21694875 51.35165811, 3.21757817 51.35174465, 3.21825373 51.35167992, 3.21881056 51.35130787, 3.21921885 51.35075974, 3.21966875 51.35031545, 3.22013927 51.35001194, 3.22054625 51.34968305, 3.22087157 51.34934223, 3.22139943 51.34909201, 3.22226644 51.34884191, 3.22310698 51.34846604, 3.22367346 51.34806752, 3.22396922 51.34769285, 3.22380435 51.34711456, 3.22358406 51.34669054, 3.22337306 51.34636235, 3.22316372 51.34601867, 3.22300363 51.34563041, 3.2228024 51.34529495, 3.22238433 51.34505272, 3.22135365 51.34489131, 3.21983993 51.34472394, 3.21917415 51.34446204, 3.22000778 51.34419274, 3.22171271 51.34397733, 3.22278535 51.34367037, 3.22300196 51.34321451, 3.22283495 51.34280658, 3.22278845 51.34249043, 3.22296965 51.34221172, 3.22314513 51.34191215, 3.22305214 51.34154725, 3.22243297 51.34114337, 3.22153056 51.34081996, 3.2206068 51.34053457, 3.21979976 51.34032667, 3.21913826 51.34012985, 3.21877575 51.34004486, 3.21832144 51.33996892, 3.21789014 51.34000671, 3.21752775 51.34014416, 3.21725881 51.34047282, 3.21698475 51.34102404, 3.21660805 51.34156656, 3.21624052 51.34193015, 3.21572804 51.34209168, 3.21506524 51.34206247, 3.21467686 51.34183836, 3.21466672 51.34146035, 3.21481454 51.34107685, 3.21474564 51.3407675, 3.21435416 51.34063137, 3.21390295 51.34076965, 3.21360123 51.34101105, 3.21332085 51.34120476, 3.2129451 51.34134615, 3.21252096 51.34146857, 3.2121762 51.3414911, 3.21203172 51.34137094, 3.21203208 51.34112001, 3.21195495 51.34076226, 3.21185577 51.34031606, 3.21269596 51.33956242, 3.21282542 51.33906174, 3.21293104 51.33892715, 3.21304715 51.33877957, 3.21333516 51.33865845, 3.21357703 51.33852911, 3.21377873 51.33831668, 3.21402597 51.33804596, 3.21427667 51.33774555, 3.2144742 51.33742237, 3.21436381 51.33710206, 3.21377778 51.33690858, 3.21283865 51.33705866, 3.21189713 51.3375262, 3.21135974 51.33793592, 3.21105266 51.33814955, 3.21075976 51.33823645, 3.20848811 51.33950186, 3.20800114 51.33967936, 3.20757961 51.33994424, 3.20731032 51.34024417, 3.2070806 51.34054613, 3.20678258 51.34085226, 3.20621252 51.34115386, 3.20536184 51.34142601, 3.20437038 51.34168351, 3.20346737 51.34175813, 3.20294285 51.34158325, 3.20269716 51.34120643, 3.20257926 51.34075117, 3.20270586 51.3402462, 3.20306134 51.33978105, 3.20339906 51.33946526, 3.20380485 51.33929706, 3.20436382 51.33919156, 3.20491636 51.33906746, 3.20540905 51.33894432, 3.20584834 51.33881283, 3.20620728 51.33869863, 3.20644927 51.33854377, 3.20664096 51.33831584, 3.20689321 51.33804858, 3.20718014 51.33770227, 3.2076112 51.33723617, 3.20765185 51.33685315, 3.20728016 51.33659875, 3.20676541 51.33655095, 3.20627832 51.3366518, 3.20589685 51.33677554, 3.20566082 51.33694196, 3.20550156 51.33719993, 3.20529175 51.3375591, 3.20498204 51.33798134, 3.20444214 51.33832645, 3.20375013 51.33853102, 3.20305693 51.33868337, 3.20235455 51.33881795, 3.20167601 51.3389703, 3.20099783 51.33908224, 3.20041525 51.33908927, 3.19997716 51.3389039, 3.19971883 51.33866215, 3.19948256 51.33849275, 3.19919085 51.33839726, 3.198897 51.33831251, 3.1968708 51.33553314, 3.19710255 51.3354125, 3.19915032 51.33616078, 3.20065713 51.33630574, 3.20057976 51.33582294, 3.19969118 51.33546066, 3.19878662 51.33513761, 3.19782364 51.3347522, 3.19742763 51.33461714, 3.19708824 51.33450127, 3.19669795 51.33425415, 3.19649363 51.33391356, 3.19683206 51.33285356, 3.19698656 51.33126044, 3.19669104 51.33089244, 3.19557714 51.33032894, 3.19462395 51.32997203, 3.19356406 51.32983136, 3.19224274 51.32996345, 3.19129992 51.3303622, 3.19109917 51.33083606, 3.19140935 51.33129776, 3.19210482 51.33163702, 3.19296062 51.33180857, 3.19316173 51.33253992, 3.19408905 51.3351953, 3.19501412 51.33823454, 3.19529176 51.33859313, 3.19551456 51.33917487, 3.19569337 51.33967745, 3.19563735 51.34004605, 3.19525635 51.34032476, 3.19462895 51.3405081, 3.19369555 51.34063685, 3.19256544 51.34065056, 3.19185746 51.34052146, 3.19155276 51.34028316, 3.1913743 51.33999896, 3.19126618 51.33970594, 3.19181275 51.33947361, 3.19166386 51.33912444, 3.19143856 51.33868575, 3.19114387 51.33815753, 3.19070637 51.33774614, 3.19021261 51.33750427, 3.18891168 51.33536422, 3.18794584 51.33425367, 3.18736625 51.33413315, 3.18628454 51.33401227, 3.18524134 51.33420551, 3.18539596 51.33480895, 3.18578231 51.33555734, 3.18643916 51.33705401, 3.18694127 51.33794725, 3.18717313 51.33850241, 3.18775272 51.33891284, 3.18818486 51.33942521, 3.19021666 51.34061825, 3.19070423 51.34079635, 3.19112766 51.34106112, 3.19140184 51.34135783, 3.19163585 51.34165084, 3.19192147 51.34194684, 3.19243061 51.34226465, 3.19315708 51.34264135, 3.19394481 51.34313512, 3.19442534 51.34351754, 3.19480014 51.34375787, 3.19540715 51.34401131, 3.19630587 51.34436643, 3.19715965 51.34463406, 3.19778395 51.34481716, 3.19828784 51.34494495, 3.19883156 51.34506965, 3.19945705 51.34518003, 3.20003128 51.34532738, 3.20052695 51.34557414, 3.20092547 51.34604955, 3.20120072 51.34664583, 3.20157218 51.34703422, 3.20217955 51.34725487, 3.20288837 51.34748554, 3.20288026 51.34784496, 3.20218384 51.34808445, 3.20160365 51.34826827, 3.20127273 51.34849632, 3.20098233 51.34879124, 3.20031881 51.34900868, 3.19923294 51.34913075, 3.19834077 51.34921062, 3.19768941 51.34924877, 3.19703495 51.34931362, 3.19633508 51.34925485, 3.19587946 51.34890461, 3.19558334 51.34829545, 3.19518161 51.34779394, 3.19461286 51.34752154, 3.19380105 51.34738755, 3.19284916 51.34743154, 3.19228947 51.34770596, 3.19220376 51.34812462, 3.19242191 51.34840655, 3.19274962 51.34854257, 3.19304454 51.34869885, 3.19335246 51.34898293, 3.19374907 51.34937334, 3.19407046 51.34976447, 3.19439864 51.35012662, 3.19381893 51.35033095, 3.19330275 51.35037661, 3.19285345 51.35028124, 3.19249094 51.35015702, 3.19224966 51.34999061, 3.19207382 51.34975696, 3.19187152 51.34948182, 3.19167376 51.34918797, 3.19143355 51.34888136, 3.19113874 51.3485769, 3.19070065 51.34834993, 3.19020152 51.34831512, 3.18970036 51.34839153, 3.18924463 51.34835207, 3.18896174 51.34811914, 3.18896174 51.34782171, 3.18897235 51.34756827, 3.18880117 51.34740067, 3.18845475 51.34727561, 3.18803024 51.34713435, 3.18755877 51.34703457, 3.18694961 51.34704006, 3.18606973 51.34721363, 3.18496466 51.34739864, 3.18347371 51.34662557, 3.18298686 51.34641564, 3.18256176 51.34607613, 3.1822803 51.34565914, 3.18203127 51.34528637, 3.18173897 51.34496784, 3.18128967 51.34469032, 3.18078315 51.34445703, 3.18035412 51.34418881, 3.18009961 51.34393895, 3.17986166 51.34376025, 3.17949176 51.34362984, 3.17892826 51.34353435, 3.17813921 51.34359693, 3.17721617 51.34385288, 3.17628527 51.34420824, 3.17552376 51.34470105, 3.1751008 51.34519792, 3.17485881 51.34556782, 3.17468941 51.34580266, 3.17445874 51.34593105, 3.17416704 51.34605002, 3.17396402 51.3463155, 3.17393661 51.34674525, 3.17404008 51.34714746, 3.17457235 51.3471086, 3.17505574 51.34729803, 3.17549884 51.34749305, 3.17578065 51.34776664, 3.17581773 51.34807313, 3.17592275 51.34836745, 3.1762526 51.34861255, 3.17662132 51.34880626, 3.17694485 51.34914386, 3.17742407 51.34975064, 3.1782788 51.35043061, 3.17931032 51.35100615, 3.18049276 51.35159934, 3.18163061 51.35213566, 3.18273962 51.35267282, 3.18355215 51.35305142, 3.18406916 51.35336232, 3.18434894 51.35367084, 3.18449652 51.35402453, 3.18444026 51.35442376, 3.18425763 51.35476685, 3.18426085 51.35501361, 3.18466294 51.355165, 3.18542564 51.35528052, 3.18621278 51.35543442, 3.18926191 51.35546172, 3.18989885 51.35526836, 3.19056535 51.35506308, 3.19111598 51.35477078, 3.19152296 51.35445952, 3.19197452 51.35425031, 3.19244492 51.35424924, 3.19284296 51.35437226, 3.19306934 51.35460806, 3.19307184 51.35491383, 3.19285786 51.35513222, 3.19248581 51.35526764, 3.19202673 51.35542095, 3.19146717 51.35562372, 3.19073725 51.35589302, 3.18981135 51.35610032, 3.18853796 51.35621905, 3.18690443 51.35619807, 3.18565154 51.35611343, 3.18482757 51.35618436, 3.18434227 51.35649836, 3.18408036 51.35688555, 3.18371975 51.35709226, 3.18306506 51.35702062, 3.18223774 51.35671973, 3.18167925 51.35655224, 3.18126178 51.3564018, 3.18090093 51.35628402, 3.18062258 51.35611105, 3.18032634 51.35581064, 3.17992866 51.35541594, 3.17961061 51.35505235, 3.17936134 51.35475433, 3.17917275 51.35452986, 3.17894292 51.35439634, 3.17866576 51.35431206, 3.17844546 51.35417771, 3.17827106 51.35394466, 3.17801344 51.35362232, 3.17759597 51.35322821, 3.17693734 51.35286963, 3.17629385 51.35279834, 3.17585278 51.35302067, 3.17576575 51.35334551, 3.17593765 51.35365343, 3.17615867 51.35389173, 3.17642593 51.35403466, 3.17672503 51.35412657, 3.17694831 51.35425985, 3.17707622 51.35445404, 3.17749417 51.3548851, 3.17740786 51.35523236, 3.17727935 51.35538375, 3.17712784 51.35530186, 3.17690194 51.35510266, 3.17642856 51.35494995, 3.17577326 51.35493374, 3.17541945 51.35506666, 3.1754837 51.35531211, 3.17578745 51.35560954, 3.17631006 51.35591531, 3.17696094 51.35622692, 3.17760527 51.35662353, 3.17800081 51.35698783, 3.17822504 51.35727572, 3.1783818 51.35749972, 3.17871225 51.35770655, 3.17942607 51.35802627, 3.18041551 51.35849273, 3.18112957 51.35881162, 3.18146038 51.35901475, 3.18161547 51.3592298, 3.18183422 51.35951006, 3.18222094 51.35989904, 3.18286884 51.36041164, 3.18383431 51.36046088, 3.18478572 51.36026895, 3.18581796 51.36011803, 3.18660104 51.36000454, 3.18715405 51.36000764, 3.18761444 51.36011755, 3.18797755 51.36023283, 3.18807256 51.36034584, 3.18771184 51.36045504, 3.18693435 51.36057115, 3.18604636 51.36072826, 3.18506217 51.36088002, 3.18398738 51.36097145, 3.18270016 51.36084116, 3.18150318 51.36046386, 3.18062043 51.36003351, 3.17998362 51.35953665, 3.17960727 51.35910654, 3.17921877 51.35885692, 3.17865527 51.35867965, 3.17809904 51.35841012, 3.17765474 51.35804856, 3.17723691 51.35764945, 3.17696416 51.35732865, 3.17676544 51.35709274, 3.17606306 51.35695267, 3.17490065 51.35633767, 3.17419255 51.35599935, 3.173913 51.35580826, 3.17380905 51.35559463, 3.17359841 51.35530806, 3.1730473 51.35494924, 3.17187452 51.35458553, 3.17042577 51.35434866, 3.1695224 51.35409451, 3.16906345 51.35364676, 3.16867554 51.35299087, 3.16838396 51.35219896, 3.16818631 51.35141397, 3.16819215 51.35062027, 3.16838467 51.3500092, 3.16861594 51.34945548, 3.16886497 51.34875977, 3.16908181 51.34789145, 3.16930008 51.34727442, 3.16949117 51.34696364, 3.16973245 51.34683931, 3.17002225 51.3467555, 3.17024505 51.34655905, 3.17037475 51.34621525, 3.17116904 51.34557295, 3.17131817 51.34503841, 3.1715126 51.34447384, 3.17170107 51.34388971, 3.17197335 51.3435297, 3.17237675 51.34332955, 3.17280197 51.34310055, 3.17310715 51.34280717, 3.17335546 51.34238565, 3.17355823 51.34174752, 3.17376804 51.34090722, 3.17395532 51.33995271, 3.17419255 51.33939397, 3.17447996 51.33915913, 3.17471767 51.33889711, 3.17490804 51.33847356, 3.17512631 51.33807302, 3.17533815 51.33772981, 3.1755712 51.33737314, 3.17577815 51.33697951, 3.17598426 51.33664262, 3.17616057 51.33639967, 3.17641842 51.33624125, 3.17684185 51.33608878, 3.17739224 51.33585823, 3.17758262 51.33543003, 3.17842972 51.33501112, 3.17908943 51.33470666, 3.17955506 51.33441675, 3.17983532 51.33412623, 3.18006134 51.33383846, 3.18025565 51.33355224, 3.18046844 51.33327734, 3.18066883 51.33304775, 3.18092644 51.33290112, 3.18121052 51.33281362, 3.18133104 51.3327229, 3.18117893 51.33263016, 3.18089354 51.33254945, 3.18074834 51.33245504, 3.18088615 51.33235943, 3.18118811 51.33226466, 3.1814121 51.33212137, 3.18141294 51.33194017, 3.18118954 51.33179712, 3.18088365 51.33170414, 3.18073344 51.33161354, 3.1808598 51.33152676, 3.1811558 51.33144176, 3.18144095 51.33129561, 3.181674 51.3310616, 3.1818434 51.33077204, 3.18172813 51.33046424, 3.18122816 51.33018863, 3.18055475 51.33000255, 3.17996717 51.32985282, 3.17947054 51.32973135, 3.17894661 51.32960677, 3.17829335 51.32949102, 3.17746484 51.32931197, 3.17630267 51.32898462, 3.17490745 51.32842386, 3.17478347 51.328125, 3.17429781 51.328125, 3.17437255 51.32800424, 3.17400312 51.32787061, 3.17350864 51.32769096, 3.17290044 51.32743812, 3.17232323 51.32723331, 3.17173934 51.32706738, 3.1710577 51.32693458, 3.17029321 51.32679415, 3.16944158 51.32666266, 3.1686753 51.32651925, 3.16791153 51.32638752, 3.16703832 51.32624316, 3.16599596 51.32610035, 3.16505945 51.32592225, 3.1640625 51.32589281, 3.1640625 51.32561386, 3.16384256 51.32555544, 3.16294563 51.32519901, 3.16204464 51.32491112, 3.16114724 51.32461774, 3.16028941 51.32435024, 3.15953934 51.32407641, 3.15891051 51.3238759, 3.15822506 51.32371652, 3.15739274 51.3235867, 3.15662515 51.32344675, 3.15595305 51.32331491, 3.15541542 51.32316995, 3.15490866 51.32303596, 3.15425754 51.32289064, 3.15339613 51.32275665, 3.15250683 51.32261145, 3.15160453 51.32247758, 3.15082777 51.32233274, 3.1501199 51.32219875, 3.14946556 51.32205355, 3.14882994 51.32191837, 3.14829051 51.321771149999996, 3.1478008 51.32163632, 3.14724505 51.32149994, 3.14656711 51.32139897, 3.14607382 51.32117903, 3.14557874 51.32107794, 3.14502001 51.32094181, 3.14439774 51.32080686, 3.14384377 51.32065964, 3.14333856 51.32052445, 3.14278722 51.32037926, 3.14217102 51.32024527, 3.14161921 51.32010043, 3.14111412 51.31996655, 3.14056241 51.31982136, 3.13994634 51.31968737, 3.13939476 51.31954241, 3.13888896 51.31940854, 3.13833344 51.31926346, 3.13770914 51.31912935, 3.13715386 51.31898427, 3.13667953 51.31885016, 3.13624108 51.3187052, 3.13581145 51.31857145, 3.13537514 51.31842744, 3.13490474 51.31829572, 3.13435137 51.31815171, 3.13371205 51.31800878, 3.1330986 51.3178308, 3.13261604 51.31771982, 3.13194001 51.31738877, 3.13136256 51.31717217, 3.13079166 51.31701434, 3.13011503 51.31688833, 3.12934077 51.31675005, 3.12846661 51.3166188, 3.12769127 51.31647384, 3.12702274 51.31633937, 3.12648845 51.31619394, 3.12598205 51.31605983, 3.12532795 51.31591523, 3.12446237 51.31578362, 3.12357736 51.31564343, 3.12270975 51.3155117, 3.12201643 51.31534731, 3.12138045 51.31514215, 3.12065005 51.31488025, 3.11992204 51.31467521, 3.1193397 51.31451047, 3.11886835 51.31437767, 3.11843348 51.31423533, 3.11799943 51.31410265, 3.11755896 51.31396687, 3.1171149 51.3138659, 3.11658835 51.31364477, 3.11590481 51.31354272, 3.11533892 51.31340706, 3.11483765 51.31327963, 3.11431515 51.31315136, 3.11379385 51.31303632, 3.11340296 51.3128494, 3.11302972 51.312572, 3.11296952 51.31252706, 3.11226344 51.31214237, 3.11149764 51.31187415, 3.11088836 51.31169462, 3.11040854 51.31158757, 3.10997784 51.31154382, 3.10955775 51.31158912, 3.10913694 51.31158674, 3.10868478 51.31146967, 3.10811365 51.31126964, 3.10732114 51.31096625, 3.10632944 51.31061637, 3.10523045 51.31019795, 3.10421002 51.30985165, 3.10331106 51.30955386, 3.10253251 51.30935431, 3.10184526 51.30920875, 3.10112977 51.30911601, 3.10059333 51.30897641, 3.10033691 51.30879855, 3.10014474 51.30843806, 3.09984696 51.30790973, 3.09905434 51.3075484, 3.09773505 51.30737424, 3.0966233 51.30724967, 3.09583116 51.30713511, 3.09525502 51.30697751, 3.09470952 51.30677307, 3.09410417 51.30651045, 3.09352684 51.30630457, 3.09294021 51.30613995, 3.0923115 51.30600822, 3.09175825 51.30586755, 3.09128344 51.30573547, 3.09084141 51.30559135, 3.09041393 51.30546141, 3.09000981 51.3053261, 3.08962667 51.30520141, 3.08917058 51.30503142, 3.08836305 51.30477047, 3.08685553 51.30436707, 3.08477533 51.30392945, 3.08273053 51.30338097, 3.08198595 51.30328465, 3.08152056 51.3030827, 3.08099425 51.30287313, 3.08035874 51.30260265, 3.07959926 51.30233288, 3.07852781 51.30201662, 3.07717657 51.30168045, 3.07600355 51.30130601, 3.07532573 51.30104101, 3.07503796 51.30078495, 3.07489157 51.30042636, 3.07470822 51.30001962, 3.07450783 51.2996006, 3.07427716 51.29924488, 3.07407773 51.29894996, 3.07389247 51.29869652, 3.07373512 51.29848635, 3.07342112 51.29829133, 3.07272267 51.29797101, 3.07162786 51.29743123, 3.07047892 51.29687655, 3.06937933 51.29633462, 3.06867647 51.29601085, 3.06837571 51.29581714, 3.06828201 51.29562986, 3.0677917 51.29523015, 3.06725025 51.29512775, 3.06656575 51.29499114, 3.06571007 51.29486024, 3.06493676 51.2947278, 3.06433106 51.29461706, 3.06399417 51.29446781, 3.06380987 51.29423642, 3.06356525 51.29393208, 3.06316662 51.29356396, 3.06256771 51.29318857, 3.06206274 51.29293025, 3.06175435 51.29266655, 3.06149161 51.29230726, 3.06109035 51.29198051, 3.06062591 51.29177678, 3.06016934 51.29163051, 3.05973053 51.29151463, 3.05927742 51.29137135, 3.05873406 51.29117727, 3.05802238 51.29086566, 3.05713665 51.29045725, 3.05608463 51.28993225, 3.05517316 51.28949678, 3.05434036 51.28907585, 3.05404305 51.28907585, 3.05331802 51.28883338, 3.05240142 51.28854752, 3.05137408 51.28818643, 3.05057478 51.28793645, 3.05016828 51.28774643, 3.04998255 51.28751791, 3.04978704 51.28723955, 3.04951692 51.28693748, 3.0490936 51.28666317, 3.0486201 51.28647745, 3.04815531 51.286327, 3.04767632 51.28620148, 3.04712641 51.28606606, 3.04650164 51.28593707, 3.0459131 51.28577733, 3.04531693 51.28557825, 3.04466677 51.28531134, 3.04408383 51.28505707, 3.04363275 51.28476751, 3.04335415 51.28446496, 3.04311395 51.28416276, 3.04283583 51.28386223, 3.04241204 51.28359675, 3.0419265 51.28341913, 3.04150772 51.28326285, 3.04089677 51.28305697, 3.04023135 51.28278005, 3.03957951 51.28251076, 3.03894174 51.28224051, 3.03836834 51.28204417, 3.03773284 51.28185082, 3.03688455 51.28158093, 3.03593075 51.28118813, 3.03511667 51.28084207, 3.03442764 51.28054202, 3.03388548 51.28034246, 3.03343785 51.28019345, 3.03301954 51.28007543, 3.03258085 51.2799319, 3.03205061 51.27973938, 3.03134716 51.27942991, 3.03042793 51.27901864, 3.02922606 51.27847457, 3.0280292 51.27799594, 3.02696753 51.27754307, 3.02614033 51.27715838, 3.02548015 51.27674186, 3.02502131 51.27634192, 3.02457595 51.27594924, 3.02412581 51.27597761, 3.02395117 51.27575505, 3.02371216 51.27549267, 3.02330911 51.27523506, 3.02281511 51.27505314, 3.02221966 51.27488542, 3.02145565 51.27468884, 3.02057421 51.2744174, 3.01962423 51.27415192, 3.01861882 51.27387095, 3.0176506 51.27361834, 3.01694345 51.27330482, 3.01641595 51.27291775, 3.01578546 51.27250254, 3.01512635 51.27217615, 3.01447332 51.27189624, 3.01386738 51.27170897, 3.01316512 51.27155042, 3.01225901 51.2713623, 3.01133251 51.27105737, 3.01038516 51.27064574, 3.00930595 51.2700963, 3.00832152 51.2696135, 3.00740922 51.26917934, 3.00664127 51.26887417, 3.00631321 51.26872921, 3.00588775 51.26863992, 3.00543511 51.26849866, 3.00488317 51.26829755, 3.00415206 51.26797557, 3.00324464 51.26756024, 3.00222588 51.2670598, 3.00153255 51.26673162, 3.00119412 51.26651084, 3.00094914 51.26626885, 3.00056446 51.26601911, 3.00010633 51.26583827, 2.99964905 51.26568985, 2.99920213 51.26556444, 2.99873185 51.26540828, 2.99817622 51.26520622, 2.99751616 51.26492596, 2.9968549 51.26465583, 2.99622023 51.26438916, 2.9956907 51.26419771, 2.99515605 51.26399434, 2.99437416 51.26366794, 2.99323142 51.26315403, 2.99200797 51.26264501, 2.99075365 51.26206493, 2.99032116 51.26199162, 2.98983383 51.26178706, 2.98927355 51.26157224, 2.9886142 51.26129401, 2.98795486 51.26102746, 2.98732185 51.26076317, 2.98679245 51.26057172, 2.98625064 51.26036358, 2.98544741 51.26002443, 2.98427701 51.25949585, 2.98308337 51.25900936, 2.98206842 51.25857854, 2.98138571 51.25831461, 2.98089516 51.25813663, 2.98038864 51.257954, 2.97974944 51.25768006, 2.97896576 51.25734794, 2.9779309 51.25690234, 2.97670972 51.25641811, 2.97536254 51.25584984, 2.97414255 51.25536478, 2.97313797 51.25493622, 2.97245932 51.25466931, 2.97198415 51.25450945, 2.97157121 51.25442112, 2.97073233 51.25395477, 2.97013032 51.25375485, 2.96951246 51.25350797, 2.96898532 51.25331616, 2.96848416 51.25312924, 2.9678117 51.25286603, 2.96681464 51.25244367, 2.96559417 51.2519598, 2.96424115 51.25138795, 2.96302104 51.25090396, 2.96202457 51.25048244, 2.96135414 51.25022161, 2.96085572 51.25003862, 2.96032548 51.2498461, 2.95968437 51.24957597, 2.95902598 51.24930775, 2.95838475 51.24903774, 2.95785367 51.24884474, 2.95735407 51.24866104, 2.95668387 51.24840045, 2.95569468 51.24798334, 2.95449531 51.24751174, 2.95316827 51.24695385, 2.95191693 51.24644685, 2.95068395 51.24587917, 2.94974017 51.24572623, 2.94856644 51.24519205, 2.94744837 51.24476373, 2.94632864 51.24433136, 2.94522846 51.24393642, 2.94410884 51.2435044, 2.94300592 51.24310315, 2.94188428 51.24266744, 2.94078004 51.24226534, 2.93965662 51.24183083, 2.93854928 51.24143195, 2.93742776 51.24100101, 2.93634284 51.24059677, 2.93527997 51.24013174, 2.93425727 51.23964965, 2.93321955 51.23912728, 2.93247044 51.23879337, 2.93197703 51.23860967, 2.93154311 51.23848665, 2.93097222 51.23830795, 2.93011057 51.23803067, 2.9290024 51.23768377, 2.92781734 51.23754108, 2.92648923 51.23760831, 2.92473495 51.23772013, 2.92392314 51.23749375, 2.92451501 51.23713911, 2.92574847 51.23578715, 2.92636526 51.23477328, 2.92752194 51.2340492, 2.92898667 51.2335664, 2.92944944 51.23298705, 2.92983484 51.23211801, 2.92937231 51.23129725, 2.92851472 51.23069501, 2.92923963 51.22962058, 2.93045175 51.22883487, 2.93237925 51.22782111, 2.9322952 51.22736955, 2.9316082 51.22719336, 2.93045175 51.22651756, 2.92966115 51.22713602, 2.92829275 51.22878671, 2.92752194 51.22980046, 2.92605686 51.23129725, 2.92544007 51.23255253, 2.92459202 51.23395264, 2.92428362 51.23448372, 2.9232043 51.23573887, 2.922279 51.23670447, 2.92178965 51.23684537, 2.92132628 51.23667681, 2.92073023 51.23647976, 2.91979063 51.23616457, 2.91852522 51.2357496, 2.91727304 51.23524106, 2.91644311 51.23490393, 2.9159137 51.23470092, 2.91539967 51.23451602, 2.91471624 51.23422587, 2.91382861 51.23383248, 2.91274905 51.2332983, 2.91182327 51.23278713, 2.91110516 51.23219061, 2.91063201 51.23158145, 2.91021693 51.23117435, 2.90986431 51.23098266, 2.90955937 51.23079288, 2.90908992 51.23043191, 2.90827966 51.22992563, 2.90739453 51.22946918, 2.90644824 51.22897828, 2.906479 51.22866845, 2.90578651 51.22833073, 2.90491414 51.22793448, 2.90373135 51.22739732, 2.90250981 51.22691464, 2.90137875 51.22648323, 2.90050101 51.22621453, 2.89990723 51.22604525, 2.89946067 51.22592425, 2.89905262 51.22579551, 2.89864063 51.22567213, 2.8981899 51.22551894, 2.89761806 51.22532463, 2.89683437 51.22503865, 2.89587557 51.22469962, 2.89482868 51.22424841, 2.89379132 51.22376192, 2.89270794 51.22321391, 2.89183152 51.22280514, 2.89113605 51.22250497, 2.89059401 51.22232056, 2.89009392 51.22215033, 2.88952267 51.22190845, 2.88891363 51.22152221, 2.88846481 51.22111285, 2.88797474 51.22079015, 2.88691783 51.22049057, 2.88581276 51.22008812, 2.88473833 51.21968806, 2.88369584 51.21923077, 2.88268173 51.21875203, 2.88154685 51.21816957, 2.88044453 51.21761847, 2.87937737 51.21708155, 2.87862313 51.21675336, 2.87810385 51.21656203, 2.87758744 51.21638346, 2.87691247 51.21609426, 2.876037 51.21569037, 2.87491834 51.21512306, 2.87379587 51.21456444, 2.87267721 51.21399677, 2.87180114 51.21359265, 2.87112546 51.21330285, 2.87061024 51.21312487, 2.87009716 51.21293855, 2.86935735 51.21262193, 2.86830103 51.21209455, 2.86715341 51.21151114, 2.86582208 51.21077752, 2.865152 51.21057725, 2.86449754 51.21026814, 2.86395693 51.21006536, 2.86341965 51.20986366, 2.86265934 51.20953393, 2.86159313 51.20899785, 2.86049485 51.20844984, 2.85936666 51.20787418, 2.85835421 51.20740354, 2.85728395 51.20693791, 2.85611582 51.20646715, 2.85486984 51.20589125, 2.85373235 51.20534265, 2.85266674 51.20480621, 2.85192001 51.20447814, 2.85140216 51.20428514, 2.85087526 51.20409942, 2.85017693 51.20379555, 2.84929085 51.20338535, 2.84826541 51.20287633, 2.84748662 51.20253205, 2.84692955 51.20231438, 2.84638965 51.20211995, 2.84577525 51.20187771, 2.84517932 51.20168185, 2.84345353 51.20071006, 2.84245956 51.20041156, 2.84144235 51.19998813, 2.84040356 51.19951391, 2.83917165 51.19893634, 2.83790374 51.19839156, 2.83667922 51.19782436, 2.83563602 51.19735372, 2.83455431 51.19687903, 2.83338571 51.19640362, 2.83220661 51.19586706, 2.83130515 51.19546592, 2.83062565 51.19517171, 2.83010495 51.19498575, 2.82957113 51.19478261, 2.82880235 51.19444525, 2.82777953 51.19393802, 2.82689345 51.19352782, 2.82619262 51.19322217, 2.82565343 51.1930294, 2.82510793 51.19282126, 2.82433915 51.19248462, 2.8233546 51.19200134, 2.82258642 51.19166434, 2.82204592 51.19145715, 2.82152164 51.1912688, 2.82084072 51.19097376, 2.81993604 51.19057035, 2.81874776 51.19002855, 2.81756937 51.18954682, 2.81650674 51.18908358, 2.81557548 51.18868291, 2.81459582 51.18826926, 2.81363106 51.18791723, 2.8127017 51.18753743, 2.8125 51.1875, 2.8125 51.18722093, 2.81230927 51.18721127, 2.81181705 51.18700373, 2.81126606 51.18679464, 2.81065917 51.18654895, 2.81011176 51.18635297, 2.80954397 51.18613207, 2.80876386 51.18578863, 2.80777705 51.18530381, 2.80700958 51.18496716, 2.80647051 51.18476057, 2.80594742 51.18457294, 2.80526626 51.18427765, 2.80435741 51.18387222, 2.80315685 51.18332458, 2.80196238 51.18283773, 2.80091727 51.18238747, 2.80012572 51.18205571, 2.79948342 51.18178475, 2.79896092 51.18159747, 2.79841411 51.18138611, 2.79763913 51.18104363, 2.79664385 51.18055141, 2.79586005 51.18020415, 2.79532051 51.18000877, 2.79488003 51.17991066, 2.79419506 51.17951858, 2.79320943 51.17911005, 2.7920233 51.17858052, 2.79086721 51.17811072, 2.7898252 51.17765713, 2.78889394 51.17725527, 2.78784311 51.17679954, 2.78667092 51.1763221, 2.78548133 51.17577946, 2.78456902 51.17537081, 2.78388143 51.17506695, 2.78337777 51.17488146, 2.78293443 51.17472827, 2.78235447 51.17454314, 2.78143144 51.17423892, 2.78013754 51.17382574, 2.77870095 51.17327285, 2.77744627 51.17278647, 2.77644491 51.17235935, 2.77577484 51.17209423, 2.7752732 51.17190766, 2.77474332 51.17171705, 2.77413428 51.17146826, 2.77357876 51.17125595, 2.77308321 51.17104661, 2.77262092 51.17097723, 2.77188146 51.17061496, 2.77120912 51.17032886, 2.7705785 51.17006612, 2.77005625 51.16988111, 2.76955616 51.16970062, 2.76887453 51.16943765, 2.76786685 51.16901088, 2.7666738 51.16852474, 2.76546431 51.16797161, 2.76453984 51.16755641, 2.76383543 51.16724634, 2.7633059 51.16705465, 2.76286697 51.16691113, 2.76244414 51.1667906, 2.76198447 51.16663647, 2.76143074 51.16643476, 2.76076913 51.16615427, 2.76010346 51.16588211, 2.75946057 51.16561031, 2.75892484 51.16541255, 2.75841355 51.16521788, 2.75773716 51.16495192, 2.75679505 51.16457772, 2.75577533 51.16426933, 2.75406635 51.16333842, 2.75282454 51.16292286, 2.75149918 51.16238224, 2.75029898 51.16190362, 2.74930906 51.16147912, 2.74863851 51.16121554, 2.7481389 51.16103172, 2.7476083 51.16083932, 2.74696755 51.16056967, 2.74631035 51.16030204, 2.74566853 51.16003156, 2.74513006 51.15983462, 2.74461067 51.15964091, 2.74391878 51.15937102, 2.74296916 51.15897834, 2.74200833 51.15863287, 2.74120581 51.15833282, 2.74059212 51.15813148, 2.74002123 51.15797663, 2.73940861 51.15784967, 2.73886287 51.15771162, 2.73839104 51.15758193, 2.73794913 51.15743792, 2.73751128 51.15729475, 2.73707128 51.15711653, 2.73665822 51.15705752, 2.73605072 51.15687954, 2.73541915 51.15673673, 2.73486912 51.15659261, 2.73436773 51.15646076, 2.73381746 51.15631664, 2.73320115 51.15618265, 2.73265004 51.15603805, 2.73214865 51.15590656, 2.73160446 51.15576625, 2.73099124 51.15563416, 2.73040795 51.15546954, 2.72978854 51.1552639, 2.72905135 51.15500247, 2.72832835 51.15480185, 2.72776294 51.15464807, 2.72727585 51.15452552, 2.72668505 51.154356, 2.72585082 51.15408528, 2.72486413 51.15366745, 2.72390592 51.1532495, 2.72355485 51.15310204, 2.72293508 51.15284157, 2.72213483 51.15259278, 2.72156763 51.15244007, 2.72171664 51.15234375, 2.72117353 51.15234375, 2.72105205 51.15208983, 2.71993494 51.15148675, 2.71895611 51.15102625, 2.71791077 51.15057218, 2.71676588 51.15011084, 2.71554434 51.14954615, 2.71439075 51.1490041, 2.7131592 51.14843178, 2.71199465 51.14795577, 2.71094108 51.14749563, 2.71003866 51.14709151, 2.70920086 51.14669621, 2.70857048 51.14643991, 2.70807827 51.14626181, 2.70754564 51.14607096, 2.70684016 51.14576125, 2.70595276 51.14534831, 2.70493197 51.14484024, 2.70416605 51.14450312, 2.70363414 51.14429975, 2.70311642 51.14411175, 2.70244253 51.14381385, 2.70155168 51.14341116, 2.70038164 51.14289117, 2.69919336 51.14247441, 2.697914 51.1417793, 2.69748294 51.14167941, 2.69701195 51.14152336, 2.69642413 51.14131713, 2.69568145 51.14105284, 2.69493854 51.14084625, 2.69435143 51.1406821, 2.69388485 51.14055264, 2.69346476 51.14041781, 2.69304848 51.14029086, 2.69259393 51.14013183, 2.69201767 51.13992834, 2.69124734 51.13964701, 2.69036126 51.1393702, 2.68947983 51.13908946, 2.68872201 51.13888895, 2.68815696 51.13873422, 2.68767083 51.13860691, 2.68709934 51.13844764, 2.68636906 51.13824487, 2.68563092 51.13798451, 2.68504322 51.13778114, 2.6845715 51.13761735, 2.68413937 51.1374768, 2.68371224 51.13730526, 2.68333566 51.1372689, 2.68268013 51.13705921, 2.68193865 51.13684487, 2.6812191 51.13659036, 2.68062174 51.13639295, 2.68001986 51.13621545, 2.67926204 51.13601458, 2.67839146 51.13573873, 2.67752194 51.13546681, 2.67676794 51.13519084, 2.67617154 51.13498986, 2.67557132 51.13481092, 2.67481494 51.13461006, 2.67397523 51.13435364, 2.67321384 51.13415062, 2.67263317 51.13398647, 2.67213964 51.13385463, 2.67160141 51.13371408, 2.67099226 51.13358271, 2.67044353 51.13343775, 2.66993976 51.13330257, 2.66938734 51.13315523, 2.66876745 51.13302004, 2.66821074 51.13288403, 2.66771758 51.13278294, 2.66722596 51.13256288, 2.66653907 51.13246202, 2.66597855 51.13232565, 2.66551375 51.13219082, 2.66508305 51.13204336, 2.66465664 51.13190854, 2.66422021 51.13176322, 2.66374731 51.13162935, 2.66319394 51.13148427, 2.66257215 51.13135028, 2.66201866 51.13120532, 2.66151476 51.13107145, 2.66096532 51.13092625, 2.66035163 51.13079226, 2.65980184 51.1306473, 2.65929866 51.13051331, 2.65874875 51.13036787, 2.65813446 51.13023365, 2.65758526 51.13008881, 2.65708518 51.12995744, 2.65654123 51.12981915, 2.65592885 51.12969315, 2.65535367 51.12953532, 2.65476275 51.12931871, 2.65407765 51.12898767, 2.65310645 51.12884617, 2.65220773 51.12848365, 2.65134907 51.12819564, 2.65060747 51.1279273, 2.65001976 51.12773561, 2.64942813 51.12756407, 2.64867365 51.12736583, 2.64779878 51.1270895, 2.64691615 51.1268146, 2.64614713 51.12653375, 2.6455729 51.1263324, 2.64512146 51.12617767, 2.64470434 51.12605464, 2.64426267 51.12590611, 2.64373541 51.12571394, 2.64307404 51.12542975, 2.64228117 51.12509537, 2.64125371 51.12465405, 2.64007235 51.12417245, 2.63881373 51.12358534, 2.63767076 51.12302935, 2.63659835 51.12248385, 2.6358397 51.12214565, 2.63533115 51.12196612, 2.63491094 51.12187874, 2.63416171 51.12146688, 2.63322115 51.12105322, 2.63220644 51.12055731, 2.63144684 51.12022316, 2.63090158 51.12001204, 2.63036263 51.11981678, 2.62972665 51.11955047, 2.62907433 51.11928487, 2.62843704 51.11901557, 2.62790906 51.11882317, 2.62741256 51.11863995, 2.62674463 51.11837924, 2.62575233 51.11795771, 2.62453675 51.11747384, 2.62318754 51.11690116, 2.62196708 51.11641455, 2.62096608 51.11598766, 2.62029791 51.11572623, 2.61984491 51.11556816, 2.61945355 51.11545706, 2.61900401 51.11530375, 2.61834574 51.1150527, 2.61736071 51.11463571, 2.61613715 51.11412382, 2.61469352 51.11342168, 2.61349726 51.11314702, 2.61257482 51.11266375, 2.61190891 51.11237586, 2.61143112 51.11220813, 2.61097705 51.11208928, 2.61040986 51.11193883, 2.60968673 51.11174536, 2.60897374 51.11149931, 2.60841107 51.11130464, 2.60786653 51.11109138, 2.60707033 51.11074984, 2.60590816 51.11022115, 2.60472107 51.10973573, 2.60370946 51.10930431, 2.60302174 51.10903597, 2.60251307 51.10884774, 2.60198581 51.10865605, 2.60138655 51.10841072, 2.60084462 51.10821426, 2.60034812 51.10803556, 2.59979892 51.10783422, 2.59915113 51.10755825, 2.59849036 51.10728645, 2.5978229 51.10700917, 2.59721363 51.10680366, 2.59696424 51.10668254, 2.59616172 51.10638106, 2.59513974 51.10595202, 2.59393215 51.10547125, 2.59259462 51.10490286, 2.59138334 51.1044203, 2.59039021 51.10399795, 2.58971727 51.10373437, 2.58921325 51.10354686, 2.58868635 51.10335493, 2.58809066 51.10310948, 2.58756065 51.10291541, 2.58707893 51.10274184, 2.58650851 51.10254252, 2.58571553 51.10224664, 2.58471966 51.10190105, 2.58364856 51.10150325, 2.58276534 51.10122836, 2.58215857 51.10104954, 2.58170664 51.10092485, 2.58129597 51.10079694, 2.58087754 51.10067546, 2.58041692 51.10051835, 2.57985616 51.10030127, 2.57916844 51.09996974, 2.57819915 51.09982836, 2.57729626 51.09946585, 2.57639778 51.09917784, 2.57553911 51.09890914, 2.57479715 51.09871733, 2.57421005 51.09854615, 2.57362211 51.09835041, 2.57287538 51.09807897, 2.57200444 51.09780657, 2.57109106 51.09750617, 2.57020795 51.09723151, 2.56944573 51.09695518, 2.56887674 51.09675658, 2.56842995 51.09660268, 2.5680176 51.09647763, 2.56757104 51.09632361, 2.56700253 51.09612513, 2.56624103 51.09584916, 2.5653578 51.09557474, 2.56444073 51.09527385, 2.56355774 51.09499836, 2.56279576 51.09471977, 2.56222296 51.09451795, 2.56176448 51.09436762, 2.56134045 51.09427106, 2.56095862 51.09406733, 2.56046546 51.09396732, 2.55987334 51.09381115, 2.55913198 51.09360456, 2.55839181 51.09334075, 2.55780911 51.09313667, 2.55735016 51.09297717, 2.5569334 51.09284997, 2.55648637 51.09269595, 2.55591881 51.09249771, 2.55515754 51.09222186, 2.55427432 51.09194732, 2.55335677 51.09164643, 2.55247355 51.09137177, 2.55171227 51.09109604, 2.55114472 51.09089768, 2.55069792 51.09074354, 2.55028105 51.09061611, 2.54982221 51.09045672, 2.54923892 51.09025395, 2.54849684 51.08999336, 2.54775381 51.08978987, 2.54716575 51.08962607, 2.54669464 51.08948565, 2.54626346 51.08931434, 2.54361111 51.09361111, 2.39027778 51.26916667, 2.37552528 51.29718832, 2.35365697 51.33872611, 2.30586236 51.42950965, 2.23833333 51.55777778, 2.25333333 51.61305556, 2.30586236 51.65721317, 2.48166667 51.805, 2.53933306 51.87611444, 3.08138889 51.55166667, 3.21222222 51.48472222, 3.29638889 51.45, 3.31192855 51.43093201, 3.33615505 51.40120481, 3.35388889 51.37944444, 3.36458333 51.37361111, 3.37040317 51.36696434, 3.36945856 51.366822)))"^^<http://www.opengis.net/ont/geosparql#wktLiteral> ;
#>     <http://www.w3.org/ns/prov#hadPrimarySource> <http://www.marineregions.org> .
```

The geometry is provided as
[WKT](https://en.wikipedia.org/wiki/Well-known_text_representation_of_geometry).
This is the same logic that `gaz_geometry(format = "wkt")` uses to
extract the geometry:

``` r

sparql <- "
  PREFIX gsp: <http://www.opengis.net/ont/geosparql#>
  SELECT ?o
  WHERE {
    ?s gsp:asWKT ?o .
  }"

rdf_query(bpns_geom, query = sparql)
#> # A tibble: 1 × 1
#>   o                                                                             
#>   <chr>                                                                         
#> 1 <http://www.opengis.net/def/crs/OGC/1.3/CRS84> MULTIPOLYGON (((4.24204865 51.…
```

#### Extract the label in another language

Using the SPARQL syntax you can extract the label of the record in a
specific language, if this is available. If not, the preferred label
will be returned

Example: Get the Gazetteer entry for *“Belgium”* with MRGID
`<http://marineregions.org/mrgid/14>` and extract the label in Dutch
(nl) if available.

``` r

belgium <- gaz_search(14, rdf = TRUE)

sparql <- '
  PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
  SELECT (COALESCE(?altLabel, ?prefLabel) AS ?o)
  WHERE {
    OPTIONAL {
      ?s skos:altLabel ?altLabel .
      filter langMatches(lang(?altLabel), "nl")
    }
    ?s skos:prefLabel ?prefLabel .
  }
'

rdf_query(belgium, query = sparql)
#> # A tibble: 26 × 1
#>    o                             
#>    <chr>                         
#>  1 Geul                          
#>  2 Belgium                       
#>  3 Meuse Basin                   
#>  4 Belgian Internal Waters       
#>  5 Brussels Hoofdstedelijk Gewest
#>  6 Maas                          
#>  7 Zwin                          
#>  8 IJzer                         
#>  9 Belgium                       
#> 10 Scheldebekken                 
#> # ℹ 16 more rows
```

## Mirror the Marine Regions Gazetteer using the LDES Feed

The Marine Regions Gazetteer is accessible as RDF, and can be mirrored
and synchronized via [Linked Data Event Streams
(LDES)](https://w3id.org/ldes/specification). This is described with
high detail in:

> Lonneville B. et al. (2021) Publishing the Marine Regions Gazetteer as
> a Linked Data Event Stream. S4BioDiv 2021.
> <http://ceur-ws.org/Vol-2969/paper8-s4biodiv.pdf>

Building such API in R is, at the moment, out of the scope of mregions2.
However, an interested user could make use of mregions2 and
[`rdflib::rdflib`](https://docs.ropensci.org/rdflib/reference/rdflib-package.html)
to mirror the Marine Regions Gazetteer. The general approach is
described below:

### Initialize a database

By design, mregions2 stores the RDF triples in memory. This is because
the package does not aim at replicating the Marine Regions Gazetteer,
but only consult small subsets.

[`rdflib::rdflib`](https://docs.ropensci.org/rdflib/reference/rdflib-package.html)
allows to store the triples in a database. This is further explained in
`vignette("storage", package = "rdflib")`.

As an example, using a
[Virtuoso](https://en.wikipedia.org/wiki/Virtuoso_Universal_Server)
database backend is possible. Virtuoso is a popular open source database
for RDF.

``` r

triplestore <- rdf(storage = "virtuoso", 
                   user = "dba", 
                   password = "dba", 
                   dsn = "Local Virtuoso"
                   )

# Turn bpns into nquads
tmp <- tempfile(fileext = ".nq")
write_nquads(bpns, tmp)

# Parse bpns as nquads into the store
rdf_parse(tmp, format = "nquads", rdf = triplestore)
```

### Linked Data Event Stream Feed

The LDES feed is available at:

> `https://marineregions.org/feed`

This offers fragmentation of the whole Marine Regions Gazetteer based on
the last modification time of the
[`?MRGID`](https://docs.ropensci.org/mregions2/reference/MRGID.md). It
primary use is to make it easy to have external caches, replications, or
derived indexes in an efficient and incremental way.

The implementation would start by fetching the feed as start page and
then follow the `tree:relation` paths down to pages that contain only
`tree:member` entries that are older than the requested date.

^(Source: <https://github.com/ropensci/mregions/pull/62#issuecomment-1091837752>)
