# Get one record for the given MRGID

Get one record for the given MRGID

## Usage

``` r
gaz_rest_record_by_mrgid(mrgid, with_geometry = FALSE, rdf = FALSE)
```

## Arguments

- mrgid:

  (integer) A valid Marine Regions Gazetteer Identifier
  ([MRGID](https://docs.ropensci.org/mregions2/reference/MRGID.md))

- with_geometry:

  (logical) Add geometry to the result data frame? Default = FALSE

- rdf:

  (logical) Return an object of class
  [rdflib::rdf](https://docs.ropensci.org/rdflib/reference/rdf.html)?

## Value

A data frame with the Gazetteer entry

## See also

[gaz_rest](https://docs.ropensci.org/mregions2/reference/gaz_rest.md),
[MRGID](https://docs.ropensci.org/mregions2/reference/MRGID.md)

## Examples

``` r
# \donttest{
gaz_rest_record_by_mrgid(3293)
#> # A tibble: 1 × 14
#>   MRGID gazetteerSource    placeType latitude longitude minLatitude minLongitude
#>   <int> <chr>              <chr>        <dbl>     <dbl>       <dbl>        <dbl>
#> 1  3293 Flanders Marine I… EEZ           51.5      2.72        51.0         2.24
#> # ℹ 7 more variables: maxLatitude <dbl>, maxLongitude <dbl>, precision <dbl>,
#> #   preferredGazetteerName <chr>, preferredGazetteerNameLang <chr>,
#> #   status <chr>, accepted <int>
gaz_rest_record_by_mrgid(3293, with_geometry = TRUE)
#> Simple feature collection with 1 feature and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 2.238333 ymin: 51.03976 xmax: 4.402039 ymax: 51.87611
#> Geodetic CRS:  WGS 84
#> # A tibble: 1 × 15
#>   MRGID gazetteerSource    placeType latitude longitude minLatitude minLongitude
#>   <int> <chr>              <chr>        <dbl>     <dbl>       <dbl>        <dbl>
#> 1  3293 Flanders Marine I… EEZ           51.5      2.72        51.0         2.24
#> # ℹ 8 more variables: maxLatitude <dbl>, maxLongitude <dbl>, precision <dbl>,
#> #   preferredGazetteerName <chr>, preferredGazetteerNameLang <chr>,
#> #   status <chr>, accepted <int>, the_geom <MULTIPOLYGON [°]>
gaz_rest_record_by_mrgid(3293, rdf = TRUE)
#> Total of 99 triples, stored in hashes
#> -------------------------------
#> <http://marineregions.org/mrgid/3293> <http://marineregions.org/ns/ontology#contains> <http://marineregions.org/mrgid/28318> .
#> <http://marineregions.org/mrgid/3293> <http://marineregions.org/ns/ontology#contains> <http://marineregions.org/mrgid/27790> .
#> <http://marineregions.org/mrgid/3293> <http://marineregions.org/ns/ontology#contains> <http://marineregions.org/mrgid/27555> .
#> <http://marineregions.org/mrgid/3293> <http://marineregions.org/ns/ontology#contains> <http://marineregions.org/mrgid/17865> .
#> <http://marineregions.org/mrgid/3293> <http://marineregions.org/ns/ontology#contains> <http://marineregions.org/mrgid/17666> .
#> <http://marineregions.org/mrgid/3293> <http://marineregions.org/ns/ontology#contains> <http://marineregions.org/mrgid/17409> .
#> <http://marineregions.org/mrgid/3293> <http://marineregions.org/ns/ontology#contains> <http://marineregions.org/mrgid/17401> .
#> <http://marineregions.org/mrgid/3293> <http://marineregions.org/ns/ontology#contains> <http://marineregions.org/mrgid/4675> .
#> <http://marineregions.org/mrgid/3293> <http://marineregions.org/ns/ontology#contains> <http://marineregions.org/mrgid/2421> .
#> <http://marineregions.org/mrgid/3293> <http://marineregions.org/ns/ontology#contains> <http://marineregions.org/mrgid/2420> .
#> 
#> ... with 89 more triples
# }
```
