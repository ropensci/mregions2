# Search in the Marine Regions Gazetteer by names, MRGID or reverse geocode with a pair of WGS84 coordinates x and y

Search in the Marine Regions Gazetteer by names, MRGID or reverse
geocode with a pair of WGS84 coordinates x and y

## Usage

``` r
gaz_search(x, ...)

# S3 method for class 'character'
gaz_search(x, ...)

# S3 method for class 'numeric'
gaz_search(x, ..., y = NULL)

# S3 method for class 'sfg'
gaz_search(x, ...)

# S3 method for class 'sf'
gaz_search(x, ...)

# S3 method for class 'sfc'
gaz_search(x, ...)
```

## Arguments

- x:

  object to perform the search with. Can be:

  - (character) Free text search

  - (integer) A valid Marine Regions Gazetteer Identifier
    ([MRGID](https://docs.ropensci.org/mregions2/reference/MRGID.md))

  - (double) Longitude in WGS84

  - Aditionally, you can pass objects of class
    [sf::sf](https://r-spatial.github.io/sf/reference/sf.html) or
    [sf::sfc](https://r-spatial.github.io/sf/reference/sfc.html) with
    geometry of class `POINT`

- ...:

  Arguments passed on to
  [`gaz_rest_record_by_mrgid`](https://docs.ropensci.org/mregions2/reference/gaz_rest_record_by_mrgid.md),
  [`gaz_rest_records_by_name`](https://docs.ropensci.org/mregions2/reference/gaz_rest_records_by_name.md),
  [`gaz_rest_records_by_names`](https://docs.ropensci.org/mregions2/reference/gaz_rest_records_by_names.md),
  [`gaz_rest_records_by_lat_long`](https://docs.ropensci.org/mregions2/reference/gaz_rest_records_by_lat_long.md)

  `with_geometry`

  :   (logical) Add geometry to the result data frame? Default = FALSE

  `rdf`

  :   (logical) Return an object of class
      [rdflib::rdf](https://docs.ropensci.org/rdflib/reference/rdf.html)?

  `typeid`

  :   (numeric) Restrict to one or more placetypeIDs. Retrieve a list of
      placetypeIDs with
      [`gaz_rest_types()`](https://docs.ropensci.org/mregions2/reference/gaz_rest_types.md)

  `language`

  :   (character) Restrict to one language. Provide as a 2 digits
      ISO-639. See ISOcodes::ISO_639_2.

  `like`

  :   (logical) Add a '%'-sign before and after the name? (SQL LIKE
      function). Default = TRUE

  `fuzzy`

  :   (logical) Use Levenshtein query to find nearest matches? Default =
      TRUE

- y:

  (double) Latitude in WGS84 (Optional)

## Value

A data frame with Gazetteer entries

## Examples

``` r
# \donttest{

# Look-up a name in the Gazetteer
gaz_search("North Sea")
#> # A tibble: 26 × 14
#>    MRGID gazetteerSource     placeType latitude longitude preferredGazetteerName
#>    <int> <chr>               <chr>        <dbl>     <dbl> <chr>                 
#>  1  2350 (1953). Limits of … IHO Sea …     56.4      2.74 North Sea             
#>  2  2399 ASFA thesaurus      Bight         51.6      3.02 Southern Bight of the…
#>  3  8542 Large Marine Ecosy… Large Ma…     57.4      2.73 North Sea             
#>  4 17441 Aphia               General …     53.1      2.81 Central North Sea     
#>  5 17977 Aphia               General …     59        1.8  Northern North Sea    
#>  6 17977 Aphia               General …     59        1.8  Northern Part of the …
#>  7 21912 Spalding, M. D. Fo… Marine E…     55.5      4.59 North Sea             
#>  8 22165 NA                  ICES Eco…     NA       NA    North Sea             
#>  9 22253 Aphia               General …     51.4      1.99 Southern North Sea    
#> 10 22762 Flanders Marine In… Marine R…     54.6      6.78 German part of the No…
#> # ℹ 16 more rows
#> # ℹ 8 more variables: preferredGazetteerNameLang <chr>, status <chr>,
#> #   accepted <int>, minLatitude <dbl>, minLongitude <dbl>, maxLatitude <dbl>,
#> #   maxLongitude <dbl>, precision <dbl>

# Get the entries of two known MRGID including their geometry
gaz_search(c(14, 17), with_geometry = TRUE)
#> Simple feature collection with 2 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -5.143675 ymin: 41.33376 xmax: 9.560596 ymax: 51.50408
#> Geodetic CRS:  WGS 84
#> # A tibble: 2 × 15
#>   MRGID gazetteerSource    placeType latitude longitude minLatitude minLongitude
#> * <int> <chr>              <chr>        <dbl>     <dbl>       <dbl>        <dbl>
#> 1    14 (2001). The Times… Nation        50.5      4.48        49.5         2.55
#> 2    17 (2001). The Times… Nation        46.2      2.19        41.3        -5.18
#> # ℹ 8 more variables: maxLatitude <dbl>, maxLongitude <dbl>, precision <dbl>,
#> #   preferredGazetteerName <chr>, preferredGazetteerNameLang <chr>,
#> #   status <chr>, accepted <int>, the_geom <MULTIPOLYGON [°]>

# Maybe the name is in another language...
gaz_search("Noordzee", language = "nl")
#> # A tibble: 4 × 14
#>   MRGID gazetteerSource    placeType latitude longitude minLatitude minLongitude
#>   <int> <chr>              <chr>        <dbl>     <dbl>       <dbl>        <dbl>
#> 1  2350 (1953). Limits of… IHO Sea …     56.4      2.74        51.0        -4.45
#> 2  2399 ASFA thesaurus     Bight         51.6      3.02        NA          NA   
#> 3 27417 Natura 2000 Sites… Natura 2…     53.3      5.25        NA          NA   
#> 4 50168 Wikipedia          Channel       52.5      4.56        NA          NA   
#> # ℹ 7 more variables: maxLatitude <dbl>, maxLongitude <dbl>, precision <dbl>,
#> #   preferredGazetteerName <chr>, preferredGazetteerNameLang <chr>,
#> #   status <chr>, accepted <int>

# Get all the records intersecting with the longitude 51.21551 and latitude 2.927
# restricting to some placetypes
gaz_search(x = 2.927, y = 51.21551, typeid = c(255, 259))
#> # A tibble: 4 × 13
#>   MRGID gazetteerSource    placeType latitude longitude minLatitude minLongitude
#>   <int> <chr>              <chr>        <dbl>     <dbl>       <dbl>        <dbl>
#> 1 23734 The SeaVoX Salt a… SeaVoX S…     54.9      29.7        36.0        -9.50
#> 2 23734 The SeaVoX Salt a… SeaVoX S…     54.9      29.7        36.0        -9.50
#> 3 24174 The SeaVoX Salt a… SeaVoX S…     54.5      26.6        36.0        -9.50
#> 4 24174 The SeaVoX Salt a… SeaVoX S…     54.5      26.6        36.0        -9.50
#> # ℹ 6 more variables: maxLatitude <dbl>, maxLongitude <dbl>,
#> #   preferredGazetteerName <chr>, preferredGazetteerNameLang <chr>,
#> #   status <chr>, accepted <int>
# }
```
