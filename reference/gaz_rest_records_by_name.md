# Get Gazetteer Records for a given name

Get Gazetteer Records for a given name

## Usage

``` r
gaz_rest_records_by_name(
  name,
  with_geometry = FALSE,
  typeid = NULL,
  language = NULL,
  like = TRUE,
  fuzzy = TRUE
)
```

## Arguments

- name:

  (character) Term to search in the Marine Regions Gazetteer

- with_geometry:

  (logical) Add geometry to the result data frame? Default = FALSE

- typeid:

  (numeric) Restrict to one or more placetypeIDs. Retrieve a list of
  placetypeIDs with
  [`gaz_rest_types()`](https://docs.ropensci.org/mregions2/reference/gaz_rest_types.md)

- language:

  (character) Restrict to one language. Provide as a 2 digits ISO-639.
  See ISOcodes::ISO_639_2.

- like:

  (logical) Add a '%'-sign before and after the name? (SQL LIKE
  function). Default = TRUE

- fuzzy:

  (logical) Use Levenshtein query to find nearest matches? Default =
  TRUE

## Value

A data frame with Gazetteer entries

## See also

[gaz_rest](https://docs.ropensci.org/mregions2/reference/gaz_rest.md),
gaz_rest_records_by_name

## Examples

``` r
# \donttest{
gaz_rest_records_by_name("Belgian Exclusive Economic Zone", with_geometry = TRUE)
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
gaz_rest_records_by_name("Bélgica", language = "es")
#> # A tibble: 1 × 14
#>   MRGID gazetteerSource    placeType latitude longitude minLatitude minLongitude
#>   <int> <chr>              <chr>        <dbl>     <dbl>       <dbl>        <dbl>
#> 1    14 (2001). The Times… Nation        50.5      4.48        49.5         2.55
#> # ℹ 7 more variables: maxLatitude <dbl>, maxLongitude <dbl>, precision <dbl>,
#> #   preferredGazetteerName <chr>, preferredGazetteerNameLang <chr>,
#> #   status <chr>, accepted <int>
gaz_rest_records_by_name("Belgium", typeid = c(350, 351))
#> # A tibble: 2 × 13
#>   MRGID gazetteerSource    placeType latitude longitude minLatitude minLongitude
#>   <int> <chr>              <chr>        <dbl>     <dbl>       <dbl>        <dbl>
#> 1 47551 TDWG - World Geog… TDWG Ter…     50.6      4.78        49.4         2.54
#> 2 47978 TDWG - World Geog… TDWG Ter…     50.6      4.67        49.5         2.54
#> # ℹ 6 more variables: maxLatitude <dbl>, maxLongitude <dbl>,
#> #   preferredGazetteerName <chr>, preferredGazetteerNameLang <chr>,
#> #   status <chr>, accepted <int>
# }
```
