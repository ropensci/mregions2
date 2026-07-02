# Get all gazetteer records where the geometry intersects with the given latitude and longitude

Get all gazetteer records where the geometry intersects with the given
latitude and longitude

## Usage

``` r
gaz_rest_records_by_lat_long(
  latitude,
  longitude,
  with_geometry = FALSE,
  typeid = NULL
)
```

## Arguments

- latitude:

  (double) A decimal number which ranges from -90 to 90. Coordinates are
  assumed to be in WGS84

- longitude:

  (double) A decimal number which ranges from -180 to 180. Coordinates
  are assumed to be in WGS84

- with_geometry:

  (logical) Add geometries to the result data frame? Default = FALSE

- typeid:

  (numeric) Restrict to one or more placetypeIDs. Retrieve a list of
  placetypeIDs with
  [`gaz_rest_types()`](https://docs.ropensci.org/mregions2/reference/gaz_rest_types.md)

## Value

A data frame with Gazetteer entries

## See also

[gaz_rest](https://docs.ropensci.org/mregions2/reference/gaz_rest.md)

## Examples

``` r
# \donttest{
gaz_rest_records_by_lat_long(51.21551, 2.927)
#> # A tibble: 61 × 14
#>    MRGID gazetteerSource   placeType latitude longitude minLatitude minLongitude
#>    <int> <chr>             <chr>        <dbl>     <dbl>       <dbl>        <dbl>
#>  1    14 (2001). The Time… Nation        50.5      4.48        49.5         2.55
#>  2    14 (2001). The Time… Nation        50.5      4.48        49.5         2.55
#>  3    14 (2001). The Time… Nation        50.5      4.48        49.5         2.55
#>  4    14 (2001). The Time… Nation        50.5      4.48        49.5         2.55
#>  5    14 (2001). The Time… Nation        50.5      4.48        49.5         2.55
#>  6    14 (2001). The Time… Nation        50.5      4.48        49.5         2.55
#>  7    20 SAIL              Province…     51.0      3.03        50.7         2.55
#>  8    20 SAIL              Province…     51.0      3.03        50.7         2.55
#>  9    20 SAIL              Province…     51.0      3.03        50.7         2.55
#> 10    20 SAIL              Province…     51.0      3.03        50.7         2.55
#> # ℹ 51 more rows
#> # ℹ 7 more variables: maxLatitude <dbl>, maxLongitude <dbl>,
#> #   preferredGazetteerName <chr>, preferredGazetteerNameLang <chr>,
#> #   status <chr>, accepted <int>, precision <dbl>
gaz_rest_records_by_lat_long(51.21551, 2.927,
                             with_geometry = TRUE,
                             typeid = c(255, 259))
#> Simple feature collection with 8 features and 13 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -9.500575 ymin: 36.00045 xmax: 67.52637 ymax: 71.1311
#> Geodetic CRS:  WGS 84
#> # A tibble: 8 × 14
#>   MRGID gazetteerSource    placeType latitude longitude minLatitude minLongitude
#>   <int> <chr>              <chr>        <dbl>     <dbl>       <dbl>        <dbl>
#> 1 23734 The SeaVoX Salt a… SeaVoX S…     54.9      29.7        36.0        -9.50
#> 2 23734 The SeaVoX Salt a… SeaVoX S…     54.9      29.7        36.0        -9.50
#> 3 23734 The SeaVoX Salt a… SeaVoX S…     54.9      29.7        36.0        -9.50
#> 4 23734 The SeaVoX Salt a… SeaVoX S…     54.9      29.7        36.0        -9.50
#> 5 24174 The SeaVoX Salt a… SeaVoX S…     54.5      26.6        36.0        -9.50
#> 6 24174 The SeaVoX Salt a… SeaVoX S…     54.5      26.6        36.0        -9.50
#> 7 24174 The SeaVoX Salt a… SeaVoX S…     54.5      26.6        36.0        -9.50
#> 8 24174 The SeaVoX Salt a… SeaVoX S…     54.5      26.6        36.0        -9.50
#> # ℹ 7 more variables: maxLatitude <dbl>, maxLongitude <dbl>,
#> #   preferredGazetteerName <chr>, preferredGazetteerNameLang <chr>,
#> #   status <chr>, accepted <int>, the_geom <MULTIPOLYGON [°]>
# }
```
