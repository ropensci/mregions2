# Retrieve Gazetteer Records by Source

Retrieve Gazetteer Records by Source

## Usage

``` r
gaz_rest_records_by_source(source, with_geometry = FALSE)
```

## Arguments

- source:

  (character) A source from
  [`gaz_rest_sources()`](https://docs.ropensci.org/mregions2/reference/gaz_rest_sources.md)

- with_geometry:

  (logical) Add geometries to the result data frame? Default = FALSE

## Value

A data frame with Gazetteer entries

## See also

[gaz_rest](https://docs.ropensci.org/mregions2/reference/gaz_rest.md)

## Examples

``` r
# \donttest{
gaz_rest_records_by_source("ICES Ecoregions")
#> # A tibble: 18 × 13
#>    MRGID gazetteerSource placeType   latitude longitude minLatitude minLongitude
#>    <int> <chr>           <chr>          <dbl>     <dbl>       <dbl>        <dbl>
#>  1 22161 ICES Ecoregions ICES Ecore…     61.5     -9.58        60          -15  
#>  2 22162 ICES Ecoregions ICES Ecore…     75.3     37.7         63.8          5  
#>  3 22164 ICES Ecoregions ICES Ecore…     68.6      2.17        62          -11  
#>  4 22166 ICES Ecoregions ICES Ecore…     48.1    -27.3         36          -42  
#>  5 22167 ICES Ecoregions ICES Ecore…     39.6      6.28        35.1         -5.6
#>  6 22169 ICES Ecoregions ICES Ecore…     35.0     28.2         30.8         21.0
#>  7 22170 ICES Ecoregions ICES Ecore…     59.1     20.0         53.6         12.0
#>  8 22172 ICES Ecoregions ICES Ecore…     43.5     34.4         40.9         27.4
#>  9 22173 ICES Ecoregions ICES Ecore…     54.2    -10.5         48          -18  
#> 10 22540 ICES Ecoregions ICES Ecore…     NA       NA           30          -45  
#> 11 36310 ICES Ecoregions ICES Ecore…     69.7    -28.1         NA           NA  
#> 12 36311 ICES Ecoregions ICES Ecore…     42.7     -8.89        NA           NA  
#> 13 36312 ICES Ecoregions ICES Ecore…     39.2    -28.4         NA           NA  
#> 14 36313 ICES Ecoregions ICES Ecore…     35.2     17.3         NA           NA  
#> 15 36314 ICES Ecoregions ICES Ecore…     42.7     16.1         NA           NA  
#> 16 36315 ICES Ecoregions ICES Ecore…     85.7     11.2         80.6        -40  
#> 17 36316 ICES Ecoregions ICES Ecore…     64.6    -18.9         NA           NA  
#> 18 36317 ICES Ecoregions ICES Ecore…     56.0      2.93        48           -5  
#> # ℹ 6 more variables: maxLatitude <dbl>, maxLongitude <dbl>,
#> #   preferredGazetteerName <chr>, preferredGazetteerNameLang <chr>,
#> #   status <chr>, accepted <int>
# }
```
