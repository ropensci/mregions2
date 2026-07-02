# Get Gazetteer Records for all given names

Get Gazetteer Records for all given names

## Usage

``` r
gaz_rest_records_by_names(
  names,
  with_geometry = FALSE,
  like = TRUE,
  fuzzy = TRUE
)
```

## Arguments

- names:

  (character) Vector with the terms to search in the Marine Regions
  Gazetteer

- with_geometry:

  (logical) Add geometry to the result data frame? Default = FALSE

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
[gaz_rest_records_by_name](https://docs.ropensci.org/mregions2/reference/gaz_rest_records_by_name.md)

## Examples

``` r
# \donttest{
gaz_rest_records_by_names(
  c("Belgian Exclusive Economic Zone", "Dutch Exclusive Economic Zone")
)
#> # A tibble: 8 × 14
#>   MRGID gazetteerSource    placeType latitude longitude minLatitude minLongitude
#>   <int> <chr>              <chr>        <dbl>     <dbl>       <dbl>        <dbl>
#> 1  3293 Flanders Marine I… EEZ           51.5      2.72        51.0         2.24
#> 2  5668 Flanders Marine I… EEZ           53.6      4.19        51.3         2.54
#> 3 26519 Flanders Marine I… EEZ           13.7    -69.7         12.2       -70.4 
#> 4 26520 Flanders Marine I… EEZ           12.7    -68.4         11.7       -68.7 
#> 5 26517 Flanders Marine I… EEZ           13.2    -69.0         11.7       -69.5 
#> 6 26518 Flanders Marine I… EEZ           17.4    -63.6         16.7       -64.0 
#> 7 26526 Flanders Marine I… EEZ           17.3    -63.1         16.7       -63.6 
#> 8 21803 Flanders Marine I… EEZ           17.9    -63.1         17.8       -63.3 
#> # ℹ 7 more variables: maxLatitude <dbl>, maxLongitude <dbl>, precision <dbl>,
#> #   preferredGazetteerName <chr>, preferredGazetteerNameLang <chr>,
#> #   status <chr>, accepted <int>
# }
```
