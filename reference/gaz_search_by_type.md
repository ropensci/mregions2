# Retrieve Gazetteer Records by Placetype

Retrieve Gazetteer Records by Placetype

## Usage

``` r
gaz_search_by_type(x, ...)

# S3 method for class 'character'
gaz_search_by_type(x, ...)

# S3 method for class 'numeric'
gaz_search_by_type(x, ...)
```

## Arguments

- x:

  A [place
  type](https://docs.ropensci.org/mregions2/reference/gaz_types.md).
  Either:

  - (character) The name of a place type.

  - (integer) The typeid of a place type.

- ...:

  Arguments passed on to
  [`gaz_rest_records_by_type`](https://docs.ropensci.org/mregions2/reference/gaz_rest_records_by_type.md)

  `type`

  :   (character) The placetype from
      [`gaz_rest_types()`](https://docs.ropensci.org/mregions2/reference/gaz_rest_types.md)

  `with_geometry`

  :   (logical) Add geometries to the result data frame? Default = FALSE

## Value

A data frame with Gazetteer entries

## See also

[`gaz_types()`](https://docs.ropensci.org/mregions2/reference/gaz_types.md)

## Examples

``` r
# \donttest{
# This
gaz_search_by_type("EEZ")
#> # A tibble: 246 × 14
#>    MRGID gazetteerSource   placeType latitude longitude minLatitude minLongitude
#>    <int> <chr>             <chr>        <dbl>     <dbl>       <dbl>        <dbl>
#>  1  3293 Flanders Marine … EEZ           51.5      2.72        51.0         2.24
#>  2  5668 Flanders Marine … EEZ           53.6      4.19        51.3         2.54
#>  3  5669 Flanders Marine … EEZ           54.6      8.40        52.9         3.35
#>  4  5670 Flanders Marine … EEZ           40.9     19.1         39.6        18.3 
#>  5  5672 Flanders Marine … EEZ           42.9     29.2         42.0        27.4 
#>  6  5673 Flanders Marine … EEZ           43.4     15.6         41.6        13.0 
#>  7  5674 Flanders Marine … EEZ           56.2      9.21        54.4         3.25
#>  8  5675 Flanders Marine … EEZ           58.8     23.0         57.6        20.4 
#>  9  5676 Flanders Marine … EEZ           61.8     21.9         58.8        19.1 
#> 10  5677 Flanders Marine … EEZ           46.1     -1.96        41.2        -9.88
#> # ℹ 236 more rows
#> # ℹ 7 more variables: maxLatitude <dbl>, maxLongitude <dbl>, precision <dbl>,
#> #   preferredGazetteerName <chr>, preferredGazetteerNameLang <chr>,
#> #   status <chr>, accepted <int>

# is the same as
gaz_search_by_type(70)
#> # A tibble: 246 × 14
#>    MRGID gazetteerSource   placeType latitude longitude minLatitude minLongitude
#>    <int> <chr>             <chr>        <dbl>     <dbl>       <dbl>        <dbl>
#>  1  3293 Flanders Marine … EEZ           51.5      2.72        51.0         2.24
#>  2  5668 Flanders Marine … EEZ           53.6      4.19        51.3         2.54
#>  3  5669 Flanders Marine … EEZ           54.6      8.40        52.9         3.35
#>  4  5670 Flanders Marine … EEZ           40.9     19.1         39.6        18.3 
#>  5  5672 Flanders Marine … EEZ           42.9     29.2         42.0        27.4 
#>  6  5673 Flanders Marine … EEZ           43.4     15.6         41.6        13.0 
#>  7  5674 Flanders Marine … EEZ           56.2      9.21        54.4         3.25
#>  8  5675 Flanders Marine … EEZ           58.8     23.0         57.6        20.4 
#>  9  5676 Flanders Marine … EEZ           61.8     21.9         58.8        19.1 
#> 10  5677 Flanders Marine … EEZ           46.1     -1.96        41.2        -9.88
#> # ℹ 236 more rows
#> # ℹ 7 more variables: maxLatitude <dbl>, maxLongitude <dbl>, precision <dbl>,
#> #   preferredGazetteerName <chr>, preferredGazetteerNameLang <chr>,
#> #   status <chr>, accepted <int>
# }
```
