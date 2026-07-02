# Retrieve Gazetteer Records by Placetype

Retrieve Gazetteer Records by Placetype

## Usage

``` r
gaz_rest_records_by_type(type, with_geometry = FALSE)
```

## Arguments

- type:

  (character) The placetype from
  [`gaz_rest_types()`](https://docs.ropensci.org/mregions2/reference/gaz_rest_types.md)

- with_geometry:

  (logical) Add geometries to the result data frame? Default = FALSE

## Value

A data frame with Gazetteer entries

## See also

[gaz_rest](https://docs.ropensci.org/mregions2/reference/gaz_rest.md),
[`gaz_rest_types()`](https://docs.ropensci.org/mregions2/reference/gaz_rest_types.md)

## Examples

``` r
# \donttest{
gaz_rest_records_by_type("FAO Subdivisions")
#> # A tibble: 31 × 13
#>    MRGID gazetteerSource   placeType latitude longitude minLatitude minLongitude
#>    <int> <chr>             <chr>        <dbl>     <dbl>       <dbl>        <dbl>
#>  1 48594 FAO Major Fishin… FAO Subd…     36.5     -2.86        36          -5.6 
#>  2 48595 FAO Major Fishin… FAO Subd…     35.9     -3.00        35.8        -3.33
#>  3 48596 FAO Major Fishin… FAO Subd…     35.6     -3.76        35.1        -5.6 
#>  4 48597 FAO Major Fishin… FAO Subd…     37.2      3.16        35.1        -2.22
#>  5 48598 FAO Major Fishin… FAO Subd…     39.2      3.48        38           0.5 
#>  6 48599 FAO Major Fishin… FAO Subd…     40.3      2.35        37          -1   
#>  7 48600 FAO Major Fishin… FAO Subd…     39.8      6.98        38           6   
#>  8 48601 FAO Major Fishin… FAO Subd…     42.6      5.77        41.3         2.95
#>  9 48602 FAO Major Fishin… FAO Subd…     42.4      8.67        41.3         7.63
#> 10 48603 FAO Major Fishin… FAO Subd…     42.7     10.1         42.3         7.52
#> # ℹ 21 more rows
#> # ℹ 6 more variables: maxLatitude <dbl>, maxLongitude <dbl>,
#> #   preferredGazetteerName <chr>, preferredGazetteerNameLang <chr>,
#> #   status <chr>, accepted <int>
gaz_rest_records_by_type("EEZ")
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
