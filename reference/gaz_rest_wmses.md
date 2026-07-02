# Get WMS information for a given MRGID

Get WMS information for a given MRGID

## Usage

``` r
gaz_rest_wmses(mrgid)
```

## Arguments

- mrgid:

  (integer) A valid Marine Regions Gazetteer Identifier
  ([MRGID](https://docs.ropensci.org/mregions2/reference/MRGID.md))

## Value

a data frame with information from the WMS services including:

- `value`: the value to filter on

- `MRGID` : see
  [MRGID](https://docs.ropensci.org/mregions2/reference/MRGID.md)

- `url`: the base URL of the WMS service

- `namespace`: see
  [`mrp_view()`](https://docs.ropensci.org/mregions2/reference/mrp_view.md)
  details

- `featureType`: see
  [`mrp_view()`](https://docs.ropensci.org/mregions2/reference/mrp_view.md)
  details

- `featureName`: see
  [`mrp_view()`](https://docs.ropensci.org/mregions2/reference/mrp_view.md)
  details

## See also

[gaz_rest](https://docs.ropensci.org/mregions2/reference/gaz_rest.md),
[MRGID](https://docs.ropensci.org/mregions2/reference/MRGID.md),
[`mrp_view()`](https://docs.ropensci.org/mregions2/reference/mrp_view.md)

## Examples

``` r
# \donttest{
gaz_rest_wmses(3293)
#> # A tibble: 1 × 6
#>   value MRGID url                              namespace featureType featureName
#>   <chr> <int> <chr>                            <chr>     <chr>       <chr>      
#> 1 3293   3293 https://geo.vliz.be/geoserver/w… MarineRe… eez         mrgid      
# }
```
