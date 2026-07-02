# Get the names for a given MRGID

Get the names for a given MRGID

## Usage

``` r
gaz_rest_names_by_mrgid(mrgid)
```

## Arguments

- mrgid:

  (integer) A valid Marine Regions Gazetteer Identifier
  ([MRGID](https://docs.ropensci.org/mregions2/reference/MRGID.md))

## Value

a vector with all the names of a Marine Regions Gazetteer entry

## See also

[gaz_rest](https://docs.ropensci.org/mregions2/reference/gaz_rest.md),
[MRGID](https://docs.ropensci.org/mregions2/reference/MRGID.md)

## Examples

``` r
gaz_rest_names_by_mrgid(3293)
#> [1] "Belgian Exclusive Economic Zone" "Belgian Continental Shelf"      
gaz_rest_names_by_mrgid(14)
#> [1] "Belgium" "Bélgica"
```
