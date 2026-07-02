# Get the names of the columns and data type of the data product

Get the names of the columns and data type of the data product

## Usage

``` r
mrp_colnames(layer)
```

## Arguments

- layer:

  (character) Identifier of the data product. See
  [mrp_list](https://docs.ropensci.org/mregions2/reference/mrp_list.md)

## Value

A data frame with the column names and data type in the Marine Regions
data product

## Details

This function becomes useful to write CQL or OGC filters that you can
pass to
[`mrp_get()`](https://docs.ropensci.org/mregions2/reference/mrp_get.md)
or
[`mrp_view()`](https://docs.ropensci.org/mregions2/reference/mrp_view.md)
as it allows you to know the column names and the data types beforehand.
Use it together with
[`mrp_col_unique()`](https://docs.ropensci.org/mregions2/reference/mrp_col_unique.md)
to know all the possible values in the column name that you want to
query on.

The actual description of each column is available only to the Maritime
Boundaries products. See <https://marineregions.org/eezattribute.php>

## See also

[mrp_list](https://docs.ropensci.org/mregions2/reference/mrp_list.md) to
describe the list of products,
[`mrp_col_unique()`](https://docs.ropensci.org/mregions2/reference/mrp_col_unique.md)
to get the unique values of a the columns of a data product, useful to
write queries that can be passed to
[`mrp_get()`](https://docs.ropensci.org/mregions2/reference/mrp_get.md)
or
[`mrp_view()`](https://docs.ropensci.org/mregions2/reference/mrp_view.md)
via the arguments `cql_filter` or `filter`.

## Examples

``` r
# \donttest{
mrp_colnames("eez")
#> # A tibble: 31 × 3
#>    layer colname    type  
#>  * <chr> <chr>      <chr> 
#>  1 eez   mrgid      int   
#>  2 eez   geoname    string
#>  3 eez   mrgid_ter1 int   
#>  4 eez   pol_type   string
#>  5 eez   mrgid_sov1 int   
#>  6 eez   territory1 string
#>  7 eez   iso_ter1   string
#>  8 eez   sovereign1 string
#>  9 eez   mrgid_ter2 int   
#> 10 eez   mrgid_sov2 int   
#> # ℹ 21 more rows
mrp_colnames("ecoregions")
#> # A tibble: 6 × 3
#>   layer      colname   type   
#> * <chr>      <chr>     <chr>  
#> 1 ecoregions eco_code  long   
#> 2 ecoregions ecoregion string 
#> 3 ecoregions lat       decimal
#> 4 ecoregions long      decimal
#> 5 ecoregions placetype string 
#> 6 ecoregions mrgid     int    
# }
```
