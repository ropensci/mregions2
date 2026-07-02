# Get all the possible values of a column of a Marine Regions data product

Get all the possible values of a column of a Marine Regions data product

## Usage

``` r
mrp_col_unique(layer, colname)

mrp_col_distinct(layer, colname)
```

## Arguments

- layer:

  (character) Identifier of the data product. See
  [mrp_list](https://docs.ropensci.org/mregions2/reference/mrp_list.md)

- colname:

  (character) Column name in the data product. See
  [`mrp_colnames()`](https://docs.ropensci.org/mregions2/reference/mrp_colnames.md)

## Value

A numeric or character vector with the unique values of a column of a
Marine Regions data product.

## Details

This function becomes useful to write CQL or OGC filters that you can
pass to
[`mrp_get()`](https://docs.ropensci.org/mregions2/reference/mrp_get.md)
or
[`mrp_view()`](https://docs.ropensci.org/mregions2/reference/mrp_view.md)
as it helps to know all the possible values in the column name that you
want to query on beforehand. Use it together with
[`mrp_colnames()`](https://docs.ropensci.org/mregions2/reference/mrp_colnames.md)
to know the columns and data types in the data product.

### Geometry columns

Note that columns of type `geometry` are forbidden as their performance
is sub-optimal and would likely crash your R session.

## See also

[mrp_list](https://docs.ropensci.org/mregions2/reference/mrp_list.md) to
describe the list of products,
[`mrp_colnames()`](https://docs.ropensci.org/mregions2/reference/mrp_colnames.md)
to get the names and data type of the columns of a data product, useful
to write queries that can be passed to
[`mrp_get()`](https://docs.ropensci.org/mregions2/reference/mrp_get.md)
or
[`mrp_view()`](https://docs.ropensci.org/mregions2/reference/mrp_view.md)
via the arguments `cql_filter` or `filter`.

## Examples

``` r
# \donttest{
mrp_col_unique("ecs", "pol_type")
#> [1] "ECS CLCS Recommendation"       "ECS CLCS Submission"          
#> [3] "ECS DOALOS Deposit"            "ECS Joint CLCS Recommendation"
#> [5] "ECS Joint CLCS Submission"     "ECS Joint DOALOS Deposit"     
#> [7] "ECS Non-UNCLOS Claim"          "ECS Overlapping claim"        
mrp_col_unique("ecs_boundaries", "line_type")
#> [1] "ECS CLCS Joint Recommendation" "ECS CLCS Joint Submission"    
#> [3] "ECS CLCS Recommendation"       "ECS CLCS Submission"          
#> [5] "ECS Connection Line"           "ECS DOALOS Deposit"           
#> [7] "ECS DOALOS Joint Deposit"      "ECS Non-UNCLOS Claim"         
#> [9] "ECS Treaty"                   
# }
```
