# Marine Regions Data Products Ontology

More information available at
[`vignette("mrp_ontology", package = "mregions2")`](https://docs.ropensci.org/mregions2/articles/mrp_ontology.html)

## Usage

``` r
mrp_ontology
```

## Format

### `mrp_ontology`

A data frame with 374 rows and 4 columns:

- layer:

  Identifier of the data product. Use in
  [`mrp_get()`](https://docs.ropensci.org/mregions2/reference/mrp_get.md)

- colname:

  Name of the columns of each data product.

- type:

  Data type of the column.

- definition:

  Definition of the column.

## Source

<https://marineregions.org/sources.php>
<https://marineregions.org/eezattribute.php>

## Examples

``` r
mrp_ontology
#> # A tibble: 374 × 4
#>    layer colname    type   definition                                           
#>    <chr> <chr>      <chr>  <chr>                                                
#>  1 eez   mrgid      int    Marine Regions Geographic Identifier of the feature.…
#>  2 eez   geoname    string Name of the feature.                                 
#>  3 eez   mrgid_ter1 int    Marine Regions Geographic Identifier of the territor…
#>  4 eez   pol_type   string Basis of creation or legal status of feature. One of…
#>  5 eez   mrgid_sov1 int    Marine Regions Geographic Identifier of the sovereig…
#>  6 eez   territory1 string Specific land area which directly relates to the fea…
#>  7 eez   iso_ter1   string ISO 3 code of the territory feature.                 
#>  8 eez   sovereign1 string State that claims jurisdiction over the territory.   
#>  9 eez   mrgid_ter2 int    Marine Regions Geographic Identifier of the territor…
#> 10 eez   mrgid_sov2 int    Marine Regions Geographic Identifier of the sovereig…
#> # ℹ 364 more rows
```
