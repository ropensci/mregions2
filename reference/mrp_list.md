# Available data products at Marine Regions

A data frame including the name, abstract and some other relevant about
each data product in Marine Regions.

## Usage

``` r
mrp_list
```

## Format

### `mrp_list`

A data frame with 21 rows and 7 columns:

- title:

  Data product name

- namespace:

  Workspace in geoserver

- layer:

  Identifier of the data product. Use in
  [`mrp_get()`](https://docs.ropensci.org/mregions2/reference/mrp_get.md)

- license:

  terms of use of the data products

- citation:

  preferred citation of the data products

- doi:

  ISO 26324 [Digital Object Identifier](https://www.doi.org/))

- imis:

  Url of the data products in the [Integrated Marine Information System
  (IMIS)](https://vliz.be/en/imis)

- abstract:

  Description of the data product

## Source

<https://marineregions.org/sources.php>
<https://marineregions.org/eezmethodology.php>

## Details

Direct downloads are also available at:
<https://marineregions.org/downloads.php>

## Examples

``` r
mrp_list
#> # A tibble: 21 × 8
#>    title                   namespace layer license citation doi   imis  abstract
#>    <chr>                   <chr>     <chr> <chr>   <chr>    <chr> <chr> <chr>   
#>  1 Exclusive Economic Zon… MarineRe… eez   Creati… "Flande… http… http… "Versio…
#>  2 Maritime Boundaries (v… MarineRe… eez_… Creati… "Flande… http… http… "Versio…
#>  3 Territorial Seas (12 N… MarineRe… eez_… Creati… "Flande… http… http… "Versio…
#>  4 Contiguous Zones (24 N… MarineRe… eez_… Creati… "Flande… http… http… "Versio…
#>  5 Internal Waters (v3, w… MarineRe… eez_… Creati… "Flande… http… http… "Versio…
#>  6 Archipelagic Waters (v… MarineRe… eez_… Creati… "Flande… http… http… "Versio…
#>  7 High Seas (v1, world, … MarineRe… high… Creati… "Flande… http… http… "High S…
#>  8 Extended Continental S… MarineRe… ecs   Creati… "Flande… http… http… "This d…
#>  9 Extended Continental S… MarineRe… ecs_… Creati… "Flande… http… http… "This d…
#> 10 IHO Sea Areas (v3)      MarineRe… iho   Creati… "Flande… http… http… "World …
#> # ℹ 11 more rows
```
