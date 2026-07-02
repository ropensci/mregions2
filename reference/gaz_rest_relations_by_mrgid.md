# Retrieve Gazetter Relations by MRGID

Retrieve Gazetter Relations by MRGID

## Usage

``` r
gaz_rest_relations_by_mrgid(
  mrgid,
  with_geometry = FALSE,
  direction = "both",
  type = "all"
)
```

## Arguments

- mrgid:

  (integer) A valid Marine Regions Gazetteer Identifier
  ([MRGID](https://docs.ropensci.org/mregions2/reference/MRGID.md))

- with_geometry:

  (logical) Add geometries to the result data frame? Default = FALSE

- direction:

  (character) Must be one of upper, lower, both:

  - `upper`: lists all parents of the record.

  - `lower`: lists all childs of the record.

  - `both`: lists parents and childs of the record (default)

- type:

  (character) Must be one of partof, partlypartof, adjacentto,
  similarto, administrativepartof, influencedby, all.

## Value

A data frame with Gazetteer entries

## See also

[List of types (Object
Properties)](https://marineregions.org/ontology/documentation.html),
[gaz_rest](https://docs.ropensci.org/mregions2/reference/gaz_rest.md),
[MRGID](https://docs.ropensci.org/mregions2/reference/MRGID.md)

## Examples

``` r
# \donttest{
gaz_rest_relations_by_mrgid(7378)
#> # A tibble: 7 × 14
#>   MRGID gazetteerSource    placeType latitude longitude minLatitude minLongitude
#>   <int> <chr>              <chr>        <dbl>     <dbl>       <dbl>        <dbl>
#> 1  1908 (1953). Limits of… IHO Sea …    27.0      -169.        0            118.
#> 2  7378 IHO-IOC GEBCO Gaz… Trench       16.7       148.       11            142.
#> 3  8316 Flanders Marine I… EEZ           6.77      150.       -1.17         135.
#> 4  8487 Flanders Marine I… EEZ          29.6       138.       17.1          122.
#> 5  6299 IHO-IOC GEBCO Gaz… Deep         11.4       143.       NA             NA 
#> 6 48957 Flanders Marine I… EEZ          12.9       144.       11.0          141.
#> 7 48980 Flanders Marine I… EEZ          18.3       146.       12.2          141.
#> # ℹ 7 more variables: maxLatitude <dbl>, maxLongitude <dbl>,
#> #   preferredGazetteerName <chr>, preferredGazetteerNameLang <chr>,
#> #   status <chr>, accepted <int>, precision <int>
# }
```
