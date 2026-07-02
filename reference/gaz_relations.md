# Walk the hierarchy of the MarineRegions Gazetter given a Gazetteer MRGID or Gazetteer entries

Walk the hierarchy of the MarineRegions Gazetter given a Gazetteer MRGID
or Gazetteer entries

## Usage

``` r
gaz_relations(x, ...)

# S3 method for class 'numeric'
gaz_relations(x, ...)

# S3 method for class 'mr_df'
gaz_relations(x, ...)
```

## Arguments

- x:

  the object from which the relations are retrieved. Can be:

  - (integer) A valid Marine Regions Gazetteer Identifier
    ([MRGID](https://docs.ropensci.org/mregions2/reference/MRGID.md)),
    passed to
    [`gaz_rest_relations_by_mrgid()`](https://docs.ropensci.org/mregions2/reference/gaz_rest_relations_by_mrgid.md)

  - A data frame retrieved with
    [mregions2](https://docs.ropensci.org/mregions2/reference/mregions2-package.md)
    via its functions
    [`gaz_search()`](https://docs.ropensci.org/mregions2/reference/gaz_search.md),
    [`gaz_search_by_source()`](https://docs.ropensci.org/mregions2/reference/gaz_search_by_source.md)
    or
    [`gaz_search_by_type()`](https://docs.ropensci.org/mregions2/reference/gaz_search_by_type.md).

- ...:

  Arguments passed on to
  [`gaz_rest_relations_by_mrgid`](https://docs.ropensci.org/mregions2/reference/gaz_rest_relations_by_mrgid.md)

  `with_geometry`

  :   (logical) Add geometries to the result data frame? Default = FALSE

  `direction`

  :   (character) Must be one of upper, lower, both:

      - `upper`: lists all parents of the record.

      - `lower`: lists all childs of the record.

      - `both`: lists parents and childs of the record (default)

  `type`

  :   (character) Must be one of partof, partlypartof, adjacentto,
      similarto, administrativepartof, influencedby, all.

## Value

A data frame with Gazetteer entries

## Details

You can pass the output of most `gaz_*` functions to `gaz_relations()`
to retrieve the related gazetteer entries

### Developer info

This is done in the method `gaz_relations.mr_df()`. `mr_df` is a class
defined in this package to ensure the data frame passed to gaz_relations
has a variable with
[MRGID](https://docs.ropensci.org/mregions2/reference/MRGID.md).

## Examples

``` r
# \donttest{
# Get the relations of the Belgian Exclusive Economic Zone
gaz_search("Belgian Exclusive Economic Zone") %>% gaz_relations()
#> # A tibble: 37 × 14
#>    MRGID gazetteerSource   placeType latitude longitude minLatitude minLongitude
#>    <int> <chr>             <chr>        <dbl>     <dbl>       <dbl>        <dbl>
#>  1  3293 Flanders Marine … EEZ           51.5      2.72        51.0         2.24
#>  2  2419 ASFA thesaurus    Sandbank…     51.5      2.58        NA          NA   
#>  3  2420 ASFA thesaurus    Sandbank…     51.5      2.88        NA          NA   
#>  4  2421 ASFA thesaurus    Sandbank…     51.3      2.64        NA          NA   
#>  5    14 (2001). The Time… Nation        50.5      4.48        49.5         2.55
#>  6  2350 (1953). Limits o… IHO Sea …     56.4      2.74        51.0        -4.45
#>  7  4675 Cattrijsse, A.; … Sandbank…     51.3      2.98        NA          NA   
#>  8  2550 ASFA thesaurus    Coast         51.2      2.91        51.1         2.54
#>  9 17401 Aphia             Wreck         51.4      2.32        NA          NA   
#> 10 17409 Aphia             Wreck         51.1      2.33        NA          NA   
#> # ℹ 27 more rows
#> # ℹ 7 more variables: maxLatitude <dbl>, maxLongitude <dbl>, precision <dbl>,
#> #   preferredGazetteerName <chr>, preferredGazetteerNameLang <chr>,
#> #   status <chr>, accepted <int>

# Or using its mrgid
gaz_relations(3293)
#> # A tibble: 37 × 14
#>    MRGID gazetteerSource   placeType latitude longitude minLatitude minLongitude
#>    <int> <chr>             <chr>        <dbl>     <dbl>       <dbl>        <dbl>
#>  1  3293 Flanders Marine … EEZ           51.5      2.72        51.0         2.24
#>  2  2419 ASFA thesaurus    Sandbank…     51.5      2.58        NA          NA   
#>  3  2420 ASFA thesaurus    Sandbank…     51.5      2.88        NA          NA   
#>  4  2421 ASFA thesaurus    Sandbank…     51.3      2.64        NA          NA   
#>  5    14 (2001). The Time… Nation        50.5      4.48        49.5         2.55
#>  6  2350 (1953). Limits o… IHO Sea …     56.4      2.74        51.0        -4.45
#>  7  4675 Cattrijsse, A.; … Sandbank…     51.3      2.98        NA          NA   
#>  8  2550 ASFA thesaurus    Coast         51.2      2.91        51.1         2.54
#>  9 17401 Aphia             Wreck         51.4      2.32        NA          NA   
#> 10 17409 Aphia             Wreck         51.1      2.33        NA          NA   
#> # ℹ 27 more rows
#> # ℹ 7 more variables: maxLatitude <dbl>, maxLongitude <dbl>, precision <dbl>,
#> #   preferredGazetteerName <chr>, preferredGazetteerNameLang <chr>,
#> #   status <chr>, accepted <int>
# }
```
