# Get all the Marine Regions sources

Get all the Marine Regions sources

## Usage

``` r
gaz_rest_sources()
```

## Value

a data frame with three columns:

- `sourceID`: the identifier of the source in the Marine Regions
  Gazetteer database.

- `source`: the name of the source.

- `sourceURL`: if available, the URL of the source.

## Details

gaz_search() is a memoised function from gaz_rest_search(). See
[`memoise::memoise()`](https://memoise.r-lib.org/reference/memoise.html).

## See also

[gaz_rest](https://docs.ropensci.org/mregions2/reference/gaz_rest.md),
[`gaz_search_by_source()`](https://docs.ropensci.org/mregions2/reference/gaz_search_by_source.md),
[`gaz_rest_records_by_source()`](https://docs.ropensci.org/mregions2/reference/gaz_rest_records_by_source.md),
[`gaz_rest_source_by_sourceid()`](https://docs.ropensci.org/mregions2/reference/gaz_rest_source_by_sourceid.md)

## Examples

``` r
# \donttest{
# This
gaz_rest_sources()
#> # A tibble: 724 × 3
#>    sourceID source                                                     sourceURL
#>       <int> <chr>                                                      <chr>    
#>  1        1 SAIL                                                       http://w…
#>  2        2 ASFA thesaurus                                             http://w…
#>  3        4 Nationaal Instituut voor de Statistiek (NIS) / National S… http://s…
#>  4        5 Oosthoek Times Wereldatlas                                 NA       
#>  5        6 Lausch, E. (2000). Atlas van de oceanen: met de dieptekaa… NA       
#>  6        7 (2001). The Times comprehensive atlas of the world. 10th … NA       
#>  7        8 IHO-IOC GEBCO Gazetteer of Undersea Feature Names (2002-1… http://w…
#>  8        9 BIOMARE                                                    http://w…
#>  9       10 (1953). Limits of oceans and seas. 3rd edition. IHO Speci… NA       
#> 10       11 Nomenclature des espaces maritimes/List of maritime areas  NA       
#> # ℹ 714 more rows

# is the same as
gaz_sources()
#> # A tibble: 724 × 3
#>    sourceID source                                                     sourceURL
#>       <int> <chr>                                                      <chr>    
#>  1        1 SAIL                                                       http://w…
#>  2        2 ASFA thesaurus                                             http://w…
#>  3        4 Nationaal Instituut voor de Statistiek (NIS) / National S… http://s…
#>  4        5 Oosthoek Times Wereldatlas                                 NA       
#>  5        6 Lausch, E. (2000). Atlas van de oceanen: met de dieptekaa… NA       
#>  6        7 (2001). The Times comprehensive atlas of the world. 10th … NA       
#>  7        8 IHO-IOC GEBCO Gazetteer of Undersea Feature Names (2002-1… http://w…
#>  8        9 BIOMARE                                                    http://w…
#>  9       10 (1953). Limits of oceans and seas. 3rd edition. IHO Speci… NA       
#> 10       11 Nomenclature des espaces maritimes/List of maritime areas  NA       
#> # ℹ 714 more rows
# }
```
