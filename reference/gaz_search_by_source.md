# Retrieve Gazetteer Records by Source

Retrieve Gazetteer Records by Source

## Usage

``` r
gaz_search_by_source(x, ...)

# S3 method for class 'character'
gaz_search_by_source(x, ...)

# S3 method for class 'numeric'
gaz_search_by_source(x, ...)
```

## Arguments

- x:

  source as free text or `sourceID` as integer

- ...:

  Arguments passed on to
  [`gaz_rest_records_by_source`](https://docs.ropensci.org/mregions2/reference/gaz_rest_records_by_source.md)

  `with_geometry`

  :   (logical) Add geometries to the result data frame? Default = FALSE

## Value

A data frame with Gazetteer entries

## See also

[`gaz_sources()`](https://docs.ropensci.org/mregions2/reference/gaz_sources.md)

## Examples

``` r
# \donttest{
# Check out all sources
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

# Look up by source name
gaz_search_by_source("Gazetteer of Greenland")
#> # A tibble: 65 × 9
#>    MRGID gazetteerSource     placeType latitude longitude preferredGazetteerName
#>    <int> <chr>               <chr>        <dbl>     <dbl> <chr>                 
#>  1 48707 Gazetteer of Green… Fjord         63.7     -41.0 Bernstorffs Isfjord   
#>  2 48708 Gazetteer of Green… Fjord         65.5     -39.1 Ikertivaq             
#>  3 48710 Gazetteer of Green… Fjord         60.9     -43.1 Iluileq               
#>  4 48711 Gazetteer of Green… Fjord         60.5     -43.1 Kangerlussuatsiaq     
#>  5 48720 Gazetteer of Green… Fjord         66.4     -35.8 Kangertittivatsiaq    
#>  6 48721 Gazetteer of Green… Fjord         66       -37.9 Sermilik              
#>  7 48722 Gazetteer of Green… Fjord         64.0     -41   Umiiviip Kangertiva   
#>  8 48723 Gazetteer of Green… Fjord         81       -22.0 Danmarksfjorden       
#>  9 48724 Gazetteer of Green… Fjord         83.1     -31.2 Frederick E. Hyde Fjo…
#> 10 48725 Gazetteer of Green… Fjord         82.8     -25   G. B. Schley Fjord    
#> # ℹ 55 more rows
#> # ℹ 3 more variables: preferredGazetteerNameLang <chr>, status <chr>,
#> #   accepted <int>

# Or query by SourceID
gaz_search_by_source(386)
#> # A tibble: 65 × 9
#>    MRGID gazetteerSource     placeType latitude longitude preferredGazetteerName
#>    <int> <chr>               <chr>        <dbl>     <dbl> <chr>                 
#>  1 48707 Gazetteer of Green… Fjord         63.7     -41.0 Bernstorffs Isfjord   
#>  2 48708 Gazetteer of Green… Fjord         65.5     -39.1 Ikertivaq             
#>  3 48710 Gazetteer of Green… Fjord         60.9     -43.1 Iluileq               
#>  4 48711 Gazetteer of Green… Fjord         60.5     -43.1 Kangerlussuatsiaq     
#>  5 48720 Gazetteer of Green… Fjord         66.4     -35.8 Kangertittivatsiaq    
#>  6 48721 Gazetteer of Green… Fjord         66       -37.9 Sermilik              
#>  7 48722 Gazetteer of Green… Fjord         64.0     -41   Umiiviip Kangertiva   
#>  8 48723 Gazetteer of Green… Fjord         81       -22.0 Danmarksfjorden       
#>  9 48724 Gazetteer of Green… Fjord         83.1     -31.2 Frederick E. Hyde Fjo…
#> 10 48725 Gazetteer of Green… Fjord         82.8     -25   G. B. Schley Fjord    
#> # ℹ 55 more rows
#> # ℹ 3 more variables: preferredGazetteerNameLang <chr>, status <chr>,
#> #   accepted <int>
# }
```
