# Get the name of a source by providing a sourceID

Get the name of a source by providing a sourceID

## Usage

``` r
gaz_rest_source_by_sourceid(sourceid)
```

## Arguments

- sourceid:

  (integer) A valid sourceID

## Value

a named vector with the source name and, if available, the url to the
source.

## See also

[gaz_rest](https://docs.ropensci.org/mregions2/reference/gaz_rest.md),
[`gaz_sources()`](https://docs.ropensci.org/mregions2/reference/gaz_sources.md)

## Examples

``` r
# \donttest{
gaz_rest_source_by_sourceid(390)
#>                                                                                                                                                                                                       source 
#> "Flanders Marine Institute (2018). Maritime Boundaries Geodatabase: Maritime Boundaries and Exclusive Economic Zones (200NM). Available online at http://www.marineregions.org https://doi.org/10.14284/312" 
#>                                                                                                                                                                                                          url 
#>                                                                                                                                                                               "http://www.marineregions.org" 
gaz_rest_source_by_sourceid(657)
#>                                              source 
#> "IHO-IOC GEBCO Gazetteer of Undersea Feature Names" 
#>                                                 url 
#>               "http://www.ngdc.noaa.gov/gazetteer/" 
# }
```
