
# mregions2

<!-- badges: start -->
<!-- badges: end -->

mregions2 supersedes [mregions](https://github.com/ropensci/mregions).
It aims to:

-   Retrieve marine geospatial information from the [Marineregions
    gazetteer](https://marineregions.org/gazetteer.php?p=webservices).
-   Access and handle data products from the [Flanders Marine Institute
    (VLIZ)
    geoserver](http://geo.vliz.be/geoserver/web/wicket/bookmarkable/org.geoserver.web.demo.MapPreviewPage?1).

## Installation

You can install the development version of mregions2 from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("lifewatch/mregions2")
```

## Examples

``` r
library(mregions2)

# helper library
library(mapview)
```

For more information about a function, type `?<function_name>` in the R
console. This opens the function’s documentation.

### `mr_gaz_records_by_names()`

This function retrieves records from the [Marine
Gazetteer](https://marineregions.org/gazetteer.php?p=search) by
inputting one or more search terms, i.e. `"North Sea"` or `"Belgium"`.
The result is a [`tibble`](https://tibble.tidyverse.org) with all
records that contain one of the search terms in their
`preferredGazetteerName`.

``` r
north_sea <- mr_gaz_records_by_names("North Sea")

head(north_sea$preferredGazetteerName)
#> [1] "Belgian part of the North Sea"            
#> [2] "Central North Sea"                        
#> [3] "Danish part of the North Sea"             
#> [4] "Dutch part of the North Sea"              
#> [5] "EMODnet-Biology reporting area: North Sea"
#> [6] "French part of Greater North Sea"
```

<!-- When looking for the Belgian Part of the North Sea, for example, searching for `c("North Sea","Belgian")` will result in all records that contain one of the two terms. -->
<!-- ```{r example2 mr_gaz_records_by_names()} -->
<!-- northsea_belgian <- mr_gaz_records_by_names(c("North Sea", "Belgian")) -->
<!-- # Inspect all record names that were found -->
<!-- northsea_belgian$preferredGazetteerName -->
<!-- ``` -->

Records can have the same name, however, the are always distinguishable
by their [MRGID](https://marineregions.org/mrgid.php). Let’s look a bit
closer at the records named `North Sea`.

``` r
# north_sea_name <- north_sea[north_sea$preferredGazetteerName == "North Sea", ]
north_sea_name <- subset(north_sea, preferredGazetteerName == "North Sea")
# View(north_sea_name)
```

The 6 records with the same `preferredGazetteerName == "North Sea"`
differ in their `Placetype`. The correct record to choose depends on the
goal of the analysis. The status of the record can, however, help decide
which one to choose. For example, the `status` of the 6 records in
`north_sea_name` is either `standard`, `synonym` or `deleted`. In this
case, `North Sea` with `Placetype == "IHO Sea Area"`, that has the
`standard` status, should be preferred.
<!-- provide link to further info about the status names -->

``` r
north_sea_name$placeType
#> [1] "IHO Sea Area"                        
#> [2] "Large Marine Ecosystem"              
#> [3] "Marine Ecoregion of the World (MEOW)"
#> [4] "ICES Ecoregion"                      
#> [5] "SeaVoX SeaArea - level 3"            
#> [6] "SeaVoX SeaArea - sub-region"
north_sea_name$status
#> [1] "standard" "synonym"  "synonym"  "deleted"  "synonym"  "synonym"
```

### `mr_gaz_record()`

This function retrieves a geometry from the [VLIZ
geoserver](http://geo.vliz.be/geoserver/web/wicket/bookmarkable/org.geoserver.web.demo.MapPreviewPage?1).
The geometry is a geospatial object of `class(sf)` (simple feature) from
the `sf` R package. Here you can find an
[introduction](https://r-spatial.github.io/sf/) to
`simple feature objects` in `sf` and an
[overview](https://r-spatial.github.io/sf/reference/index.html) of all
available functions.

In the following example, the geometry of the North Sea IHO Sea Area is
retrieved and visualised.

``` r
north_sea_IHO <- subset(north_sea, placeType == "IHO Sea Area" & preferredGazetteerName == "North Sea")
mrgid <- north_sea_IHO$MRGID

# check the outcome when a geom is there and when not
# IHO_geom <- mr_gaz_record(mrgid)
# colnames(IHO_geom)
# 
# mapview(IHO_geom)
```

### `mr_gaz_info()`

This function lists information about the [Marine
Gazetteer](https://marineregions.org/gazetteer.php?p=search) records,
namely `placeTypes` and `sources`. This is especially useful when using
`mr_gaz_records_by_type()` and `mr_gaz_records_by_source()`.The results
are [`tibbles`](https://tibble.tidyverse.org) with the first 100 of all
`sources` when using `mr_gaz_info("sources")` or with all possible
placeTypes when using `mr_gaz_info("placetypes")`.

``` r
all_sources <- mr_gaz_info("sources")
head(all_sources$source)
#> [1] "SAIL"                                                                                                                                                                                                                                                  
#> [2] "ASFA thesaurus"                                                                                                                                                                                                                                        
#> [3] "Nationaal Instituut voor de Statistiek (NIS) / National Statistical Service"                                                                                                                                                                           
#> [4] "Oosthoek Times Wereldatlas"                                                                                                                                                                                                                            
#> [5] "Lausch, E. (2000). Atlas van de oceanen: met de dieptekaarten van de wereldzeeën, die de Canadese Hydrografische Dienst heeft gepubliceerd (General Bathymetric Chart of the Oceans, GEBCO). Tirion Uitgevers B.V.: Baarn. ISBN 90-7303-598-8. 264 pp."
#> [6] "(2001). The Times comprehensive atlas of the world. 10th ed. Times Books: London. ISBN 0-7230-0792-6. 67, 220, 124 plates pp."
all_types <- mr_gaz_info("placetypes")
head(all_types$type)
#> [1] "Town"                      "Arrondissement"           
#> [3] "Department"                "Province (administrative)"
#> [5] "Country"                   "Continent"
```

### `mr_gaz_records_by_type()`

This function retrieves the first 100 records from the [Marine
Gazetteer](https://marineregions.org/gazetteer.php?p=search) by
inputting a placetype. This can be `"Sandbank"` or `"Archipelago"`, for
instance. The result of `mr_gaz_records_by_type()` is a
[`tibble`](https://tibble.tidyverse.org) with the first 100 records of
the specified `Placetype`.

In the following example, a list of all the available `placeTypes` will
be fetched using `mr_gaz_info("placetypes")`.

``` r
placetypes <- mr_gaz_info("placetypes")

archipelagos <- mr_gaz_records_by_type("Archipelago")
head(archipelagos$preferredGazetteerName)
#> [1] "Vainameri Archipelago" "Azores"                "Channel Islands"      
#> [4] "Faeroe Islands"        "British Isles"         "Isles of Scilly"
```

### `mr_gaz_records_by_source()`

This function retrieves the first 100 records from the [Marine
Gazetteer](https://marineregions.org/gazetteer.php?p=search) by
inputting a source. This can be `"GeoNames"` or `"AlgaeNames"`, for
instance. In the following example, a list of the first 100 available
`sources` will be fetched using `mr_gaz_info("sources")`. The result of
`mr_gaz_records_by_source()` is a
[`tibble`](https://tibble.tidyverse.org) with the first 100 records of
the specified `source`.

``` r
placetypes <- mr_gaz_info("placetypes")

archipelagos <- mr_gaz_records_by_type("Archipelago")
head(archipelagos$preferredGazetteerName)
#> [1] "Vainameri Archipelago" "Azores"                "Channel Islands"      
#> [4] "Faeroe Islands"        "British Isles"         "Isles of Scilly"
```

### `mr_gaz_records_by_latlon()`

This function retrieves the first 100 records from the [Marine
Gazetteer](https://marineregions.org/gazetteer.php?p=search) by
inputting a Latitude and a Longitude. The result of
`mr_gaz_records_by_latlon()` is a
[`tibble`](https://tibble.tidyverse.org) with the first 100 records of
the specified `lat` and `lon`, where their centroid is within the
bounding box calculated by latitude (+/- radius) and longitude (+/-
radius).

In the following examples, all records in the area of the Mariana
Trench, will be retrieved.

``` r
mariana_trench <- mr_gaz_records_by_names("Mariana Trench")

mariana_trench_lat <- mariana_trench$latitude
mariana_trench_lon <- mariana_trench$longitude

records_mariana_trench <- mr_gaz_records_by_latlon(mariana_trench_lat, mariana_trench_lon)

head(records_mariana_trench$preferredGazetteerName)
#> [1] "Trinidad Seamount"      "Victoria Guyot"         "Vogt Guyot"            
#> [4] "Lapulapu Ridge"         "Lowrie Guyot"           "Minami-Kasuga Seamount"
```

There are many Guyots around Mariana Trench, apparently! Let’s explore
them a bit.

``` r
all_types <- mr_gaz_info("placetypes")

guyot <- subset(all_types, type == "Guyot")
guyot$description
#> [1] "A seamount having a comparatively smooth flat top. Also called tablemount."

guyots_mariana_trench <- subset(records_mariana_trench, placeType == "Guyot")

nrow(guyots_mariana_trench)
#> [1] 21
```
