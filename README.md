
# mregions2

<!-- badges: start -->

[![Funding](https://img.shields.io/static/v1?label=powered+by&message=lifewatch.be&labelColor=1a4e8a&color=f15922)](https://lifewatch.be)
[![Status at rOpenSci Software Peer
Review](https://badges.ropensci.org/590_status.svg)](https://github.com/ropensci/software-review/issues/590)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/mregions2)](https://cran.r-project.org/package=mregions2)
[![R-CMD-check](https://github.com/ropensci/mregions2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ropensci/mregions2/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/ropensci/mregions2/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ropensci/mregions2?branch=main)

<!-- badges: end -->

![](https://user-images.githubusercontent.com/54405067/156809171-1487bb9f-35af-4418-8e2f-93c24a59aad0.gif)

`mregions2` offers a streamlined interface to access data from [Marine
Regions](https://marineregions.org) in R for researchers, marine
scientists, and geospatial analysts seeking marine geographical
information

Marine Regions offers two key resources: the [Marine Regions
Gazetteer](https://marineregions.org/gazetteer.php), a list of
standardized marine place names with unique identifiers, and the [Marine
Regions Data Products](https://marineregions.org/sources.php), including
popular features like the world maritime boundaries.

You can find detailed information in the articles online:

- [Introduction to
  mregions2](https://docs.ropensci.org/mregions2/articles/mregions2.html)
- [Why mregions and
  mregions2?](https://docs.ropensci.org/mregions2/articles/why_mregions2.html)
- [Marine Regions Data Products
  Ontology](https://docs.ropensci.org/mregions2/articles/mrp_ontology.html)
- [mregions2 as
  RDF](https://docs.ropensci.org/mregions2/articles/mregions2-rdf.html)

## Installation

You can install the latest CRAN version with:

``` r
install.packages("mregions2")
```

Or install the development version of mregions2 from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("ropensci/mregions2", build_vignettes = TRUE)
```

Load the library with:

``` r
library(mregions2)
```

The function `mrp_view()` requires two extra packages that are not
listed as Imports, hence they are not installed along with `mregions2`
but you must install yourself:

``` r
install.packages("leaflet")
install.packages("leaflet.extras2")
```

Some of the examples below use the pipe operator `%>%`. Install and load
`magrittr`:

``` r
# install.packages("magrittr")
library(magrittr)
```

## Query the Marine Regions Gazetteer

The [Marine Regions Gazetteer](https://marineregions.org/gazetteer.php)
is a standard list of marine georeferenced place names.

> Gazetteer: a dictionary of geographical names.

<sup><https://www.thefreedictionary.com/gazetteer></sup>

You can search the Gazetteer in many ways:

Search by free text:

``` r
gaz_search("Belgian Part of the North Sea")
#> # A tibble: 1 × 9
#>   MRGID gazetteerSource      placeType latitude longitude preferredGazetteerName
#>   <int> <chr>                <chr>        <dbl>     <dbl> <chr>                 
#> 1 26567 "VLIZ (2020). Inter… Marine R…     51.5      2.70 Belgian part of the N…
#> # ℹ 3 more variables: preferredGazetteerNameLang <chr>, status <chr>,
#> #   accepted <int>
```

Search by unique identifier. See `?MRGID`:

``` r
gaz_search(3293)
#> # A tibble: 1 × 14
#>   MRGID gazetteerSource    placeType latitude longitude minLatitude minLongitude
#>   <int> <chr>              <chr>        <dbl>     <dbl>       <dbl>        <dbl>
#> 1  3293 Flanders Marine I… EEZ           51.5      2.72        51.0         2.24
#> # ℹ 7 more variables: maxLatitude <dbl>, maxLongitude <dbl>, precision <dbl>,
#> #   preferredGazetteerName <chr>, preferredGazetteerNameLang <chr>,
#> #   status <chr>, accepted <int>
```

Search by location:

``` r
gaz_search(x = 2.927, y = 51.21551)
#> # A tibble: 55 × 14
#>    MRGID gazetteerSource   placeType latitude longitude minLatitude minLongitude
#>    <int> <chr>             <chr>        <dbl>     <dbl>       <dbl>        <dbl>
#>  1    14 (2001). The Time… Nation        50.5      4.48        49.5         2.55
#>  2    14 (2001). The Time… Nation        50.5      4.48        49.5         2.55
#>  3    14 (2001). The Time… Nation        50.5      4.48        49.5         2.55
#>  4    14 (2001). The Time… Nation        50.5      4.48        49.5         2.55
#>  5    14 (2001). The Time… Nation        50.5      4.48        49.5         2.55
#>  6    14 (2001). The Time… Nation        50.5      4.48        49.5         2.55
#>  7    20 SAIL              Province…     51.0      3.03        50.7         2.55
#>  8    20 SAIL              Province…     51.0      3.03        50.7         2.55
#>  9    20 SAIL              Province…     51.0      3.03        50.7         2.55
#> 10    20 SAIL              Province…     51.0      3.03        50.7         2.55
#> # ℹ 45 more rows
#> # ℹ 7 more variables: maxLatitude <dbl>, maxLongitude <dbl>,
#> #   preferredGazetteerName <chr>, preferredGazetteerNameLang <chr>,
#> #   status <chr>, accepted <int>, precision <dbl>
```

Search by place type:

``` r
gaz_search_by_type("EEZ")
#> # A tibble: 246 × 14
#>    MRGID gazetteerSource   placeType latitude longitude minLatitude minLongitude
#>    <int> <chr>             <chr>        <dbl>     <dbl>       <dbl>        <dbl>
#>  1  3293 Flanders Marine … EEZ           51.5      2.72        51.0         2.24
#>  2  5668 Flanders Marine … EEZ           53.6      4.19        51.3         2.54
#>  3  5669 Flanders Marine … EEZ           54.6      8.40        52.9         3.35
#>  4  5670 Flanders Marine … EEZ           40.9     19.1         39.6        18.3 
#>  5  5672 Flanders Marine … EEZ           42.9     29.2         42.0        27.4 
#>  6  5673 Flanders Marine … EEZ           43.4     15.6         41.6        13.0 
#>  7  5674 Flanders Marine … EEZ           56.2      9.21        54.4         3.25
#>  8  5675 Flanders Marine … EEZ           58.8     23.0         57.6        20.4 
#>  9  5676 Flanders Marine … EEZ           61.8     21.9         58.8        19.1 
#> 10  5677 Flanders Marine … EEZ           46.1     -1.96        41.2        -9.88
#> # ℹ 236 more rows
#> # ℹ 7 more variables: maxLatitude <dbl>, maxLongitude <dbl>, precision <dbl>,
#> #   preferredGazetteerName <chr>, preferredGazetteerNameLang <chr>,
#> #   status <chr>, accepted <int>
```

Search by authortative source:

``` r
gaz_search_by_source("Flanders Marine Institute (VLIZ)")
#> # A tibble: 23 × 13
#>    MRGID gazetteerSource     placeType latitude longitude preferredGazetteerName
#>    <int> <chr>               <chr>        <dbl>     <dbl> <chr>                 
#>  1 62642 Seys, J.; Pint, S.… Sampling…     51.3      3.16 SW-Blankenberge-01    
#>  2 62643 Seys, J.; Pint, S.… Sampling…     51.2      2.96 SW-Bredene-01         
#>  3 62644 Seys, J.; Pint, S.… Sampling…     51.3      3.26 SW-Duinbergen-01      
#>  4 62645 Seys, J.; Pint, S.… Sampling…     51.3      3.00 SW-De Haan-Vosseslag-…
#>  5 62646 Seys, J.; Pint, S.… Sampling…     51.3      3.24 SW-Heist-01           
#>  6 62647 Seys, J.; Pint, S.… Sampling…     51.1      2.62 SW-Koksijde-01        
#>  7 62648 Seys, J.; Pint, S.… Sampling…     51.2      2.81 SW-Middelkerke-01     
#>  8 62649 Seys, J.; Pint, S.… Sampling…     51.2      2.71 SW-Nieuwpoort-01      
#>  9 62650 Seys, J.; Pint, S.… Sampling…     51.1      2.69 SW-Oostduinkerke-01   
#> 10 62651 Seys, J.; Pint, S.… Sampling…     51.2      2.94 SW-Oostende_Oosteroev…
#> # ℹ 13 more rows
#> # ℹ 7 more variables: preferredGazetteerNameLang <chr>, status <chr>,
#> #   accepted <int>, minLatitude <dbl>, minLongitude <dbl>, maxLatitude <dbl>,
#> #   maxLongitude <dbl>
```

The list of place types and sources are available with `gaz_types()` and
`gaz_sources()` respectively.

You can add the geometry of the Gazetteer entries with `gaz_geometry()`:

``` r
# Get a record and turn into a sf object with geometry
gaz_search(3293) %>% gaz_geometry()
#> Simple feature collection with 1 feature and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 2.238333 ymin: 51.03976 xmax: 4.402039 ymax: 51.87611
#> Geodetic CRS:  WGS 84
#> # A tibble: 1 × 15
#>   MRGID gazetteerSource    placeType latitude longitude minLatitude minLongitude
#>   <int> <chr>              <chr>        <dbl>     <dbl>       <dbl>        <dbl>
#> 1  3293 Flanders Marine I… EEZ           51.5      2.72        51.0         2.24
#> # ℹ 8 more variables: maxLatitude <dbl>, maxLongitude <dbl>, precision <dbl>,
#> #   preferredGazetteerName <chr>, preferredGazetteerNameLang <chr>,
#> #   status <chr>, accepted <int>, the_geom <MULTIPOLYGON [°]>

# Or get only the geometry 
gaz_geometry(3293, format = "sfc")
#> Geometry set for 1 feature 
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 2.238333 ymin: 51.03976 xmax: 4.402039 ymax: 51.87611
#> Geodetic CRS:  WGS 84
#> MULTIPOLYGON (((4.242049 51.35397, 4.230735 51....
```

The entries of the Marine Regions Gazetteer are organized
hierarchically. You can browse this hierarchy up and down with
`gaz_relations()`

``` r
# Get all relations
gaz_search(3293) %>% gaz_relations()
#> # A tibble: 37 × 14
#>    MRGID gazetteerSource   placeType latitude longitude minLatitude minLongitude
#>    <int> <chr>             <chr>        <dbl>     <dbl>       <dbl>        <dbl>
#>  1  3293 Flanders Marine … EEZ           51.5      2.72        51.0         2.24
#>  2  2419 ASFA thesaurus    Sandbank…     51.5      2.58        NA          NA   
#>  3  2420 ASFA thesaurus    Sandbank…     51.5      2.88        NA          NA   
#>  4  2421 ASFA thesaurus    Sandbank…     51.3      2.64        NA          NA   
#>  5    14 (2001). The Time… Nation        50.5      4.48        49.5         2.55
#>  6  2350 (1953). Limits o… IHO Sea …     56.4      2.74        51.0        -4.45
#>  7  4675 <NA>              Sandbank…     51.3      2.98        NA          NA   
#>  8  2550 ASFA thesaurus    Coast         51.2      2.91        51.1         2.54
#>  9 17401 Aphia             Wreck         51.4      2.32        NA          NA   
#> 10 17409 Aphia             Wreck         51.1      2.33        NA          NA   
#> # ℹ 27 more rows
#> # ℹ 7 more variables: maxLatitude <dbl>, maxLongitude <dbl>, precision <dbl>,
#> #   preferredGazetteerName <chr>, preferredGazetteerNameLang <chr>,
#> #   status <chr>, accepted <int>

# Or get the relations directly
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
#>  7  4675 <NA>              Sandbank…     51.3      2.98        NA          NA   
#>  8  2550 ASFA thesaurus    Coast         51.2      2.91        51.1         2.54
#>  9 17401 Aphia             Wreck         51.4      2.32        NA          NA   
#> 10 17409 Aphia             Wreck         51.1      2.33        NA          NA   
#> # ℹ 27 more rows
#> # ℹ 7 more variables: maxLatitude <dbl>, maxLongitude <dbl>, precision <dbl>,
#> #   preferredGazetteerName <chr>, preferredGazetteerNameLang <chr>,
#> #   status <chr>, accepted <int>
```

## Marine Regions Data Products

In addition to the Marine Regions Gazetteer, the Marine Regions Team
creates and hosts geographical Data Products, being the most popular one
the [Marine Regions Maritime
Boundaries](https://marineregions.org/eez.php).

An overview of all available products can be consulted with `mrp_list`

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

Their attributes are explained in the [Marine Regions Data Products
Ontology
article](https://docs.ropensci.org/mregions2/articles/mrp_ontology.html),
or simply run `mrp_ontology`

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

You can visualize the Marine Regions Data Products with `mrp_view()`. It
opens an interactive `leaflet::leaflet` viewer.

``` r
mrp_view("eez")
```

![](https://raw.githubusercontent.com/ropensci/mregions2/main/man/figures/README-prod1-1.png)

Or you can download and read the data products into R with `mrp_get()`

``` r
mrp_get("eez")
```

You can specify the download path in the `path` argument:

``` r
mrp_get("eez", path = "path/to/data")
```

Get to know more in the [Get Started
vignette](https://docs.ropensci.org/mregions2/articles/mregions2.html).

## Related packages

### Marine biodiversity data

The main purpose of Marine Regions is to serve as the geographical
backbone for the World Register of Marine Species (WoRMS), an
authoritative classification and catalogue of marine names. Here is a
list of R packages to access Marine taxonomical and biogeographic data
that can be combined with data from `mregions2`:

- [worrms](https://docs.ropensci.org/worrms/): *World Register of Marine
  Species (WoRMS) Client*.
- [eurobis](https://lifewatch.github.io/eurobis/): *Download data from
  EurOBIS using the LifeWatch/EMODnet-Biology Web Feature Services*.
- [robis](https://cran.r-project.org/package=robis): *Ocean Biodiversity
  Information System (OBIS) Client*.
- [rgbif](https://docs.ropensci.org/rgbif/): *Interface to the Global
  Biodiversity Information Facility API*.

### More geographical data

There are other R packages that allow to access other gazetteers and
boundaries data. Here is a non-exhaustive list:

- [geonames](https://docs.ropensci.org/geonames/): *Interface to the
  “Geonames” Spatial Query Web Service*.
- [osmdata](https://docs.ropensci.org/osmdata/): *Import ‘OpenStreetMap’
  Data as Simple Features or Spatial Objects*.
- [osmextract](https://docs.ropensci.org/osmextract/): *Download and
  Import Open Street Map Data Extracts*.
- [rnaturalearth](https://docs.ropensci.org/rnaturalearth/): *World Map
  Data from Natural Earth*.

## Citation

``` r
citation("mregions2")
```

To cite package ‘mregions2’ in publications use:

Fernández Bejarano S, Pohl L (2024). *mregions2: Access Data from
Marineregions.org: Gazetteer & Data Products*.
<doi:10.32614/CRAN.package.mregions2>
<https://doi.org/10.32614/CRAN.package.mregions2>.

A BibTeX entry for LaTeX users is

@Manual{, title = {mregions2: Access Data from Marineregions.org:
Gazetteer & Data Products}, author = {Salvador Jesús {Fernández
Bejarano} and Lotte Pohl}, year = {2024}, doi =
{10.32614/CRAN.package.mregions2}, }

## License

MIT. See `LICENSE.md`

This software is developed for scientific, educational and research
purposes. It is not meant to be used for legal, economical (in the sense
of exploration of natural resources) or navigational purposes. See the
[Marine Regions disclaimer](https://marineregions.org/disclaimer.php)
for more information.

## Code of Conduct

Please note that this package is released with a [Contributor Code of
Conduct](https://ropensci.org/code-of-conduct/). By contributing to this
project, you agree to abide by its terms.
