# Marine Regions Data Products Ontology

> An ontology is a categorization of all of the concepts in some field
> of knowledge.

[GNU Collaborative International Dictionary of
English](https://gcide.gnu.org.ua)

The data products hosted at marineregions.org are of vector type, which
they come together with attribute tables. These attribute tables are
named, but such names are not yet standardized.

To ease the understanding of the data products, the Marine Regions Team
has described the attributes.

The Flanders Marine Institute (VLIZ), hosting institution behind Marine
Regions, is currently working towards Open Linked Data standards
([Lonneville et al.,
2021](http://ceur-ws.org/Vol-2969/paper8-s4biodiv.pdf)). The naming of
these attributes will likely turn into standardized terms reusing or
expanding on-going RDF-available ontologies.

This work was triggered during the [rOpenSci
review](https://github.com/ropensci/software-review/issues/590#issuecomment-1606138219).
Thanks to [@sheilasaia](https://github.com/sheilasaia) for raising this
issue.

You can consult the list in your R session via the `mrp_ontology` object
that is loaded with `mregions2`

``` r

library(mregions2)
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

------------------------------------------------------------------------

### Exclusive Economic Zones (200 NM) (v11, world, 2019)

Version 11 of the Exclusive Economic Zones from the VLIZ Maritime
Boundaries Geodatabase. An exclusive economic zone (EEZ) is a seazone
extending from a state’s coast or baseline over which the state has
special rights over the exploration and use of marine resources.
Generally a state’s EEZ extends 200 nautical miles out from its coast,
except where resulting points would be closer to another country. This
dataset also contains delimitation of disputed areas and joint regimes.

Update (2021-01-14): corrected centroid longitude for features crossing
the dateline

| layer | colname | type | definition |
|:---|:---|:---|:---|
| eez | mrgid | int | Marine Regions Geographic Identifier of the feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez | geoname | string | Name of the feature. |
| eez | mrgid_ter1 | int | Marine Regions Geographic Identifier of the territory feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez | pol_type | string | Basis of creation or legal status of feature. One of: 200NM, Joint regime, Overlapping claim. |
| eez | mrgid_sov1 | int | Marine Regions Geographic Identifier of the sovereign nation feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez | territory1 | string | Specific land area which directly relates to the feature. |
| eez | iso_ter1 | string | ISO 3 code of the territory feature. |
| eez | sovereign1 | string | State that claims jurisdiction over the territory. |
| eez | mrgid_ter2 | int | Marine Regions Geographic Identifier of the territory feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez | mrgid_sov2 | int | Marine Regions Geographic Identifier of the sovereign nation feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez | territory2 | string | Specific land area which directly relates to the feature. |
| eez | iso_ter2 | string | ISO 3 code of the territory feature. |
| eez | sovereign2 | string | State that claims jurisdiction over the territory. |
| eez | mrgid_ter3 | int | Marine Regions Geographic Identifier of the territory feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez | mrgid_sov3 | int | Marine Regions Geographic Identifier of the sovereign nation feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez | territory3 | string | Specific land area which directly relates to the feature. |
| eez | iso_ter3 | string | ISO 3 code of the territory feature. |
| eez | sovereign3 | string | State that claims jurisdiction over the territory. |
| eez | x_1 | decimal | Centroid longitude of feature in EPGS:4326 |
| eez | y_1 | decimal | Centroid latitude of feature in EPGS:4326 |
| eez | mrgid_eez | int | Marine Regions Geographic Identifier of the feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez | area_km2 | int | Area of feature in square kilometers. |
| eez | iso_sov1 | string | ISO 3 code of the sovereign nation feature. |
| eez | iso_sov2 | string | ISO 3 code of the sovereign nation feature. |
| eez | iso_sov3 | string | ISO 3 code of the sovereign nation feature. |
| eez | un_sov1 | double | United Nations M49 Country Code of the sovereign nation feature. |
| eez | un_sov2 | double | United Nations M49 Country Code of the sovereign nation feature. |
| eez | un_sov3 | double | United Nations M49 Country Code of the sovereign nation feature. |
| eez | un_ter1 | double | United Nations M49 Country Code of the territory feature. |
| eez | un_ter2 | double | United Nations M49 Country Code of the territory feature. |
| eez | un_ter3 | double | United Nations M49 Country Code of the territory feature. |

### Maritime Boundaries (v11, world, 2019)

Version 11 of the Maritime Boundaries from the VLIZ Maritime Boundaries
Geodatabase. Boundaries have been built using information about treaties
between coastal countries. When treaties are not available, median lines
have been calculated. This dataset also contains delimitation of
disputed boundaries and joint regimes.

| layer | colname | type | definition |
|:---|:---|:---|:---|
| eez_boundaries | line_id | decimal | Unique identifier of feature. |
| eez_boundaries | line_name | string | Name of the feature. |
| eez_boundaries | line_type | string | Basis of creation or legal status of feature. One of: 200 NM, Archipelagic Baseline, Connection line, Joint regime, Median line, Straight Baseline, Treaty, Unilateral claim (undisputed), Unsettled, Unsettled median line |
| eez_boundaries | mrgid_sov1 | decimal | Marine Regions Geographic Identifier of the sovereign nation feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez_boundaries | mrgid_ter1 | decimal | Marine Regions Geographic Identifier of the territory feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez_boundaries | territory1 | string | Specific land area which directly relates to the feature. |
| eez_boundaries | sovereign1 | string | State that claims jurisdiction over the territory. |
| eez_boundaries | mrgid_ter2 | decimal | Marine Regions Geographic Identifier of the territory feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez_boundaries | territory2 | string | Specific land area which directly relates to the feature. |
| eez_boundaries | mrgid_sov2 | decimal | Marine Regions Geographic Identifier of the sovereign nation feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez_boundaries | sovereign2 | string | State that claims jurisdiction over the territory. |
| eez_boundaries | mrgid_eez1 | decimal | Marine Regions Geographic Identifier of the Exclusive Economic Zone feature that origins this feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez_boundaries | eez1 | string | Name of the Exclusive Economic Zone feature that origins from this feature |
| eez_boundaries | mrgid_eez2 | decimal | Marine Regions Geographic Identifier of the Exclusive Economic Zone feature that origins this feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez_boundaries | eez2 | string | Name of the Exclusive Economic Zone feature that origins from this feature |
| eez_boundaries | source1 | string | Name of the authoritative source that defines the feature. |
| eez_boundaries | url1 | string | Uniform Resource Locator to the authoritative source that defines the feature. |
| eez_boundaries | source2 | string | Name of the authoritative source that defines the feature. |
| eez_boundaries | url2 | string | Uniform Resource Locator to the authoritative source that defines the feature. |
| eez_boundaries | source3 | string | Name of the authoritative source that defines the feature. |
| eez_boundaries | url3 | string | Uniform Resource Locator to the authoritative source that defines the feature. |
| eez_boundaries | origin | string | Type of source of feature. One of: Buffer, Database, External, Link, Median |
| eez_boundaries | doc_date | date | Date of publication of the authoritative source. |
| eez_boundaries | mrgid_jreg | decimal | Marine Regions Geographic Identifier of the Joint Regime polygon feature related to this feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez_boundaries | joint_reg | string | Name of the Joint Regime polygon feature related to this feature. |
| eez_boundaries | length_km | decimal | Length of feature in kilometers. |

### Territorial Seas (12 NM) (v3, world, 2019)

Version 3 of the Territorial Seas from the VLIZ Maritime Boundaries
Geodatabase. Territorial seas are a belt of coastal waters extending at
most 12 nautical miles (22.2 km; 13.8 mi) from the baseline (usually the
mean low-water mark) of a coastal state.

Update (2021-01-14): corrected centroid longitude for features crossing
the dateline

| layer | colname | type | definition |
|:---|:---|:---|:---|
| eez_12nm | mrgid | int | Marine Regions Geographic Identifier of the feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez_12nm | geoname | string | Name of the feature. |
| eez_12nm | pol_type | string | Basis of creation or legal status of feature. |
| eez_12nm | mrgid_ter1 | int | Marine Regions Geographic Identifier of the territory feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez_12nm | territory1 | string | Specific land area which directly relates to the feature. |
| eez_12nm | mrgid_sov1 | int | Marine Regions Geographic Identifier of the sovereign nation feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez_12nm | sovereign1 | string | State that claims jurisdiction over the territory. |
| eez_12nm | iso_ter1 | string | ISO 3 code of the territory feature. |
| eez_12nm | x_1 | decimal | Centroid longitude of feature in EPGS:4326 |
| eez_12nm | y_1 | decimal | Centroid latitude of feature in EPGS:4326 |
| eez_12nm | mrgid_eez | int | Marine Regions Geographic Identifier of the Exclusive Economic Zone feature related to this feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez_12nm | area_km2 | int | Area of feature in square kilometers. |
| eez_12nm | iso_sov1 | string | ISO 3 code of the sovereign nation feature. |
| eez_12nm | un_sov1 | double | United Nations M49 Country Code of the sovereign nation feature. |
| eez_12nm | un_ter1 | double | United Nations M49 Country Code of the territory feature. |

### Contiguous Zones (24 NM) (v3, world, 2019)

Version 3 of the Contiguous Zones from the VLIZ Maritime Boundaries
Geodatabase. The Contiguous Zone is a band of water extending from the
outer edge of the territorial sea to up to 24 nautical miles (44.4 km;
27.6 mi) from the baseline.

Update (2021-01-14): corrected centroid longitude for features crossing
the dateline

| layer | colname | type | definition |
|:---|:---|:---|:---|
| eez_24nm | mrgid | int | Marine Regions Geographic Identifier of the feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez_24nm | geoname | string | Name of the feature. |
| eez_24nm | pol_type | string | Basis of creation or legal status of feature. |
| eez_24nm | mrgid_ter1 | int | Marine Regions Geographic Identifier of the territory feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez_24nm | territory1 | string | Specific land area which directly relates to the feature. |
| eez_24nm | mrgid_sov1 | int | Marine Regions Geographic Identifier of the sovereign nation feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez_24nm | sovereign1 | string | State that claims jurisdiction over the territory. |
| eez_24nm | iso_ter1 | string | ISO 3 code of the territory feature. |
| eez_24nm | x_1 | decimal | Centroid longitude of feature in EPGS:4326 |
| eez_24nm | y_1 | decimal | Centroid latitude of feature in EPGS:4326 |
| eez_24nm | mrgid_eez | int | Marine Regions Geographic Identifier of the Exclusive Economic Zone feature related to this feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez_24nm | area_km2 | int | Area of feature in square kilometers. |
| eez_24nm | iso_sov1 | string | ISO 3 code of the sovereign nation feature. |
| eez_24nm | un_sov1 | double | United Nations M49 Country Code of the sovereign nation feature. |
| eez_24nm | un_ter1 | double | United Nations M49 Country Code of the territory feature. |

### Internal Waters (v3, world, 2019)

Version 3 of the Internal Waters from the VLIZ Maritime Boundaries
Geodatabase. Internal Waters are the waters on the landward side of the
baseline of a nation’s territorial waters, except in archipelagic
states. It includes waterways such as rivers and canals, and sometimes
the water within small bays.

| layer | colname | type | definition |
|:---|:---|:---|:---|
| eez_internal_waters | mrgid | int | Marine Regions Geographic Identifier of the feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez_internal_waters | geoname | string | Name of the feature. |
| eez_internal_waters | pol_type | string | Basis of creation or legal status of feature. |
| eez_internal_waters | mrgid_ter1 | int | Marine Regions Geographic Identifier of the territory feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez_internal_waters | territory1 | string | Specific land area which directly relates to the feature. |
| eez_internal_waters | mrgid_sov1 | int | Marine Regions Geographic Identifier of the sovereign nation feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez_internal_waters | sovereign1 | string | State that claims jurisdiction over the territory. |
| eez_internal_waters | iso_ter1 | string | ISO 3 code of the territory feature. |
| eez_internal_waters | x_1 | decimal | Centroid longitude of feature in EPGS:4326 |
| eez_internal_waters | y_1 | decimal | Centroid latitude of feature in EPGS:4326 |
| eez_internal_waters | mrgid_eez | int | Marine Regions Geographic Identifier of the Exclusive Economic Zone feature related to this feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez_internal_waters | area_km2 | int | Area of feature in square kilometers. |
| eez_internal_waters | iso_sov1 | string | ISO 3 code of the sovereign nation feature. |
| eez_internal_waters | un_sov1 | double | United Nations M49 Country Code of the sovereign nation feature. |
| eez_internal_waters | un_ter1 | double | United Nations M49 Country Code of the territory feature. |

### Archipelagic Waters (v3, world, 2019)

Version 3 of the Archipelagic Waters from the VLIZ Maritime Boundaries
Geodatabase. Archipelagic Waters are waters falling within archipelagic
baselines.

Update (2021-01-14): corrected centroid longitude for features crossing
the dateline

| layer | colname | type | definition |
|:---|:---|:---|:---|
| eez_archipelagic_waters | mrgid | int | Marine Regions Geographic Identifier of the feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez_archipelagic_waters | geoname | string | Name of the feature. |
| eez_archipelagic_waters | pol_type | string | Basis of creation or legal status of feature. |
| eez_archipelagic_waters | mrgid_ter1 | int | Marine Regions Geographic Identifier of the territory feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez_archipelagic_waters | territory1 | string | Specific land area which directly relates to the feature. |
| eez_archipelagic_waters | mrgid_sov1 | int | Marine Regions Geographic Identifier of the sovereign nation feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez_archipelagic_waters | sovereign1 | string | State that claims jurisdiction over the territory. |
| eez_archipelagic_waters | iso_ter1 | string | ISO 3 code of the territory feature. |
| eez_archipelagic_waters | x_1 | decimal | Centroid longitude of feature in EPGS:4326 |
| eez_archipelagic_waters | y_1 | decimal | Centroid latitude of feature in EPGS:4326 |
| eez_archipelagic_waters | mrgid_eez | int | Marine Regions Geographic Identifier of the Exclusive Economic Zone feature related to this feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez_archipelagic_waters | area_km2 | int | Area of feature in square kilometers. |
| eez_archipelagic_waters | iso_sov1 | string | ISO 3 code of the sovereign nation feature. |
| eez_archipelagic_waters | un_sov1 | double | United Nations M49 Country Code of the sovereign nation feature. |
| eez_archipelagic_waters | un_ter1 | double | United Nations M49 Country Code of the territory feature. |

### High Seas (v1, world, 2020)

High Seas from the VLIZ Maritime Boundaries Geodatabase. The United
Nations Convention on the Law of the Sea describes the high seas as ‘all
parts of the sea that are not included in the exclusive economic zone,
in the territorial sea or in the internal waters of a State, or in the
archipelagic waters of an archipelagic State.’ In the Maritime
Boundaries Geodatabase, Marine Regions makes available most of the
maritime areas defined in the Law of the Sea Convention: Exclusive
Economic Zones (EEZ), Territorial Seas (TS), Contiguous Zones (CZ),
Internal Waters (IW), Archipelagic Waters (AW) and High Seas (HS).

Flanders Marine Institute (2020). Maritime Boundaries Geodatabase: High
Seas, version 1. Available online at <http://www.marineregions.org/>
<https://doi.org/10.14284/418>.

Methodology: <https://www.marineregions.org/eezmethodology.php>

| layer | colname | type | definition |
|:---|:---|:---|:---|
| high_seas | \_\_gid | long | Unique identifier of feature. |
| high_seas | mrgid | decimal | Marine Regions Geographic Identifier of the feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| high_seas | source | string | Name of the authoritative source that defines the feature. |
| high_seas | area_km2 | decimal | Area of feature in square kilometers. |

### Extended Continental Shelves (v01, world, 2022)

This dataset represents the legal continental shelves beyond 200
nautical miles as submitted to/recommended by the Commission on the
Limits of the Continental Shelf (CLCS) or deposited to the Division for
Ocean Affairs and the Law of the Sea (DOALOS). In the Maritime
Boundaries Geodatabase, Marine Regions makes available most of the
maritime areas defined in the Law of the Sea Convention: Exclusive
Economic Zones (EEZ), Territorial Seas (TS), Contiguous Zones (CZ),
Internal Waters (IW), Archipelagic Waters (AW) and High Seas (HS).

| layer | colname | type | definition |
|:---|:---|:---|:---|
| ecs | geoname | string | Name of the feature. |
| ecs | pol_type | string | Basis of creation or legal status of feature. |
| ecs | territory1 | string | Specific land area which directly relates to the feature. |
| ecs | mrgid_ter1 | string | Marine Regions Geographic Identifier of the territory feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| ecs | iso_ter1 | string | ISO 3 code of the territory feature. |
| ecs | un_ter1 | string | United Nations M49 Country Code of the territory feature. |
| ecs | sovereign1 | string | State that claims jurisdiction over the territory. |
| ecs | mrgid_sov1 | int | Marine Regions Geographic Identifier of the sovereign nation feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| ecs | iso_sov1 | string | ISO 3 code of the sovereign nation feature. |
| ecs | un_sov1 | string | United Nations M49 Country Code of the sovereign nation feature. |
| ecs | territory2 | string | Specific land area which directly relates to the feature. |
| ecs | mrgid_ter2 | string | Marine Regions Geographic Identifier of the territory feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| ecs | iso_ter2 | string | ISO 3 code of the territory feature. |
| ecs | un_ter2 | string | United Nations M49 Country Code of the territory feature. |
| ecs | sovereign2 | string | State that claims jurisdiction over the territory. |
| ecs | mrgid_sov2 | int | Marine Regions Geographic Identifier of the sovereign nation feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| ecs | iso_sov2 | string | ISO 3 code of the sovereign nation feature. |
| ecs | un_sov2 | string | United Nations M49 Country Code of the sovereign nation feature. |
| ecs | territory3 | string | Specific land area which directly relates to the feature. |
| ecs | mrgid_ter3 | string | Marine Regions Geographic Identifier of the territory feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| ecs | iso_ter3 | string | ISO 3 code of the territory feature. |
| ecs | un_ter3 | string | United Nations M49 Country Code of the territory feature. |
| ecs | sovereign3 | string | State that claims jurisdiction over the territory. |
| ecs | mrgid_sov3 | int | Marine Regions Geographic Identifier of the sovereign nation feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| ecs | iso_sov3 | string | ISO 3 code of the sovereign nation feature. |
| ecs | un_sov3 | string | United Nations M49 Country Code of the sovereign nation feature. |
| ecs | territory4 | string | Specific land area which directly relates to the feature. |
| ecs | mrgid_ter4 | string | Marine Regions Geographic Identifier of the territory feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| ecs | iso_ter4 | string | ISO 3 code of the territory feature. |
| ecs | un_ter4 | string | United Nations M49 Country Code of the territory feature. |
| ecs | sovereign4 | string | State that claims jurisdiction over the territory. |
| ecs | mrgid_sov4 | int | Marine Regions Geographic Identifier of the sovereign nation feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| ecs | iso_sov4 | string | ISO 3 code of the sovereign nation feature. |
| ecs | un_sov4 | string | United Nations M49 Country Code of the sovereign nation feature. |
| ecs | territory5 | string | Specific land area which directly relates to the feature. |
| ecs | mrgid_ter5 | string | Marine Regions Geographic Identifier of the territory feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| ecs | iso_ter5 | string | ISO 3 code of the territory feature. |
| ecs | un_ter5 | string | United Nations M49 Country Code of the territory feature. |
| ecs | sovereign5 | string | State that claims jurisdiction over the territory. |
| ecs | mrgid_sov5 | int | Marine Regions Geographic Identifier of the sovereign nation feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| ecs | iso_sov5 | string | ISO 3 code of the sovereign nation feature. |
| ecs | un_sov5 | string | United Nations M49 Country Code of the sovereign nation feature. |
| ecs | territory6 | string | Specific land area which directly relates to the feature. |
| ecs | mrgid_ter6 | string | Marine Regions Geographic Identifier of the territory feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| ecs | iso_ter6 | string | ISO 3 code of the territory feature. |
| ecs | un_ter6 | string | United Nations M49 Country Code of the territory feature. |
| ecs | sovereign6 | string | State that claims jurisdiction over the territory. |
| ecs | mrgid_sov6 | int | Marine Regions Geographic Identifier of the sovereign nation feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| ecs | iso_sov6 | string | ISO 3 code of the sovereign nation feature. |
| ecs | un_sov6 | string | United Nations M49 Country Code of the sovereign nation feature. |
| ecs | territory7 | string | Specific land area which directly relates to the feature. |
| ecs | mrgid_ter7 | string | Marine Regions Geographic Identifier of the territory feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| ecs | iso_ter7 | string | ISO 3 code of the territory feature. |
| ecs | un_ter7 | string | United Nations M49 Country Code of the territory feature. |
| ecs | sovereign7 | string | State that claims jurisdiction over the territory. |
| ecs | mrgid_sov7 | int | Marine Regions Geographic Identifier of the sovereign nation feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| ecs | iso_sov7 | string | ISO 3 code of the sovereign nation feature. |
| ecs | un_sov7 | string | United Nations M49 Country Code of the sovereign nation feature. |
| ecs | x_1 | string | Centroid longitude of feature in EPGS:4326 |
| ecs | y_1 | string | Centroid latitude of feature in EPGS:4332 |
| ecs | area_km2 | string | Area of feature in square kilometers. |
| ecs | mrgid | int | Marine Regions Geographic Identifier of the feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| ecs | mrgid_ecs | int | Marine Regions Geographic Identifier of the Extended Continental Shelf feature related to this feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |

### Extended Continental Shelves - boundaries (v01, world, 2022)

This dataset represents the outer boundaries of the legal continental
shelves beyond 200 nautical miles as submitted to/recommended by the
Commission on the Limits of the Continental Shelf (CLCS) or deposited to
the Division for Ocean Affairs and the Law of the Sea (DOALOS). In the
Maritime Boundaries Geodatabase, Marine Regions makes available most of
the maritime areas defined in the Law of the Sea Convention: Exclusive
Economic Zones (EEZ), Territorial Seas (TS), Contiguous Zones (CZ),
Internal Waters (IW), Archipelagic Waters (AW) and High Seas (HS).

| layer | colname | type | definition |
|:---|:---|:---|:---|
| ecs_boundaries | line_id | double | Unique identifier of feature. |
| ecs_boundaries | line_name | string | Name of the feature. |
| ecs_boundaries | line_type | string | Basis of creation or legal status of feature. One of: ECS CLCS Joint Recommendation, ECS CLCS Joint Submission, ECS CLCS Recommendation, ECS CLCS Submission, ECS Connection Line, ECS DOALOS Deposit, ECS DOALOS Joint Deposit, ECS Treaty |
| ecs_boundaries | territory1 | string | Specific land area which directly relates to the feature. |
| ecs_boundaries | sovereign1 | string | State that claims jurisdiction over the territory. |
| ecs_boundaries | territory2 | string | Specific land area which directly relates to the feature. |
| ecs_boundaries | sovereign2 | string | State that claims jurisdiction over the territory. |
| ecs_boundaries | territory3 | string | Specific land area which directly relates to the feature. |
| ecs_boundaries | sovereign3 | string | State that claims jurisdiction over the territory. |
| ecs_boundaries | territory4 | string | Specific land area which directly relates to the feature. |
| ecs_boundaries | sovereign4 | string | State that claims jurisdiction over the territory. |
| ecs_boundaries | territory5 | string | Specific land area which directly relates to the feature. |
| ecs_boundaries | sovereign5 | string | State that claims jurisdiction over the territory. |
| ecs_boundaries | territory6 | string | Specific land area which directly relates to the feature. |
| ecs_boundaries | sovereign6 | string | State that claims jurisdiction over the territory. |
| ecs_boundaries | origin | string | Type of source of feature. One of: Buffer, Database, External, Link, Median |
| ecs_boundaries | url1 | string | Uniform Resource Locator to the authoritative source that defines the feature. |
| ecs_boundaries | source1 | string | Name of the authoritative source that defines the feature. |
| ecs_boundaries | doc_date | date | Date of publication of the authoritative source. |
| ecs_boundaries | mrgid_ter1 | double | Name of the authoritative source that defines the feature. |
| ecs_boundaries | mrgid_ter2 | double | Marine Regions Geographic Identifier of the territory feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| ecs_boundaries | mrgid_ter3 | double | Marine Regions Geographic Identifier of the territory feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| ecs_boundaries | mrgid_ter4 | double | Marine Regions Geographic Identifier of the territory feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| ecs_boundaries | mrgid_ter5 | double | Marine Regions Geographic Identifier of the territory feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| ecs_boundaries | mrgid_ter6 | double | Marine Regions Geographic Identifier of the territory feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| ecs_boundaries | mrgid_sov1 | double | Marine Regions Geographic Identifier of the sovereign nation feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| ecs_boundaries | mrgid_sov2 | double | Marine Regions Geographic Identifier of the sovereign nation feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| ecs_boundaries | mrgid_sov3 | double | Marine Regions Geographic Identifier of the sovereign nation feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| ecs_boundaries | mrgid_sov4 | double | Marine Regions Geographic Identifier of the sovereign nation feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| ecs_boundaries | mrgid_sov5 | double | Marine Regions Geographic Identifier of the sovereign nation feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| ecs_boundaries | mrgid_sov6 | double | Marine Regions Geographic Identifier of the sovereign nation feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| ecs_boundaries | url2 | string | Uniform Resource Locator to the authoritative source that defines the feature. |
| ecs_boundaries | url3 | string | Uniform Resource Locator to the authoritative source that defines the feature. |
| ecs_boundaries | source2 | string | Name of the authoritative source that defines the feature. |
| ecs_boundaries | source3 | string | Name of the authoritative source that defines the feature. |
| ecs_boundaries | length_km | decimal | Length of feature in kilometers. |

### IHO Sea Areas (v3)

World seas represents the boundaries for the major oceans and seas of
the world. The source for the boundaries is the publication ‘Limits of
Oceans & Seas, Special Publication No. 23’ published by the IHO in 1953.
(<http://www.marineregions.org/files/S23_1953.pdf>)

| layer | colname | type | definition |
|:---|:---|:---|:---|
| iho | id | string | Unique identifier of feature. |
| iho | longitude | decimal | Centroid longitude of feature in EPGS:4326 |
| iho | latitude | decimal | Centroid latitude of feature in EPGS:4326 |
| iho | min_x | decimal | Minimum longitude of feature in EPGS:4326 |
| iho | min_y | decimal | Minimum latitude of feature in EPGS:4326 |
| iho | max_x | decimal | Maximum longitude of feature in EPGS:4326 |
| iho | max_y | decimal | Maximum latitude of feature in EPGS:4326 |
| iho | area | long | Area of feature in square kilometers. |
| iho | mrgid | int | Marine Regions Geographic Identifier of the feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |

### Global Oceans and Seas (v1)

Global Oceans and Seas represents the boundaries between the 10 main
oceans and seas (Arctic Ocean, North and South Atlantic Ocean, North and
South Pacific Ocean, Southern Ocean, Indian Ocean, Baltic Sea,
Mediterranean Region, South China and Eastern Archipelagic Seas). The
boundaries are largely based on the publication ‘Limits of Oceans &
Seas, Special Publication No. 23’, published by the IHO in 1953. The
dataset is available in World Geodetic System of 1984 (WGS84).

| layer | colname   | type    | definition                                 |
|:------|:----------|:--------|:-------------------------------------------|
| goas  | latitude  | decimal | Centroid longitude of feature in EPGS:4326 |
| goas  | longitude | decimal | Centroid latitude of feature in EPGS:4326  |
| goas  | min_y     | decimal | Minimum latitude of feature in EPGS:4326   |
| goas  | min_x     | decimal | Minimum longitude of feature in EPGS:4326  |
| goas  | max_y     | decimal | Maximum latitude of feature in EPGS:4326   |
| goas  | max_x     | decimal | Maximum longitude of feature in EPGS:4326  |
| goas  | area_km2  | double  | Area of feature in square kilometers.      |

### The intersect of the Exclusive Economic Zones and IHO areas (v4)

The maritime boundaries provide a useful tool to limit national marine
areas, but do not include information on marine regional and sub
regional seas. This hampers the usage of these boundaries for
implementing nature conservation strategies or analyzing marine
biogeographic patterns. For example, a species occurring in the German
EEZ can live in the North Sea, the Baltic Sea or Kattegat area. Each of
these different marine areas has very distinct hydrological,
oceanographic and ecological conditions.

| layer | colname | type | definition |
|:---|:---|:---|:---|
| eez_iho | mrgid | long | Marine Regions Geographic Identifier of the feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez_iho | marregion | string | Name of the feature. |
| eez_iho | mrgid_iho | long | Marine Regions Geographic Identifier of the International Hydrographic Office Sea Area feature related to this feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez_iho | iho_sea | string | Name of the International Hydrographic Office Sea Area polygon feature related to this feature. |
| eez_iho | mrgid_eez | long | Marine Regions Geographic Identifier of the Exclusive Economic Zone feature related to this feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez_iho | eez | string | Name of the Exclusive Economic Zone feature related to this feature. |
| eez_iho | mrgid_ter1 | long | Marine Regions Geographic Identifier of the territory feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez_iho | territory1 | string | Specific land area which directly relates to the feature. |
| eez_iho | iso_ter1 | string | ISO 3 code of the territory feature. |
| eez_iho | un_ter1 | long | United Nations M49 Country Code of the territory feature. |
| eez_iho | mrgid_sov1 | long | Marine Regions Geographic Identifier of the sovereign nation feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez_iho | sovereign1 | string | State that claims jurisdiction over the territory. |
| eez_iho | iso_sov1 | string | ISO 3 code of the sovereign nation feature. |
| eez_iho | un_sov1 | long | United Nations M49 Country Code of the sovereign nation feature. |
| eez_iho | mrgid_ter2 | long | Marine Regions Geographic Identifier of the territory feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez_iho | territory2 | string | Specific land area which directly relates to the feature. |
| eez_iho | iso_ter2 | string | ISO 3 code of the territory feature. |
| eez_iho | un_ter2 | long | United Nations M49 Country Code of the territory feature. |
| eez_iho | mrgid_sov2 | long | Marine Regions Geographic Identifier of the sovereign nation feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez_iho | sovereign2 | string | State that claims jurisdiction over the territory. |
| eez_iho | iso_sov2 | string | ISO 3 code of the sovereign nation feature. |
| eez_iho | un_sov2 | long | United Nations M49 Country Code of the sovereign nation feature. |
| eez_iho | mrgid_ter3 | long | Marine Regions Geographic Identifier of the territory feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez_iho | territory3 | string | Specific land area which directly relates to the feature. |
| eez_iho | iso_ter3 | string | ISO 3 code of the territory feature. |
| eez_iho | un_ter3 | long | United Nations M49 Country Code of the territory feature. |
| eez_iho | mrgid_sov3 | long | Marine Regions Geographic Identifier of the sovereign nation feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez_iho | sovereign3 | string | State that claims jurisdiction over the territory. |
| eez_iho | iso_sov3 | string | ISO 3 code of the sovereign nation feature. |
| eez_iho | un_sov3 | long | United Nations M49 Country Code of the sovereign nation feature. |
| eez_iho | area_km2 | long | Area of feature in square kilometers. |
| eez_iho | x_1 | decimal | Centroid longitude of feature in EPGS:4326 |
| eez_iho | y_1 | decimal | Centroid latitude of feature in EPGS:4326 |

### Marine and land zones: the union of world country boundaries and EEZ’s

This dataset combines the boundaries of the world countries and the
Exclusive Economic Zones of the world. It was created by combining the
ESRI world country database and the EEZ version 11 dataset.

| layer | colname | type | definition |
|:---|:---|:---|:---|
| eez_land | union | string | Name of the feature. |
| eez_land | mrgid_eez | long | Marine Regions Geographic Identifier of the Exclusive Economic Zone feature related to this feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez_land | territory1 | string | Specific land area which directly relates to the feature. |
| eez_land | mrgid_ter1 | long | Marine Regions Geographic Identifier of the territory feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez_land | iso_ter1 | string | ISO 3 code of the territory feature. |
| eez_land | un_ter1 | long | United Nations M49 Country Code of the territory feature. |
| eez_land | sovereign1 | string | State that claims jurisdiction over the territory. |
| eez_land | mrgid_sov1 | long | Marine Regions Geographic Identifier of the sovereign nation feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez_land | iso_sov1 | string | ISO 3 code of the sovereign nation feature. |
| eez_land | un_sov1 | long | United Nations M49 Country Code of the sovereign nation feature. |
| eez_land | territory2 | string | Specific land area which directly relates to the feature. |
| eez_land | mrgid_ter2 | long | Marine Regions Geographic Identifier of the territory feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez_land | iso_ter2 | string | ISO 3 code of the territory feature. |
| eez_land | un_ter2 | long | United Nations M49 Country Code of the territory feature. |
| eez_land | sovereign2 | string | State that claims jurisdiction over the territory. |
| eez_land | mrgid_sov2 | long | Marine Regions Geographic Identifier of the sovereign nation feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez_land | iso_sov2 | string | ISO 3 code of the sovereign nation feature. |
| eez_land | un_sov2 | long | United Nations M49 Country Code of the sovereign nation feature. |
| eez_land | territory3 | string | Specific land area which directly relates to the feature. |
| eez_land | mrgid_ter3 | long | Marine Regions Geographic Identifier of the territory feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez_land | iso_ter3 | string | ISO 3 code of the territory feature. |
| eez_land | un_ter3 | long | United Nations M49 Country Code of the territory feature. |
| eez_land | sovereign3 | string | State that claims jurisdiction over the territory. |
| eez_land | mrgid_sov3 | long | Marine Regions Geographic Identifier of the sovereign nation feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| eez_land | iso_sov3 | string | ISO 3 code of the sovereign nation feature. |
| eez_land | un_sov3 | long | United Nations M49 Country Code of the sovereign nation feature. |
| eez_land | pol_type | string | Basis of creation or legal status of feature. |
| eez_land | y_1 | decimal | Centroid latitude of feature in EPGS:4326 |
| eez_land | x_1 | decimal | Centroid longitude of feature in EPGS:4326 |
| eez_land | area_km2 | long | Area of feature in square kilometers. |

### Global Biogeochemical Provinces (Longhurst)

The dataset represents the division of the world oceans into provinces
as defined by Longhurst (1995; 1998; 2006). The division has been based
on the prevailing role of physical forcing as a regulator of
phytoplankton distribution. The dataset contains the initial static
boundaries developed at the Bedford Institute of Oceanography, Canada.
Note that the boundaries of these provinces are not fixed in time and
space, but are dynamic and move under seasonal and interannual changes
in physical forcing. At the first level of reduction, Longhurst
recognised four principal biomes: the Polar biome, the Westerlies biome,
the Trade winds biome, and the Coastal biome. These four biomes are
recognised in every major ocean basin. At the next level of reduction,
the ocean basins are divided into provinces, roughly ten for each basin.
These regions provide a template for data analysis or for making
parameter assignments on a global scale. Please refer to Longhurst’s
publications when using these shapefiles.

| layer | colname | type | definition |
|:---|:---|:---|:---|
| longhurst | provcode | string | Unique identifier of feature as 4 code. |
| longhurst | provdescr | string | Name of feature. |
| longhurst | x | decimal | Centroid longitude of feature in EPGS:4326 |
| longhurst | y | decimal | Centroid latitude of feature in EPGS:4326 |
| longhurst | area_m2 | decimal | Area of feature in square meters. |
| longhurst | orig_fid | int | Unique identifier of feature. |
| longhurst | mrgid | int | Marine Regions Geographic Identifier of the feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |

### Global contourite distribution

Contourites are sedimentary units which are deposited or substantially
reworked by alongslope bottom currents. They refer to the lithological
unit and the terminology reflects the grain size of the sediment. Drifts
refer to the same sedimentary unit as a contourite, but the word ‘drift’
reflects the morphological appearance of the sedimentary unit. Their
terminology reflects the external shape, and the internal shape, which
is determined by seismography. If drifts are clustered into a local area
of certain size, we call the clustered sediment group a Contourite
Depositional System (CDS). These sedimentary units are generally, but
not exclusively, found in the deep sea (\>2000m depth). They are a
useful tool for determining paleoceanic and paleoclimatic changes, since
their distribution is linked to bottom currents. They have gained
interest from the hydrocarbon industry, since accumulation of source
rocks may be influenced by bottom currents. Slope instability is also an
area of interest for further research.

Source methodology: For acquisition of the resulting information,
several hundreds of papers on the relevant subject were read. Firstly,
abstracts, introductions and conclusions were read for a general view on
the article. Then further reading on the relevant chapters provided the
presented information.

In 2013, 166 records with the place type Drift were added to the Marine
Gazetteer. These are linked to one of the 24 Contourite Depositional
Systems, if they make part of such a system. Each drift is also
influenced by a water mass or current. This water mass or current
determines the sedimentation, erosion and movements of these sediments.

In 2014, the list was extended with 81 records of the place type Drift.
No Contourite Depositional Systems were added. The search process was
identical to 2013, only this time a summary paper was the starting point
(Rebesco et al., 2014). In this publication, the major drifts were
listed with their most relevant sources along with them. These listed
sources were then sought after and read. Not all papers were accessible
at the time of this process, and thus it is advised to read these
publications in the future as well.

| layer | colname | type | definition |
|:---|:---|:---|:---|
| cds | mrgid | int | Marine Regions Geographic Identifier of the feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| cds | added_in | short | Year of designation of feature. |
| cds | \_\_xmin | double | Minimum longitude of feature in EPGS:4326 |
| cds | \_\_xmax | double | Maximum longitude of feature in EPGS:4326 |
| cds | ymin | double | Minimum latitude of feature in EPGS:4326 |
| cds | ymax | double | Maximum latitude of feature in EPGS:4326 |
| cds | centroid | string | Centroid of feature in EPGS:4326 |

### Emission Control Areas (ECAs) designated under regulation 13 of MARPOL Annex VI (NOx emission control)

Polygon data representing the Emission Control Areas (ECAs) designated
under regulation 13 of MARPOL Annex VI (NOx emission control). The
outline coordinates were processed in June 2019. The coastline was
extracted from the ESRI Countries 2014. Available ECAs: 1) North
American area (regulation 13.6.1 and appendix VII of MARPOL Annex VI);
2) United States Caribbean sea area (regulation 13.6.2 and appendix VII
of MARPOL Annex VI)

Preferred citation: Flanders Marine Institute (VLIZ), Belgium; (2020).
Emission Control Areas (ECAs) designated under regulation 13 of MARPOL
Annex VI (NOx emission control). Available online at
<http://www.marineregions.org/>. <https://doi.org/10.14284/396>

| layer         | colname    | type   | definition                   |
|:--------------|:-----------|:-------|:-----------------------------|
| eca_reg13_nox | area       | string | Name of feature.             |
| eca_reg13_nox | regulation | string | Legal regulation of feature. |

### Emission Control Areas (ECAs) designated under regulation 14 of MARPOL Annex VI (SOx and particulate matter emission control)

Polygon data representing the Emission Control Areas (ECAs) designated
under regulation 14 of MARPOL Annex VI (SOx and particulate matter
emission control). The outline coordinates were processed in June 2019.
The coastline was extracted from the ESRI Countries 2014. Available
ECAs: 1) Baltic Sea area (regulation 14.3.1 of MARPOL Annex VI and
regulation 1.11.2 of MARPOL Annex I); 2) North Sea area (regulation
14.3.1 of MARPOL Annex VI and regulation 1.14.6 of MARPOL Annex V); 3)
North American area (regulation 14.3.2 and appendix VII of MARPOL Annex
VI); 4) United States Caribbean sea area (regulation 14.3.3 and appendix
VII of MARPOL Annex VI)

Preferred citation: Flanders Marine Institute (VLIZ), Belgium; (2020).
Emission Control Areas (ECAs) designated under regulation 14 of MARPOL
Annex VI (SOx and particulate matter emission control). Available online
at <http://www.marineregions.org/>. <https://doi.org/10.14284/397>

| layer            | colname    | type   | definition                   |
|:-----------------|:-----------|:-------|:-----------------------------|
| eca_reg14_sox_pm | area       | string | Name of feature.             |
| eca_reg14_sox_pm | regulation | string | Legal regulation of feature. |

### UNESCO World Heritage Marine Sites (v02, 2023)

This file contains the shapefile of the 50 marine sites inscribed on the
UNESCO World Heritage List (as of 1 January 2023). Launched in 2005, the
mission of the World Heritage Marine Programme is to establish effective
conservation of existing and potential marine areas of Outstanding
Universal Value to make sure they will be maintained and thrive for
generations to come. In order to create the data, information from the
UNESCO World Heritage Marine Programme and Protected Planet were
collected and compiled.

Citable as data publication UNESCO (2023). Boundaries of UNESCO World
Heritage Marine Sites (v02). Available online at
<https://www.marineregions.org/>. <https://doi.org/10.14284/592>

| layer | colname | type | definition |
|:---|:---|:---|:---|
| worldheritagemarineprogramme | full_name | string | Name of feature. |
| worldheritagemarineprogramme | country | string | State in which the feature lies. |
| worldheritagemarineprogramme | year | string | Year of designation of feature. |
| worldheritagemarineprogramme | beauty | string | Boolean. Indicates if the designation of feature is based on landscape criteria. |
| worldheritagemarineprogramme | geology | string | Boolean. Indicates if the designation of feature is based on geological importance criteria. |
| worldheritagemarineprogramme | ecology | string | Boolean. Indicates if the designation of feature is based on ecological importance criteria. |
| worldheritagemarineprogramme | habitat | string | Boolean. Indicates if the designation of feature is based on ecological importance criteria. |
| worldheritagemarineprogramme | long_whc | string | Centroid longitude of feature in EPGS:4326 |
| worldheritagemarineprogramme | lat_whc | string | Centroid latitude of feature in EPGS:4326 |
| worldheritagemarineprogramme | source | string | Name of the authoritative source that defines the feature. |
| worldheritagemarineprogramme | buffer | string | Boolean. Indicates if the feature is a buffer around another feature. |
| worldheritagemarineprogramme | url | string | Uniform Resource Locator to the authoritative source that defines the feature. |
| worldheritagemarineprogramme | mrgid | string | Marine Regions Geographic Identifier of the feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| worldheritagemarineprogramme | refid | string | Unique Identifier of the feature. |
| worldheritagemarineprogramme | area_km2 | decimal | Area of feature in square kilometers. |

### The 66 Large Marine Ecosystems of the World

LMEs are natural regions of ocean space encompassing coastal waters from
river basins and estuaries to the seaward boundary of continental
shelves and the outer margins of coastal currents. They are relatively
large regions of 200,000 km2 or greater, the natural boundaries of which
are based on four ecological criteria: bathymetry, hydrography,
productivity, and trophically related populations. The theory,
measurement, and modeling relevant to monitoring the changing states of
LMEs are imbedded in reports on ecosystems with multiple steady states,
and on the pattern formation and spatial diffusion within ecosystems.
The concept that critical processes controlling the structure and
function of biological communities can best be addressed on a regional
basis has been applied to the ocean by using LMEs as the distinct units
for marine resources assessment, monitoring, and management.

| layer | colname | type | definition |
|:---|:---|:---|:---|
| lme | objectid | long | Unique identifier of feature. |
| lme | lme_name | string | Name of feature. |
| lme | grouping | string | Boolean: Indicates if the feature belongs to the category: Artic |
| lme | arctic | string | Boolean: Indicates if the feature belongs to the category: Artic |
| lme | uslmes | string | Boolean. Indicates if the feature is related to the nation: United States |
| lme | shape_leng | decimal | Perimeter of feature in meters calculated in spheroid. |
| lme | shape_area | decimal | Cartesian area of feature in square meters. |
| lme | sum_gis_km | decimal | Geodetic area of feature in square kilometers. |
| lme | mrgid | long | Marine Regions Geographic Identifier of the feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| lme | minlat | double | Minimum latitude of feature in EPGS:4326 |
| lme | minlong | double | Minimum longitude of feature in EPGS:4326 |
| lme | maxlat | double | Maximum latitude of feature in EPGS:4326 |
| lme | maxlong | double | Maximum longitude of feature in EPGS:4326 |
| lme | lat | double | Centroid latitude of feature in EPGS:4326 |
| lme | lon | double | Centroid longitude of feature in EPGS:4326 |
| lme | lme_number | long | Unique identifier of feature. |

### Marine Ecoregions of the World - Ecoregions

MEOW is a biogeographic classification of the world’s coasts and
shelves. It is the first ever comprehensive marine classification system
with clearly defined boundaries and definitions and was developed to
closely link to existing regional systems. The ecoregions nest within
the broader biogeographic tiers of Realms and Provinces.

MEOW represents broad-scale patterns of species and communities in the
ocean, and was designed as a tool for planning conservation across a
range of scales and assessing conservation efforts and gaps worldwide.
The current system focuses on coast and shelf areas (as this is where
the majority of human activity and conservation action is focused) and
does not consider realms in pelagic or deep benthic environment. It is
hoped that parallel but distinct systems for pelagic and deep benthic
biotas will be devised in the near future.

The project was led by The Nature Conservancy (TNC) and the World
Wildlife Fund (WWF), with broad input from a working group representing
key NGO, academic and intergovernmental conservation partners.

(source: WWF - Marine Ecoregions of the World)

Note: The inland boundaries of the ecoregions extend far inland - a
convention to ensure inclusion of any coastline and estuarine/lagoonal
systems which may be derived from different map sources. For
visualisations in the Marine Regions gazetteer, the areas inland have
been removed from the shapefile.

References: Spalding, M. D. Fox, H. E. Allen, G. R. Davidson, N.
Ferdana, Z. A. Finlayson, M. Halpern, B. S. Jorge, M. A. Lombana, A.
Lourie, S. A., (2007). Marine Ecoregions of the World: A
Bioregionalization of Coastal and Shelf Areas. Bioscience 2007, VOL 57;
numb 7, pages 573-584. doi: 10.1641/B570707

| layer | colname | type | definition |
|:---|:---|:---|:---|
| ecoregions | eco_code | long | Unique identifier of feature. |
| ecoregions | ecoregion | string | Name of feature. Areas of relatively homogeneous species composition, clearly distinct from adjacent systems. The species composition is likely to be determined by the predominance of a small number of ecosystems and/or a distinct suite of oceanographic or topographic features. The dominant biogeographic forcing agents defining the ecoregions vary from location to location but may include isolation, upwelling, nutrient inputs, freshwater influx, temperature regimes, ice regimes, exposure, sediments, currents, and bathymetric or coastal complexity. |
| ecoregions | lat | decimal | Centroid latitude of feature in EPGS:4326 |
| ecoregions | long | decimal | Centroid longitude of feature in EPGS:4326 |
| ecoregions | placetype | string | Type of feature. |
| ecoregions | mrgid | int | Marine Regions Geographic Identifier of the feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |

### SeaVoX - Sea Areas Polygons (v18, 2021)

SeaVoX is a combined SeaDataNet and MarineXML vocabulary content
governance group, it is moderated by BODC
(<https://www.bodc.ac.uk/data/codes_and_formats/seavox/>). This polygon
data set defines the geographic extent of the terms specified by the
SeaVoX vocabulary governance to describe coherent regions of the
hydrosphere. It includes land masses enclosing freshwater bodies. The
coastline data set used in the shapefile is taken from the World Vector
Shoreline data set (scale 1:250,000). Reference for the data set:
“polygon data set of the extent of water bodies from the SeaVoX Salt and
Fresh Water Body Gazetteer,
<http://vocab.nerc.ac.uk/collection/C19/current/>”. The data file
follows a hierarchical structure with each region consisting of one or
more polygons. This approach was adopted to avoid the need to have
overlapping polygons in regions where a sea area included a number of
sub-regions, for example the Mediterranean Sea includes the Aegean Sea,
Ionian Sea etc. The following gives the level in the structure at which
particular regions, which consist of more than one polygon can be found.
This level in the structure is given by the polygon’s attributes.
Attribute: REGION: ARCTIC OCEAN, ATLANTIC OCEAN,BALTIC SEA,INDIAN
OCEAN,MEDITERRANEAN REGION,PACIFIC OCEAN,SOUTH CHINA AND EASTERN
ARCHIPELAGIC SEAS,SOUTHERN OCEAN,MAINLAND NORTH
AMERICAMAINLAND,EUROPE,MAINLAND ASIA Attribute: LEVEL_1: ARAFURA
SEA,DAVIS SEA,GREENLAND SEA,GULF OF BOTHNIA,MEDITERRANEAN SEA,NORTH
ATLANTIC OCEAN,NORTH PACIFIC OCEAN,ROSS SEA,SOUTH ATLANTIC OCEAN,SOUTH
PACIFIC OCEAN,TIMOR SEA, LAURENTIAN GREAT LAKED Attribute: LEVEL_2:
MEDITERRANEAN SEA, WESTERN BASIN,MEDITERRANEAN SEA,EASTERN
BASIN,NORTHEAST ATLANTIC OCEAN (40W),NORTHEAST PACIFIC OCEAN
(180W),NORTHWEST ATLANTIC OCEAN (40W),NORTHWEST PACIFIC OCEAN
(180W),SOUTHEAST ATLANTIC OCEAN (20W),SOUTHEAST PACIFIC OCEAN
(140W),SOUTHWEST ATLANTIC OCEAN (20W),SOUTHWEST PACIFIC OCEAN (140W),
LAKE ERIE, LAKE SUPERIOR, DETROIT RIVER, ST. CLAIR RIVER, LAKE ST.
CLAIR, NIAGARA RIVER, LAKE HURON, LAKE ONTARIO, LAKE MICHIGAN, ST. MARYS
RIVER Attribute: LEVEL_3: BERING SEA,BRISTOL CHANNEL,CELTIC SEA,CORAL
SEA,ENGLISH CHANNEL,GULF OF MAINE,INNER SEAS OFF THE WEST COAST OF
SCOTLAND,IRISH SEA,JAPAN SEA,NORTH SEA,TASMAN SEA,YELLOW SEA Attribute:
LEVEL_4: CARDIGAN BAY,DOVER STRAIT,FIRTH OF CLYDE,LIVERPOOL BAY,NORTH
CHANNEL,POOLE BAY,SOLENT,SOLWAY FIRTH Attribute: SUB_REGION: This is the
lowest level in the structure. This version of the shapefile corresponds
to version 18 of the SeaVoX Salt and Fresh Water Body Gazetteer. This
version includes the following updates: addition of 1 new level 3 area
(Gulf of Maine) and 1 new sub-region (Gulf of Maine) Credits Polygon
data set of the extent of water bodies from the SeaVoX Salt and Fresh
Water Body Gazetteer, <http://vocab.nerc.ac.uk/collection/C19/current/>
Use limitations The source of the data set should be attributed as:
“polygon data set of the extent of water bodies from the SeaVoX Salt and
Fresh Water Body Gazetteer,
<http://vocab.nerc.ac.uk/collection/C19/current/>”

| layer | colname | type | definition |
|:---|:---|:---|:---|
| seavox_v18 | sub_region | string | Name of feature. |
| seavox_v18 | region | string | Name of the first hierarchical level of classification feature |
| seavox_v18 | level_1 | string | Name of the second hierarchical level of classification feature. |
| seavox_v18 | level_2 | string | Name of the third hierarchical level of classification feature. |
| seavox_v18 | level_3 | string | Name of the fourth hierarchical level of classification feature |
| seavox_v18 | level_4 | string | Name of the fifth hierarchical level of classification feature |
| seavox_v18 | skos_url | string | Uniform Resource Identifier of feature |
| seavox_v18 | lat | double | Centroid latitude of feature in EPGS:4326 |
| seavox_v18 | long | double | Centroid longitude of feature in EPGS:4326 |
| seavox_v18 | minlat | double | Minimum latitude of feature in EPGS:4326 |
| seavox_v18 | minlong | double | Minimum longitude of feature in EPGS:4326 |
| seavox_v18 | maxlat | double | Maximum latitude of feature in EPGS:4326 |
| seavox_v18 | maxlong | double | Maximum longitude of feature in EPGS:4326 |
| seavox_v18 | mrgid_sr | string | Marine Regions Geographic Identifier of the feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| seavox_v18 | mrgid_r | string | Marine Regions Geographic Identifier of the first hierarchical level of classification feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| seavox_v18 | mrgid_l1 | string | Marine Regions Geographic Identifier of the second hierarchical level of classification feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| seavox_v18 | mrgid_l2 | string | Marine Regions Geographic Identifier of the third hierarchical level of classification feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| seavox_v18 | mrgid_l3 | string | Marine Regions Geographic Identifier of the fourth hierarchical level of classification feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| seavox_v18 | mrgid_l4 | string | Marine Regions Geographic Identifier of the fifth hierarchical level of classification feature. Unique, persistent and resolvable identifier used in the Marine Regions gazetteer. |
| seavox_v18 | area_km2 | decimal | Geodetic area of feature in square kilometers. |
