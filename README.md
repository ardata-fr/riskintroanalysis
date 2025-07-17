
<!-- README.md is generated from README.Rmd. Please edit that file -->

# riskintroanalysis

<!-- badges: start -->

<!-- badges: end -->

The riskintroanalysis R package provides functions to analyse the risk
of introduction of animal diseases within a geographic area. It is
intended to be useful as a stand-alone package, but also to integrate
directly into the riskintro Rshiny app. The motivation behind these
projects is to easily conduct geospatial risk analysis using existing
data from WAHIS (World Animal Health Information System).

# Installation

You can install the development version of riskintroanalysis like so:

``` r
remotes::install_github("riskintroanalysis")
```

# Analysis

The package provides functions for certain risk of introduction
analysis, but requires external data to be provided. The functions are
inteded to be flexible, but each analytical method requires the
corresponding data.

The RShiny app [riskintro](https://github.com/ardata-fr/riskintro)
provides a graphical interface for the following analyses.

The central datasets to each method are:

1.  **The emission risk score table**, which provides a risk of emission
    diseases (that is to say the risk of a disease spreading from that
    country). This score is based on summary of risk factors, many of
    which are available from WAHIS (see more
    [here](insert%20link%20to%20riksintrodata%20package%20docs)). This
    data exists for many countries around the world, and is augmented
    with geospatial data corresponding to each country’s administrative
    boundaries.
2.  **The area of interest**, Geospatial data defining the borders of a
    region. This is the area for which we want to know the risk of
    introduction of a disease. The most common data is that of a country
    and its administrative boundaries, including internal boundaries.
    For example, French departments, or German states.

These two datasets are compared against each other through the following
analyses:

1.  **Border lengths**: using the length of shared borders the risk of
    introduction is weighed comared to each neighbouring country and
    their risk score.
2.  **Border entry points**: using an additional dataset that defines
    broder entry points, risk of introduction is weighted by legality of
    entry point.
3.  **Animal mobility**: using an additional dataset defining legal
    animal commerce flows, risk of introduction is weighed based on the
    number of animals entering the area.
4.  **Road access risk**: using raster data of the world, applied to the
    area of interest, risk of introdcution is infered through road
    acces. An area more accessible by road is considered higher risk.

These methods of analysis are intended to be used together or
individually to montor is risk of introduction.

# Example 1: Tunisia

`riskintroanalysis` is intended to be used alongside `sf` geospatial
data manipulation, `dplyr` for general data manipulation, and `terra`
for raster data.

## Core datasets

The two core datasets are emission risk factors and epidemiological
units. The first is used in three out of four analysis methods, the
second is used in all of them.

### Emission Risk Factors:

Emission risk factors are used to create an overall emission risk value
that is associated with the possible spread of a disease from that
country. It is made up of multiple scores which are weighted.

Here, we create two entries in the emission risk table for Algeria and
Libya. The absence of government oversight in each area represents an
increased risk of emission. Therefore a 1 means there is additional risk
associated with this score, and a 0 means no additional risk.

``` r

library(dplyr)
library(riskintrodata)
library(riskintroanalysis)
#> Loading required package: ggplot2
library(sf)

algeria <- erf_row(
  iso3 = "DZA",
  country = "Algeria",
  disease = "Avian infectious laryngotracheitis",
  animal_category = "Domestic",
  species = "Birds",
  disease_notification = 0,
  targeted_surveillance = 1,
  general_surveillance = 0,
  screening = 1,
  precautions_at_the_borders = 1,
  slaughter = 1,
  selective_killing_and_disposal = 1,
  zoning = 1,
  official_vaccination = 1,
  last_outbreak_end_date = as.Date("30/06/2023"),
  commerce_illegal = 0L,
  commerce_legal = 0L
)
#> ✔ All data in "emission_risk_factors" valided.

libya <- erf_row(
  iso3 = "LBY",
  country = "Libya",
  disease = "Avian infectious laryngotracheitis",
  animal_category = "Domestic",
  species = "Birds",
  disease_notification = TRUE,
  targeted_surveillance = 1,
  general_surveillance = 0,
  screening = 1,
  precautions_at_the_borders = 0,
  slaughter = 1,
  selective_killing_and_disposal = 1,
  zoning = 1,
  official_vaccination = 1,
  last_outbreak_end_date = as.Date("30/06/2019"),
  commerce_illegal = 0L,
  commerce_legal = 1
)
#> ✔ All data in "emission_risk_factors" valided.

wahis_erf <- get_wahis_erf(
    disease = "Avian infectious laryngotracheitis",
    animal_category = "Domestic",
    species = "Birds"
  )
#> ✔ All data in "emission_risk_factors" valided.

emission_risk_factors <- dplyr::bind_rows(
  algeria, 
  libya,
  wahis_erf
  )

emission_risk_table <- calc_emission_risk(emission_risk_factors = emission_risk_factors)

hist(
  emission_risk_table$emission_risk,
  main = "Histogram of emission risk score",
  xlab = "Emission risk scores"
)
```

<img src="man/figures/README-emission-risk-loading-1.png" width="100%" />

It is also useful to join the world_sf geospatial dataset to emission
risk for later.

``` r
world <- riskintrodata::world_sf

emission_risk_sf <- dplyr::left_join(
  x = world, y = emission_risk_table, 
  by = c("iso3" = "iso3")
)

plot(select(emission_risk_sf, emission_risk), 
     main = "Emission risk scores")
```

<img src="man/figures/README-display-world-1.png" width="100%" />

### Epidemiological Units

Epidemiological units (or epi units for short) is the term used for
geographical areas of interests for analysing understand the risk of
introduction. This data has no defaults and needs to be provided by the
user, however packages like `geodata` can be used to get them. For this
example, a shapefile containing the administrative boundaries of Tunisia
exists in the `riskintrodata` package for demonstration purposes.

The Tunisia epidemiological units dataset will be essential to the
following analysis methods.

``` r
library(sf)
# Example with raw sf files, previously downloaded with geodata::gadm()
tunisia_raw <- read_sf(system.file(
  package = "riskintrodata",
  "samples", "tunisia", "epi_units", "tunisia_adm2_raw.gpkg"
   ))

# Apply mapping to prepare colnames and validate dataset
tunisia <- apply_mapping(
  tunisia_raw,
  mapping = mapping_epi_units(
    eu_name = "NAME_2",
    geometry = "geom"
  ),
  validate = TRUE
)
#> ✔ All data in "epi_units" valided.

plot(sf::st_geometry(tunisia))
```

<img src="man/figures/README-epi-units-1.png" width="100%" /> What’s
left after using `apply_mapping()` is the required “epi_units” dataset
that is compatible with the rest of the analysis going forward.

## Analysis

The 4 methods of analysis provided by the package are detailed below:

This an example of analysing the risk of introduction for Tunisian
governorates. Based off of the core datasets

### Border risk

Required datasets:

1.  epi units
2.  internal neighbours table
3.  emission risk table

The border length method calculates the risk of introduction of an
animal disease for a certain species based on shared land borders. In
this example, we see Tunisia shares land borders with Algeria and Libya
(this is why entries of these two countries were added to the emission
risk table above).

Risk of introduction through land borders is the risk of emission from
of bordering countries weighted by the length of the shared border. If
an epi unit only has one bordering country with emission risk X, then
the epi units risk of introduction is also X. If two bordering countries
exist with emission risk Y ans Z, then the risk of introduction is a
weighted average of Y and Z, with more importance given to the country
with the longer border.

The analysis method starts with finding shared borders between epi units
and bordering countries, which requires an algoithm align borders.

``` r
tun_neighbours <- riskintrodata::neighbours_table |> 
  filter(country_id == "TUN")

bordering_countries <- world |> 
  filter(iso3 %in% tun_neighbours$neighbour_id)

# Run function to get shared borders
shared_borders <- calc_border_lengths(
  epi_units = tunisia,
  eu_id_col = "eu_id",
  bordering_countries = bordering_countries,
  bc_id_col = "iso3"
)
#> Warning: attribute variables are assumed to be spatially constant throughout
#> all geometries
#> Warning: attribute variables are assumed to be spatially constant throughout
#> all geometries
#> Spherical geometry (s2) switched off
#> although coordinates are longitude/latitude, st_union assumes that they are
#> planar

ggplot() +
  geom_sf(data = bordering_countries, colour = "red", fill = "pink", alpha = 0.5) +
  geom_sf(data = tunisia, fill = "lightblue", colour = "blue", alpha = 0.5) +
  geom_sf(data = shared_borders, colour = "black") +
  geom_sf_label(data = shared_borders, aes(label = round(border_length)), size = 1.5) +
  coord_sf(xlim = c(7, 12), ylim = c(30,38), expand = FALSE)
#> Warning in st_point_on_surface.sfc(sf::st_zm(x)): st_point_on_surface may not
#> give correct results for longitude/latitude data
```

<img src="man/figures/README-shared-borders-1.png" width="100%" /> The
main purpose of the function is to correct misaligned borders, as can be
seen in the overlaps and divergent borders. The epidemiological units
data is used as the “true” border as geospatial data from different
sources are rarely well aligned.

A border length has been calculated for each epi unit that a foreign
country. For most of these areas, the risk of introduction will simply
equal the emission risk of the neighbouring country, with the exception
of Tunisia’s southern most governorate. Which will have an emission risk
weighed by the border lengths or each neighbour.

``` r
border_risk_results <- calc_border_risk(
  epi_units = tunisia, 
  shared_borders = shared_borders,
  emission_risk = emission_risk_table
  )

borders <- extract_border(border_risk_results)
ri_borders <- extract_intro_risk(border_risk_results)
ggplot() +
  geom_sf(
    data = ri_borders, 
    aes(fill = border_risk),
    alpha = 0.5
  ) +
  scale_color_viridis_c(limits = c(0, 12)) +
  geom_sf(
    data = borders,
    aes(color = border_risk),
    linewidth =  2
  ) +
  scale_fill_viridis_c(limits = c(0, 12)) +
  geom_sf_label(
    data = borders,
    aes(label = round(border_risk, 2)),
    size = 5/.pt, vjust = 2
  ) +
    geom_sf_label(
    data = 
      filter(
      ri_borders,
      eu_name %in% c("Faouar", "Ben Guerdane", "Dhiba", "Remada")
    ),
    aes(label = paste(eu_name, "\n", round(border_risk, 2))),
    size = 5/.pt, fill = "lightblue", alpha = 0.6
  )
#> Warning in st_point_on_surface.sfc(sf::st_zm(x)): st_point_on_surface may not
#> give correct results for longitude/latitude data
#> Warning in st_point_on_surface.sfc(sf::st_zm(x)): st_point_on_surface may not
#> give correct results for longitude/latitude data
```

<img src="man/figures/README-border-risk-analysis-1.png" width="100%" />

We can see that Remada has a risk of introduction weighted by border
lengths. The numbers shown along the borders are the emission risk
weighted by border lengths with neighbouring countries of Algeria and
Libya. As on Remada shares borders with both countries, it is the only
one with a weighted average.

### Entry point risk

``` r
library(riskintroanalysis)
library(dplyr)

entry_points_fp <-
    system.file(
      package = "riskintrodata",
      "samples",
      "tunisia",
      "entry_points", "BORDER_CROSSING_POINTS.csv"
    )

entry_points <- readr::read_csv(entry_points_fp)
#> Rows: 110 Columns: 6
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (4): NAME, TYPE, MODE, SOURCES
#> dbl (2): LONGITUDE_X, LATITUDE_Y
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

entry_points <- apply_mapping(
  dataset = entry_points, 
  mapping = mapping_entry_points(
    point_name = "NAME", 
    lng = "LONGITUDE_X", 
    lat = "LATITUDE_Y", 
    mode = "MODE", 
    type = "TYPE", 
    sources = "SOURCES"
  ), 
  validate = TRUE
)
#> ✔ All data in "entry_points" valided.

results <- calc_entry_point_risk(
  entry_points = entry_points,
  epi_units = tunisia,
  emission_risk = emission_risk_table
)

point_risk <- extract_point_risk(results)
ri_entry_points <- extract_intro_risk(results)

ggplot() +
  geom_sf(
    data = ri_entry_points,
    aes(fill = entry_points_risk ),
    alpha = 0.6
  ) +
  geom_sf(
    data = point_risk,
    size = 1.5, shape = 21,
    aes(fill = emission_risk)
  ) +
  scale_fill_viridis_c(limits = c(0, 12)) 
```

<img src="man/figures/README-entry-points-analysis-1.png" width="100%" />

All epi units containing an entry point (with an associated weighted
emission risk) now have a risk of introduction based on the points
within its area.

### Animal mobility

Considerations:

1.  fix exports, need to export as much as possible.
2.  add “countries” package as dep and re-export country_name
3.  Provide tools to tidy data for analysis and validate datasets at the
    start of each function.
4.  Do we continue to include lealfet labels in output?
5.  create plot.generics for each output to a basic plot or leaflet?
6.  Combine calc_animal_mobility_point_risk and
    calc_animal_mobility_eu_risk into one function?

``` r
library(riskintroanalysis)
animal_mobility_fp <- system.file(package = "riskintrodata", "samples", "tunisia", "animal_mobility", "ANIMAL_MOBILITY_raw.csv")

animal_mobility_raw <- readr::read_csv(animal_mobility_fp)
#> Rows: 112 Columns: 11
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (6): ORIGIN_NAME, ORIGIN_COUNTRY, ORIGIN_ISO3, DESTINATION_NAME, DESTINA...
#> dbl (5): ORIGIN_LONGITUDE_X, ORIGIN_LATITUDE_Y, DESTINATION_LONGITUDE_X, DES...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

animal_mobility <- apply_mapping(
  animal_mobility_raw,
  mapping = mapping_animal_mobility(
    o_name = "ORIGIN_NAME",
    o_lng = "ORIGIN_LONGITUDE_X",
    o_lat = "ORIGIN_LATITUDE_Y",
    d_name = "DESTINATION_NAME",
    d_lng = "DESTINATION_LONGITUDE_X",
    d_lat = "DESTINATION_LATITUDE_Y",
    quantity = "HEADCOUNT"
  )
)
#> ✔ All data in "animal_mobility" valided.

animal_mobility_risk <- calc_animal_mobility_risk(
  animal_mobility = animal_mobility, 
  emission_risk = emission_risk_table, 
  epi_units = tunisia,
  method = "mean"
)

flows_risk <- extract_flow_risk(animal_mobility_risk)
ri_animal_mobility <- extract_intro_risk(animal_mobility_risk)

ggplot() +
  geom_sf(
    data = ri_animal_mobility,
    aes(geometry = geometry, fill = animal_mobility_risk)
  ) +
  scale_fill_viridis_c(limits = c(0, 12))+
  geom_sf(
    data = flows_risk, 
    color = "black", size = 1.5, shape = 21,
    aes(fill = emission_risk_weighted )
  )
```

<img src="man/figures/README-animal-mobility-analysis-1.png" width="100%" />

Some points remain grey indicating that the country from which the flow
comes does not have an entry in the emission risk factors table.

### Road accessibility

Road access risk is calculated from the road accessibility raster file,
a global raster that contains data of distance from roads. This data is
aggregated over each epidemiological unit of Tunisia.

``` r
library(riskintroanalysis)
library(dplyr)
library(terra)
#> terra 1.8.54

road_raster_fp <- riskintrodata::download_road_access_raster()
road_raster <- terra::rast(road_raster_fp)
terra::plot(terra::crop(road_raster, tunisia, mask = TRUE)) 
```

<img src="man/figures/README-display-raster-1.png" width="100%" />

This shows us what the raster data looks like for Tunisia. Now we will
aggragate these values over each administrative area. Here, mean is
used.

``` r
ri_road_access <- augment_epi_units_with_raster(
  epi_units = tunisia, 
  raster = road_raster, 
  risk_name = "road_access_risk",
  aggregate_fun = "mean"
)

plot(select(ri_road_access, road_access_risk))
```

<img src="man/figures/README-do-augment_epi_units_with_raster-1.png" width="100%" />

The output shows the road access risk associated with each area.

## Risk rescaling

Once all the analysis methods have given a result, we need to be able to
compare them. Notably, road access risk is not on the same 0 to 12 scale
as the other three analyses. You might also want to add your own risk
scores using other methods.

To make sure we can compare across risks, riskintro proposes the
`rescale_risk` function, whose primary purpose is to normalise risk
scores to a range between 0 and 100. Its secondary role is to transform
the risk distribution using quadratic, exponential or sigmoid functions
(and inverse versions of these). Other options include reversing the
scaling as well.

A linear transformation looks like the following. This uses the maximum
value of `road_access_risk` as the top of the range, whereas it could be
better to take the global maximum in the raster if you are intending to
comparing across countries later.

``` r
rescaled <- rescale_risk_scores(
  data = ri_road_access,
  cols = "road_access_risk", 
  names_to = "scaled_road_access_risk",
  from = c(0, max(ri_road_access$road_access_risk)),
  to = c(0, 100),
  method = "linear", 
  keep_cols = TRUE
)

plot(rescaled$road_access_risk, rescaled$scaled_road_access_risk)
```

<img src="man/figures/README-rescaling-road-access-risk-1-1.png" width="100%" />

While a sigmoid transformation looks like the below. The choice of the
`method` argument are left up to the judgement of the analyst.

``` r
ri_road_access <- rescale_risk_scores(
  data = ri_road_access,
  cols = "road_access_risk", 
  names_to = "scaled_road_access_risk",
  from = c(0, max(ri_road_access$road_access_risk)),
  method = "sigmoid", 
  keep_cols = TRUE
)

plot(ri_road_access$road_access_risk, ri_road_access$scaled_road_access_risk)
```

<img src="man/figures/README-rescaling-road-access-risk-2-1.png" width="100%" />

## Summary table

Once all the risk methods have been compiled and scaled similarly, it
can be handy to have them all in one table. Each row is an
epidemiological unit and its various risks scores.

``` r

summary_table <- tunisia |> 
  left_join(
    st_drop_geometry(ri_road_access), 
    by = c("eu_id", "eu_name")
  ) |> 
  left_join(
    st_drop_geometry(ri_entry_points), 
    by = c("eu_id", "eu_name")
  ) |> 
  left_join(
    st_drop_geometry(ri_borders), 
    by = c("eu_id", "eu_name")
  ) |> 
  left_join(
    st_drop_geometry(ri_animal_mobility), 
    by = c("eu_id", "eu_name")
  ) |> 
  select(
    eu_id, eu_name,
    ri_road_access = scaled_road_access_risk,
    ri_entry_points = entry_points_risk,
    ri_border_risk = border_risk,
    ri_animal_movement = animal_mobility_risk
  ) |>
  rescale_risk_scores(
    cols = c(
      EP = "ri_entry_points", 
      BORDER = "ri_border_risk", 
      AM = "ri_animal_movement"), 
    method = "linear", 
    from = c(0, 12),
    to = c(0, 100),
    names_prefix = "RISK__",
    keep_cols = FALSE
  )

risk_cols <- grepv(pattern = "RISK__", x = colnames(summary_table))

combined_risk_table <- summary_table |> 
  summarise_risk_scores(cols  = risk_cols, method = "mean")

ggplot() +
  geom_sf(
    data = combined_risk_table,
    aes(fill = overall_risk)
    ) +
    scale_fill_viridis_c(limits = c(0, 100)) +
  theme_void()
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />
