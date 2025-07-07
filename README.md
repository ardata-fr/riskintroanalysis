
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

## Installation

You can install the development version of riskintroanalysis like so:

``` r
remotes::install_github("riskintroanalysis")
```

## Dev notes!!

Functions required:

1.  Calculate Risk of Introduction score from emission risk factors.
2.  Functions for validation dataset
3.  Functions for easily tidying datasets so they are valid
4.  Functions to go through each analysis (listed below) using validated
    datasets

See analysis examples to develop.

## Analysis

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

This an example of analysing the risk of introduction for Tunisian
governorates.

## Border length method:

Work in progress

``` r
# library(riskintroanalysis)
library(sf)
library(dplyr)

emission_risk <- riskintro:::emission_risk_factor_defaults

url <- "~/TUNISIA-EXAMPLE-01/Mobilite_animale/ANIMAL_MOBILITY.csv"
erf <- riskintro:::emission_risk_factor_defaults
tun <- sf::read_sf("~/TUNISIA-EXAMPLE-01/UniteEPI_ADM/TUN_ADM2.shp") |> 
  select(
    eu_id = fid,
    eu_name = shapeName,
    geometry = geometry
  )

# riskintrodata
world <- riskintro:::countries_sf_lowres

er <- filter(erf, disease == "Anthrax")
er <- get_weighted_emission_risk(erf, riskintro:::emission_risk_weights)
er_world <- left_join(
  x = world, y = er, 
  by = c("ISO3" = "iso3")
)

neighbours <- right_join(
  x = world, 
  y = riskintro:::neighbours_table, 
  by = c(ISO3 = "country_id")
) |> 
  filter(ISO3 == "TUN")


# Run function to get shared borders
shared_borders <- get_shared_borders(
  epi_units = tun,
  eu_id_col = "eu_id",
  bordering_countries = neighbours,
  bc_id_col = "BC_ID"
)

# Print output
print(shared_borders)

# Visualize using a simple plot
plot(st_geometry(epi_units), col = "lightblue", border = "blue", main = "Shared Borders")
plot(st_geometry(bordering_countries), col = "pink", border = "red", add = TRUE)
plot(st_geometry(shared_borders), col = "black", lwd = 2, add = TRUE)


```

## Entry point method:

``` r
# library(riskintroanalysis)
```

## Animal mobility method:

Considerations: 1. fix exports, need to export as much as possible. 1.
add “countries” package as dep and re-export country_name 1. Provide
tools to tidy data for analysis and validate datasets at the start of
each function. 1. Do we continue to include lealfet labels in output? 1.
create plot.generics for each output to a basic plot or leaflet? 1
Combine calc_animal_mobility_point_risk and calc_animal_mobility_eu_risk
into one function?

``` r
library(tidyverse)
library(sf)
library(riskintro)
library(countries)

emission_risk <- riskintro:::emission_risk_factor_defaults

url <- "~/TUNISIA-EXAMPLE-01/Mobilite_animale/ANIMAL_MOBILITY.csv"
erf <- riskintro:::emission_risk_factor_defaults
tun <- sf::read_sf("~/TUNISIA-EXAMPLE-01/UniteEPI_ADM/TUN_ADM2.shp") |> 
  select(
    eu_id = fid,
    eu_name = shapeName,
    geometry = geometry
  )

erf <- filter(erf, disease == "Anthrax")
erf <- get_weighted_emission_risk(erf, riskintro:::emission_risk_weights)

animal_mobility <- read_csv2(
  url,
  col_types = cols(
    ORIGIN_NAME = col_character(),
    ORIGIN_COUNTRY = col_character(),
    ORIGIN_LONGITUDE_X = col_character(),
    ORIGIN_LATITUDE_Y = col_number(),
    DESTINATION_NAME = col_character(),
    DESTINATION_COUNTRY = col_character(),
    DESTINATION_LONGITUDE_X = col_character(),
    DESTINATION_LATITUDE_Y = col_number(),
    HEADCOUNT = col_double()
  ))

colnames(animal_mobility) <- c(
  "o_name",
  "o_country",
  "o_lng",
  "o_lat",
  "d_name",
  "d_country",
  "d_lng",
  "d_lat",
  "quantity"
)

animal_mobility <- animal_mobility |> 
  mutate(o_iso3 = countries::country_name(x = o_country, to = "ISO3"),
         d_iso3 = countries::country_name(x = d_country, to = "ISO3"),
         .before = 1)

analysis_point <- calc_animal_mobility_point_risk(
  animal_mobility = animal_mobility, 
  emission_risk = erf, 
  country_iso3 = "TUN"
    )

analysis_output <- calc_animal_mobility_eu_risk(
  animal_mobility_points = analysis_point, 
  epi_units = tun, 
  method = "mean"
  )

# plot data ...

```

## Road accessibility method:

``` r
# library(riskintroanalysis)
```
