#' Initilise leaflet object
#'
#' @description
#' Initialises leaflet object with Stadia.Outdoors tiles and Esri.WorldImagery, and
#' controls required to switche between them. Also includes the map scale.
#'
#' Use `leaflet_add_*()`
#' functions to add more layers.
#'
#' @param ... arguments to pass on to [leaflet::leaflet()]
#' @return returns an interactive leaflet visualisation
#' @examples
#' basemap()
#' @importFrom leaflet addScaleBar leaflet addProviderTiles addLayersControl
#' @seealso [leaflet::leaflet()], [leaflet::addProviderTiles()],
#' [leaflet::addScaleBar()], [leaflet::addLayersControl]
#' @export
basemap <- function(...) {
  ll <- leaflet(
    ...,
    options = leafletOptions(
    doubleClickZoom = FALSE,
    scrollWheelZoom = TRUE
  ))
  ll <- ll |>
    addProviderTiles(leaflet::providers$Stadia.Outdoors, group = "Stadia") |>
    addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Esri") |>
    # addProviderTiles(leaflet::providers$OpenTopoMap, group = "Open Topo Map") |>
    addLayersControl(
      baseGroups = c("Stadia", "Esri"),
      options = layersControlOptions(collapsed = FALSE),
      position = "bottomright"
    ) |>
    addScaleBar(position = "bottomleft")
  ll
}


# Palette used for the 4 color risk categorisations in Leaflet
risk_palette <- function(){
  colorBin(
    "Spectral",
    domain = seq(0, 100, 10),
    bins = seq(0, 100, 25),
    na.color = "grey",
    reverse = TRUE
  )
}

# Risk legend used for the 4 color risk categorisations in Leaflet
addRiskLegend <- function(ll, title = "Risk score category"){

  pal <- risk_palette()

  dat <- data.frame(
    labels = c(
      "High [75, 100]",
      "Medium [50, 75)",
      "Low [25, 50)",
      "Negligable [0, 25)",
      "No information"
    ),
    colors = pal(c(75, 50, 25, 0, NA))
  )

  ll |>
    addLegend(
      data = dat,
      title = title,
      colors = ~colors,
      labels = ~labels,
      layerId = "intro_risk_legend"
    )
}

# Palette used for the 4 color risk categorisations in Leaflet
risk_palette_12 <- function(){
  colorBin(
    "Spectral",
    domain = seq(0, 100, 10),
    bins = seq(0, 12, 3),
    na.color = "grey",
    reverse = TRUE
  )
}

#' @importFrom leaflet labelOptions
riLabelOptions <- function(){

  labelOptions(
    textsize = "15px"
  )
}

generate_leaflet_labels <- function(dat) {
  dat <- st_drop_geometry(dat)

  dat <- purrr::map_df(
    dat,
    function(x){
      if (is.double(x)) {
        sprintf("%.2f", x) # round doubles
      } else if (is.character(x)){
        sprintf("%.47s", x) # truncate long strings
      } else {
        x
      }
    }
  )

  labels <- pmap(
    as.list(dat),
    ~ {
      paste0(
        mapply(
          function(name, value) {
            paste0("<strong>", textify(name), ":</strong> ", value, "<br>")
          },
          names(dat), c(...)
        ),
        collapse = ""
      )
    }
  ) |>
    map(HTML)

  labels
}


