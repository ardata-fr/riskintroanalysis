#' @export
#' @rdname plot_risk
plot_risk_interactive <- function(
    dataset,
    risk_name = NULL,
    risk_col = NULL,
    scale = NULL,
    ll = basemap()
){

  table_name <- risk_name %||% attr(dataset, "table_name")
  risk_col <- risk_col %||% attr(dataset, "risk_col")
  scale <- scale %||% attr(dataset, "scale")

  cli_abort_if_not(
    "{.arg table_name} is NULL and not in attributes of {.arg dataset}" = !is.null(table_name),
    "{.arg risk_col} is NULL and not in attributes of {.arg dataset}" = !is.null(risk_col),
    "{.arg scale} is NULL and not in attributes of {.arg dataset}" = !is.null(scale)
  )

  plot_interactive_fun <- switch(
    table_name,
    "entry_points" = plot_entry_points_interactive,
    "animal_mobility" = plot_animal_mobility_interactive,
    "road_access" = plot_road_access_interactive,
    "border_risk" = plot_border_risk_interactive,
    "epi_units" = plot_epi_units_interactive,
    "risk_table" = plot_risk_table_interactive,
    cli_abort("This risk is not supported.")
  )

  plot_interactive_fun(
    dataset = dataset,
    scale = scale,
    risk_col = risk_col,
    ll = ll
  )
}


#' Legends and palettes for RiskIntro leaflets
#'
#' Interactive plots use these pallettes and legends. Risks use the viridis
#' palette while scores use the inferno palette. These functions are built on
#' [leaflet::colorNumeric()] and [leaflet::addLegend()].
#' @param ll leaflet object or proxy to add legend to.
#' @inheritParams leaflet::addLegend
#' @param scale scale used for domain.
#' @name riskintro-leaflet-legends
NULL

#' @rdname riskintro-leaflet-legends
#' @export
riskPalette <- function(scale = c(0,100)){
  pal <- leaflet::colorNumeric(
    palette = "viridis",
    domain = scale,
    na.color = "#ededed"
  )
  pal
}

#' @rdname riskintro-leaflet-legends
#' @export
addRiskLegend <- function(ll, scale, title, opacity = 0.7, layerId = "risk_legend"){
  addLegend_decreasing(
    map = ll,
    pal = riskPalette(scale = scale),
    values = scale,
    opacity = opacity,
    title = title,
    layerId = layerId # prevents stacking legends
  )
}

#' @rdname riskintro-leaflet-legends
#' @export
scorePalette <- function(scale = c(0,100)){
  pal <- leaflet::colorNumeric(
    palette = "inferno",
    domain = scale,
    na.color = "#ededed"
  )
  pal
}

#' @rdname riskintro-leaflet-legends
#' @export
addScoreLegend <- function(
    ll,
    scale = c(0, 100),
    title = "Score",
    opacity = 0.7,
    layerId = "score_legend"
    ){
  addLegend_decreasing(
    map = ll,
    pal = scorePalette(scale= scale),
    values = scale,
    opacity = opacity,
    title = title,
    layerId = layerId # prevents stacking legends
  )
}


#' Interactive leaflet plotting for entry points
#'
#' @param dataset sf object with entry point risk data
#' @param scale numeric vector of length 2 defining the risk scale
#' @param risk_col character string naming the risk column to visualize
#' @return leaflet map object
#' @export
#' @rdname plot_risk
#' @importFrom leaflet leaflet addTiles addPolygons addCircleMarkers colorNumeric addLegend
plot_entry_points_interactive <- function(dataset, scale, risk_col, ll = basemap()) {

  pal <- scorePalette(scale)
  ll <- leaflet::clearMarkers(ll)
  ll <- ll |> leaflet::addPolygons(
    data = dataset,
    fillColor = pal(dataset[[risk_col]]),
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.75,
    label = generate_leaflet_labels(
      dat = dataset,
      title_field = "eu_name",
      special_fields = setNames("/100" , nm = risk_col),
      rename = list("Exposure (controlled)" = "exposure_C",
                     "Exposure (non-controlled)" = "exposure_NC"),
      arrange = c("eu_name", risk_col, "exposure_C", "exposure_NC"),
      exclude = c("eu_id", "user_id")
    ),
    layerId = dataset$eu_id
  )
  ll <- ll |>
    addScoreLegend(scale, title = "Introduction risk", layerId = "score_legend")

  if (!is.null(extract_point_risk(dataset))) {
    points_data <- extract_point_risk(dataset)
    points_risk_col <- attr(points_data, "risk_col")
    points_scale <- c(min(points_data[[points_risk_col]]), max(points_data[[points_risk_col]]))
    pal <- riskPalette(scale = points_scale)
    ll <- ll |>
      leaflet::addCircleMarkers(
        data = points_data,
        radius = 6,
        color = "black",
        weight = 2,
        fillColor = pal(points_data[[points_risk_col]]),
        fillOpacity = 0.8,
        label = attr(points_data, "leaflet_labels"),
        layerId = points_data$point_id
      )
    ll <- ll |>
      addRiskLegend(scale = points_scale, title = "Entry points", layerId = "points_legend")

  } else {
    cli_warn("No point data provided, it will not be plotted.")
  }

  ll
}

#' Interactive leaflet plotting for animal mobility
#'
#' @param dataset sf object with animal mobility risk data
#' @param scale numeric vector of length 2 defining the risk scale
#' @param risk_col character string naming the risk column to visualize
#' @param ll leaflet object to add layers to
#' @return leaflet map object
#' @export
#' @rdname plot_risk
#' @importFrom leaflet addPolygons addCircleMarkers
plot_animal_mobility_interactive <- function(dataset, scale, risk_col, ll = basemap()) {

  pal <- scorePalette(scale)
  ll <- ll |>
    leaflet::addPolygons(
      data = dataset,
      fillColor = ~pal(get(risk_col)),
      weight = 1,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.75,
      label = paste0(
        "<strong>", dataset$eu_name, "</strong>", "<br>",
        "<strong>", "Animal mobility risk score: ",
        fmt_num(dataset$animal_mobility_risk), "/", scale[[2]], "</strong>"
      ) |> map(HTML)
    )

  if (!is.null(extract_flow_risk(dataset))) {
    flows_data <- extract_flow_risk(dataset)
    ll <- ll |>
      leaflet::addCircleMarkers(
        data = flows_data,
        radius = 6,
        color = "black",
        weight = 2,
        fillColor = ~pal(flows_data$emission_risk_weighted),
        fillOpacity = 0.8,
        label = paste0(
          "<strong>", flows_data$d_name ,"</strong>", "<br>",
          "Animal flow emission risk score: ",
          "<strong>", fmt_num(flows_data$emission_risk_weighted), "/", scale[[2]], "</strong>", "<br>",
          "Countributing risk sources:", "<br>",
          flows_data$source_label)
        |> map(HTML)
      )
  } else {
    cli_warn("No flows data provided, it will not be plotted.")
  }

  ll <- ll |>
    addScoreLegend(scale, title = risk_col)
  ll
}

#' Interactive leaflet plotting for road access
#'
#' @param dataset sf object with road access risk data
#' @param scale numeric vector of length 2 defining the risk scale
#' @param risk_col character string naming the risk column to visualize
#' @param ll leaflet object to add layers to
#' @return leaflet map object
#' @export
#' @rdname plot_risk
#' @importFrom leaflet
#'  addPolygons addRasterImage addLayersControl layersControlOptions
#'  addLegend colorNumeric
#' @importFrom htmlwidgets onRender
plot_road_access_interactive <- function(dataset, scale, risk_col, ll = basemap()) {

  pal <- scorePalette(scale)
  # Create label content for polygons
  label_content <- paste0(
    "<strong>", dataset$eu_name, "</strong>", "<br>",
    "Index average: ", fmt_num(dataset$road_access_risk), "<br>",
    "Risk score: ", fmt_num(dataset[[risk_col]]), "/", scale[[2]]
  ) |>
    map(HTML)


  group1 <- "Risk"
  group2 <- "Source"

  # Add polygons with group name for layer control
  ll <- ll |>
    leaflet::addPolygons(
      data = dataset,
      fillColor = ~pal(get(risk_col)),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      label = label_content,
      group = group1
    )

  ll <- ll |>
    addLegend_decreasing(
      pal = scorePalette(scale),
      values = scale,
      title = risk_col,
      opacity = 0.7,
      # layerId = group1,
      group = group1,
      className = paste("info legend", group1)
    )

  # Add raster layer if available
  raster_data <- attr(dataset, "raster")
  if (!is.null(raster_data)) {

    raster_scale <- attr(raster_data, "scale")

    raster_pal <- leaflet::colorNumeric(
      palette = "viridis",
      domain = raster_scale,
      na.color = "#ededed"
    )

    ll <- ll |>
      leaflet::addRasterImage(
        x = raster_data,
        colors = raster_pal,
        opacity = 0.85,
        group = group2
      )

    ll <- ll |>
      addLegend_decreasing(
        pal = raster_pal,
        values = raster_scale,
        title = "Road Access Index",
        opacity = 0.85,
        # layerId = group2,
        group = group2,
        className = paste("info legend", group2)
      )
  }


  ll <- ll |>
    leaflet::addLayersControl(
      baseGroups = c(group1, group2),
      options = leaflet::layersControlOptions(
        collapsed = FALSE,
        autoZIndex = FALSE
      ),
      position = "bottomright"
    )

  # Make legend change on control radio button click
  ll <- ll |>

    # Doesnt work, shows no legends

    # htmlwidgets::onRender("
    # function(el, x) {
    #   var updateLegend = function () {
    #       var selectedGroup = document.querySelectorAll('input:checked')[0].nextSibling.innerText.substr(1);
    #
    #       document.querySelectorAll('.legend').forEach(a => a.hidden=true);
    #       document.querySelectorAll('.legend').forEach(l => {
    #         if (l.children[0].children[0].innerText == selectedGroup) l.hidden=false;
    #       });
    #   };
    #   updateLegend();
    #   this.on('baselayerchange', e => updateLegend());
    # }")


    # Doesn't work, shows both legends always.

  htmlwidgets::onRender("
      function(el, x) {
         var map = this;
         var updateLegend = function () {
            var checkedInput = document.querySelector('input[type=\"radio\"]:checked');
            if (!checkedInput) return;

            var selectedGroup = checkedInput.nextSibling.innerText.substr(1);

            document.querySelectorAll('.legend').forEach(function(legend) {
               legend.style.display = 'none';
               if (legend.classList.contains(selectedGroup)) {
                  legend.style.display = 'block';
               }
            });
         };
         updateLegend();
         this.on('baselayerchange', function(e) {
            updateLegend();
         });
      }"
  )

  # Works in Rmd, but not in RShiny with proxy or no proxy

#     htmlwidgets::onRender("
#     function() {
#       var map = this;
#       var legends = map.controls._controlsById;
#       function addActualLegend() {
#          var sel = $('.leaflet-control-layers-base').find('input[type=\"radio\"]:checked').siblings('span').text().trim();
#          $.each(map.controls._controlsById, (nm) => map.removeControl(map.controls.get(nm)));
#          map.addControl(legends[sel]);
#       }
#       $('.leaflet-control-layers-base').on('click', addActualLegend);
#       addActualLegend();
#    }")

  ll
}

#' Interactive leaflet plotting for border risk
#'
#' @param dataset sf object with border risk data
#' @param scale numeric vector of length 2 defining the risk scale
#' @param risk_col character string naming the risk column to visualize
#' @param ll leaflet object to add layers to
#' @return leaflet map object
#' @export
#' @rdname plot_risk
#' @importFrom leaflet addPolygons addPolylines clearShapes
plot_border_risk_interactive <- function(dataset, scale, risk_col, ll = basemap()) {

  pal <- scorePalette(scale)

  ll <- clearShapes(ll)
  ll <- ll |> leaflet::addPolygons(
    data = dataset,
    fillColor = pal(dataset[[risk_col]]),
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.8,
    label = paste0(
      "<strong>", dataset$eu_name, "</strong>", "<br>",
      "<strong>", "Border risk score: ", fmt_num(dataset$border_risk), "/", scale[[2]], "</strong>"
    ) |> map(HTML)
  )

  ll <- ll |>
    addScoreLegend(scale, title = "Introduction risk")

  if (!is.null(extract_border(dataset))) {
    borders_data <- extract_border(dataset)
    borders_scale <- attr(borders_data, "scale")
    borders_pal <- riskPalette(borders_scale)
    ll <- ll |>
      leaflet::addPolylines(
        data = borders_data,
        color = pal(borders_data$border_risk),
        weight = 6,
        opacity = 1,
        label = attr(borders_data, "leaflet_labels")
      )

    ll <- ll |>
      addRiskLegend(borders_scale, title = "Emission risk")
  } else {
    cli_warn("No border data provided, it will not be plotted.")
  }

  ll
}

#' Interactive leaflet plotting for epidemiological units
#'
#' @param dataset sf object with epidemiological units data
#' @param scale numeric vector of length 2 defining the risk scale
#' @param risk_col character string naming the risk column to visualize
#' @param ll leaflet object to add layers to
#' @return leaflet map object
#' @export
#' @rdname plot_risk
#' @importFrom leaflet addPolygons
plot_epi_units_interactive <- function(dataset, scale, risk_col, ll = basemap()) {

  ll <- ll |> leaflet::addPolygons(
    data = dataset,
    weight = 2,
    opacity = 1,
    color = "black",
    fillOpacity = 0,
    label = dataset$eu_name
  )

  ll
}

#' Interactive leaflet plotting for risk table
#'
#' @param dataset sf object with risk table data
#' @param scale numeric vector of length 2 defining the risk scale
#' @param risk_col character string naming the risk column to visualize
#' @param ll leaflet object to add layers to
#' @return leaflet map object
#' @export
#' @rdname plot_risk
#' @importFrom leaflet addPolygons
plot_risk_table_interactive <- function(dataset, scale, risk_col, ll = basemap()) {

  pal <- scorePalette(scale)
  if (!isTruthy(risk_col)) {
    fillColor <-  "lightgrey"
  } else {
    fillColor <- pal(dataset[[risk_col]])
  }

  label <- generate_leaflet_labels(
    dataset,
    title_field = "eu_name",
    exclude = c(
      "eu_id"
    )
  )

  # ll <- basemap()
  ll <- ll |> leaflet::addPolygons(
    data = dataset,
    fillColor = fillColor,
    fillOpacity = 0.75,
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    label = label,
    layerId = dataset$eu_id
  )

  ll <- ll |>
    addScoreLegend(scale, title = risk_col)
  ll
}
