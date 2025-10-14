
#' Plot risk
#'
#' Helper function to plot riskintro's default risks using ggplot2.
#'
#' @param dataset risk table, generally the output of a
#' @param risk_name risk_name used to determine which plotting function to use,
#' this is NULL by default and is inferred from `attr(x, "table_name")`.
#' @param risk_col the risk column to represent in the visualisation,
#' this is NULL by default and is inferred from `attr(x, "risk_col")`.
#' @param scale the scale of `risk_col`, numeric vector of length 2,
#' this is NULL by default and is inferred from `attr(x, "scale")`.
#' @param interactive default to FALSE for ggplot2 visuals, TRUE returns
#' interactive leaflet.
#' @return ggplot2 object with the appropriate visualisation if interactive is FALSE,
#' otherwise, leaflet object
#' @export
#' @example examples/plot_risk.R
#' @name plot_risk
plot_risk <- function(
    dataset,
    risk_name = NULL,
    risk_col = NULL,
    scale = NULL,
    interactive = FALSE
    ){

  table_name <- risk_name %||% attr(dataset, "table_name")
  risk_col <- risk_col %||% attr(dataset, "risk_col")
  scale <- scale %||% attr(dataset, "scale")

  cli_abort_if_not(
    "{.arg table_name} is NULL and not in attributes of {.arg dataset}" = !is.null(table_name),
    "{.arg risk_col} is NULL and not in attributes of {.arg dataset}" = !is.null(risk_col),
    "{.arg scale} is NULL and not in attributes of {.arg dataset}" = !is.null(scale)
  )

  if (interactive) {
    plot_risk_interactive(
      dataset = dataset,
      risk_name = table_name,
      risk_col = risk_col,
      scale = scale,
      ll = basemap()
    )
  } else {
    plot_fun <- switch(
      table_name,
      "entry_points" = plot_entry_points,
      "animal_mobility" = plot_animal_mobility,
      "road_access" = plot_road_access,
      "border_risk" = plot_border_risk,
      "epi_units" = plot_epi_units,
      "risk_table" = plot_risk_table,
      "additional_risk" = plot_additional_risk,
      cli_abort("This risk is not supported.")
    )
    plot_fun(
      dataset = dataset,
      scale = scale,
      risk_col = risk_col
    )
  }
}



#' @importFrom ggplot2 scale_color_viridis_c scale_fill_viridis_c scale_color_gradientn scale_fill_gradientn
#' @importFrom grDevices hcl.colors
ggplot_risk_scale <- function(limits = c(0, 100)){
  list(
    ggplot2::scale_color_viridis_c(limits = limits, direction = -1),
    ggplot2::scale_fill_viridis_c(limits = limits, direction = -1)
  )
}

ggplot_score_scale <- function(limits = c(0, 100)){
  list(
    ggplot2::scale_color_gradientn(
      colors = grDevices::hcl.colors(256, "inferno"),
      limits = limits
    ),
    ggplot2::scale_fill_gradientn(
      colors = grDevices::hcl.colors(256, "inferno"),
      limits = limits
    )
  )
}

#' @export
#' @rdname plot_risk
plot_entry_points <- function(dataset, scale, risk_col) {
  gg <- ggplot()
  gg <- gg +
    geom_sf(
      data = dataset,
      aes(fill = .data[[risk_col]]),
      alpha = 0.6,
      color = "white",
      linewidth = 0.3
    )

  if (!is.null(extract_point_risk(dataset))) {
    points_data <- extract_point_risk(dataset)
    gg <- gg +
      geom_sf(
        data = extract_point_risk(dataset),
        size = 1.5, shape = 21,
        color = "black",
        aes(fill = .data[[attr(points_data, "risk_col")]])
      )
  } else {
    cli_warn("No point data provided, it will not be plotted.")
  }
  gg <- gg + ggplot_score_scale(limits = scale)
  gg <- gg + theme_void()
  gg
}

#' @export
#' @rdname plot_risk
plot_animal_mobility <- function(dataset, scale, risk_col) {
  gg <- ggplot()
  gg <- gg +
    geom_sf(
      data = dataset,
      aes(
        geometry = .data$geometry,
        fill = .data[[risk_col]]
      ),
      color = "white",
      linewidth = 0.3
    )

  if (!is.null(extract_flow_risk(dataset))) {
    gg <- gg +
      geom_sf(
        data = extract_flow_risk(dataset),
        color = "black", size = 1.5, shape = 21,
        aes(fill = .data$emission_risk_weighted )
      )
  } else {
    cli_warn("No flows data provided, it will not be plotted.")
  }

  gg <- gg + ggplot_score_scale(limits = scale)
  gg <- gg + theme_void()
  gg
}

#' @export
#' @rdname plot_risk
plot_road_access <- function(dataset, scale, risk_col) {
  gg <- ggplot()
  gg <- gg +
    geom_sf(
      data = dataset,
      aes(
        geometry = .data$geometry,
        fill = .data[[risk_col]]
      ),
      color = "white",
      linewidth = 0.3
    )

  gg <- gg + ggplot_score_scale(limits = scale)
  gg <- gg + theme_void()
  gg
}

#' @export
#' @rdname plot_risk
plot_border_risk <- function(dataset, scale, risk_col) {
  gg <- ggplot()
  gg <- gg +
    geom_sf(
      data = dataset,
      aes(fill = .data[[risk_col]]),
      alpha = 0.5,
      color = "white",
      linewidth = 0.3
    )

  borders_data <- extract_border(dataset)
  borders_scale <- attr(borders_data, "scale")

  gg <- gg +
    geom_sf(
      data = borders_data,
      aes(color = .data$border_risk),
      linewidth =  2
    )

  gg <- gg + ggplot_score_scale(limits = scale)
  gg <- gg + ggplot_risk_scale(limits = borders_scale)
  gg <- gg + theme_void()
  gg
}

#' @export
#' @rdname plot_risk
plot_epi_units <- function(dataset, scale, risk_col) {
  gg <- ggplot()
  gg <- gg +
    geom_sf(
      data = dataset,
      color = "black",
      fill = NA,
      linewidth = 0.5
    )
  gg <- gg + theme_void()
  gg
}

#' @export
#' @rdname plot_risk
plot_risk_table <- function(dataset, scale, risk_col){
  gg <- ggplot()
  gg <- gg +
    geom_sf(
      data = dataset,
      aes(fill = .data[[risk_col]]),
      color = "white",
      linewidth = 0.3
    )
  gg <- gg + ggplot_score_scale(limits = scale)
  gg <- gg + theme_void()
  gg

}

#' @export
#' @rdname plot_risk
plot_additional_risk <- function(dataset, scale, risk_col) {
  gg <- ggplot()
  gg <- gg +
    geom_sf(
      data = dataset,
      aes(
        geometry = .data$geometry,
        fill = .data[[risk_col]]
      ),
      color = "white",
      linewidth = 0.3
    )

  gg <- gg + ggplot_score_scale(limits = scale)
  gg <- gg + theme_void()
  gg
}
