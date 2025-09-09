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
#' [leaflet::addScaleBar()], [leaflet::addLayersControl()]
#' @export
basemap <- function(...) {
  ll <- leaflet(
    ...,
    options = leafletOptions(
    doubleClickZoom = FALSE,
    scrollWheelZoom = TRUE
  ))
  ll <- ll |>
    addProviderTiles(leaflet::providers$OpenStreetMap, group = "Street map") |>
    addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Satelite") |>
    # addProviderTiles(leaflet::providers$OpenTopoMap, group = "Topo Map") |>
    addLayersControl(
      baseGroups = c("Street map", "Satelite"),
      options = layersControlOptions(collapsed = FALSE),
      position = "bottomright"
    ) |>
    addScaleBar(position = "bottomleft")
  ll
}

#' @importFrom leaflet labelOptions
riLabelOptions <- function(){

  labelOptions(
    textsize = "15px"
  )
}
#' Generate HTML Labels for Leaflet Maps
#'
#' Creates styled HTML table labels for Leaflet map popups or tooltips from spatial
#' or regular data frames. Automatically formats numeric values, truncates long strings,
#' and converts field names to human-readable labels.
#'
#' @param dat A data frame or sf object containing the data for labels
#' @param title_field Character. Name of the field to use as the label title/header.
#'   Set to NULL to omit title. Default is NULL (no title).
#' @param special_fields Named list. Fields that should have special suffixes added
#'   (e.g., list(score = "/10", rating = "/5")). Default is empty list.
#' @param source_field Character. Name of the field containing data source information
#'   to display in italics at bottom of table. Set to NULL to omit. Default is NULL (no source).
#' @param exclude_fields Character vector. Field names to exclude from the label table.
#'   The title_field and source_field are automatically excluded. Default is empty vector.
#' @param na_string Character. String to display for NA values. Default is " - ".
#'
#' @return A list of HTML objects suitable for use with Leaflet's label or popup parameters
#'
#' @details
#' The function creates a styled HTML table with the following formatting:
#' \itemize{
#'   \item Title field (if specified) appears as a bold header
#'   \item Data fields shown in a two-column table with labels and values
#'   \item Numeric values rounded to 2 decimal places
#'   \item Character values truncated to 47 characters
#'   \item Field names converted from snake_case to Sentence case
#'   \item Source field (if specified) appears in italics at bottom
#' }
#'
#' @examples
#' \dontrun{
#' # Simplest usage - generates labels for all columns
#' labels <- generate_leaflet_labels(my_sf_data)
#'
#' # Basic usage with sf data and custom title
#' library(sf)
#' library(leaflet)
#' library(dplyr)
#'
#' epi_profile <- get_wahis_erf("Anthrax", "Cattle", "Domestic")
#' emission_scores <- calc_emission_risk(epi_profile)
#' sf_data <- left_join(
#'   x = riskintrodata::world_sf,
#'   y = emission_scores,
#'   by = "iso3"
#' )
#'
#' # With special field formatting (scores out of maximum values)
#' labels <- generate_leaflet_labels(
#'   sf_data,
#'   title_field = "country_name",
#'   special_fields = list(
#'     sc_survmeasures = "/3",
#'     sc_control = "/2",
#'     sc_commerce = "/4",
#'     sc_epistatus = "/3",
#'     emission_risk = "/12"
#'   ),
#'   exclude_fields = c(
#'     "disease", "country", "iso3",
#'     "animal_category", "species",
#'     "country_name_fr"
#'   ),
#'   source_field = "data_source"
#' )
#'
#' # Use in leaflet map
#' leaflet(sf_data) %>%
#'   addTiles() %>%
#'   addPolygons(label = labels)
#'
#' }
#'
#' @importFrom sf st_drop_geometry
#' @importFrom purrr pmap map map_df
#' @importFrom stringr str_to_sentence
#' @export
generate_leaflet_labels <- function(dat,
                                    title_field = NULL,
                                    special_fields = list(),
                                    source_field = NULL,
                                    exclude_fields = c(),
                                    na_string = " - ") {

  # Drop geometry if it exists
  if ("sf" %in% class(dat)) {
    dat <- st_drop_geometry(dat)
  }

  # Format the data
  dat_formatted <- purrr::map_df(
    dat,
    function(x) {
      if (is.double(x)) {
        ifelse(is.na(x), na_string, sprintf("%.2f", x)) # handle NAs, round doubles
      } else if (is.character(x)) {
        ifelse(is.na(x), na_string, sprintf("%.47s", x)) # handle NAs, truncate long strings
      } else {
        ifelse(is.na(x), na_string, as.character(x)) # handle NAs for other types
      }
    }
  )

  # Create labels
  labels <- pmap(
    as.list(dat_formatted),
    function(...) {
      row_data <- list(...)

      # Start with title if specified
      html_parts <- c()
      if (!is.null(title_field) && title_field %in% names(row_data)) {
        html_parts <- c(html_parts,
                        paste0('<h4 style="margin: 0 0 8px 0; font-weight: bold; font-size: 14px;">',
                               row_data[[title_field]], '</h4>'))
      }

      # Start table
      html_parts <- c(html_parts,
                      '<table style="border-collapse: collapse; width: 100%; font-size: 12px;">')

      # Get fields to include (exclude title and source fields, plus any specified exclusions)
      fields_to_exclude <- c(title_field, source_field, exclude_fields)
      fields_to_include <- names(row_data)[!names(row_data) %in% fields_to_exclude]

      # Add data rows
      for (field in fields_to_include) {
        field_label <- stringr::str_to_sentence(gsub("_", " ", field))
        field_value <- row_data[[field]]

        # Check if this field has special formatting (like "/12" suffixes)
        if (field %in% names(special_fields)) {
          field_value <- paste0(field_value, special_fields[[field]])
        }

        html_parts <- c(html_parts,
                        paste0('<tr><td style="padding: 3px 8px; border-bottom: 1px solid #ddd;"><strong>',
                               field_label, '</strong></td><td style="padding: 3px 8px; border-bottom: 1px solid #ddd;">',
                               field_value, '</td></tr>'))
      }

      # Add source row if specified
      if (!is.null(source_field) && source_field %in% names(row_data)) {
        html_parts <- c(html_parts,
                        paste0('<tr><td style="padding: 3px 8px;"><em>Source</em></td><td style="padding: 3px 8px;"><em>',
                               row_data[[source_field]], '</em></td></tr>'))
      }

      # Close table
      html_parts <- c(html_parts, '</table>')

      paste0(html_parts, collapse = "")
    }
  ) |>
    map(HTML)

  return(labels)
}


#' Leaflet addLegend
#'
#' Same as [leaflet::addLegend()] but decreasing scale on legend.
#'
#' @param decreasing make scale decreasing, default = TRUE
#' @inheritParams leaflet::addLegend
#' @export
#' @importFrom leaflet
#'  evalFormula labelFormat invokeMethod getMapData
#' @importFrom grDevices col2rgb
#' @importFrom stats quantile na.omit
addLegend_decreasing <- function (map, position = c("topright", "bottomright", "bottomleft","topleft"),
                                  pal, values, na.label = "NA", bins = 7, colors,
                                  opacity = 0.5, labels = NULL, labFormat = labelFormat(),
                                  title = NULL, className = "info legend", layerId = NULL,
                                  group = NULL, data = getMapData(map), decreasing = TRUE) {

  position <- match.arg(position)
  type <- "unknown"
  na.color <- NULL
  extra <- NULL
  if (!missing(pal)) {
    if (!missing(colors))
      stop("You must provide either 'pal' or 'colors' (not both)")
    if (missing(title) && inherits(values, "formula"))
      title <- deparse(values[[2]])
    values <- evalFormula(values, data)
    type <- attr(pal, "colorType", exact = TRUE)
    args <- attr(pal, "colorArgs", exact = TRUE)
    na.color <- args$na.color
    if (!is.null(na.color) && col2rgb(na.color, alpha = TRUE)[[4]] ==
        0) {
      na.color <- NULL
    }
    if (type != "numeric" && !missing(bins))
      warning("'bins' is ignored because the palette type is not numeric")
    if (type == "numeric") {
      cuts <- if (length(bins) == 1)
        pretty(values, bins)
      else bins
      if (length(bins) > 2)
        if (!all(abs(diff(bins, differences = 2)) <=
                 sqrt(.Machine$double.eps)))
          stop("The vector of breaks 'bins' must be equally spaced")
      n <- length(cuts)
      r <- range(values, na.rm = TRUE)
      cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
      n <- length(cuts)
      p <- (cuts - r[1])/(r[2] - r[1])
      extra <- list(p_1 = p[1], p_n = p[n])
      p <- c("", paste0(100 * p, "%"), "")
      if (decreasing == TRUE){
        colors <- pal(rev(c(r[1], cuts, r[2])))
        labels <- rev(labFormat(type = "numeric", cuts))
      }else{
        colors <- pal(c(r[1], cuts, r[2]))
        labels <- rev(labFormat(type = "numeric", cuts))
      }
      colors <- paste(colors, p, sep = " ", collapse = ", ")
    }
    else if (type == "bin") {
      cuts <- args$bins
      n <- length(cuts)
      mids <- (cuts[-1] + cuts[-n])/2
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "bin", cuts))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "bin", cuts)
      }
    }
    else if (type == "quantile") {
      p <- args$probs
      n <- length(p)
      cuts <- quantile(values, probs = p, na.rm = TRUE)
      mids <- quantile(values, probs = (p[-1] + p[-n])/2, na.rm = TRUE)
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "quantile", cuts, p))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "quantile", cuts, p)
      }
    }
    else if (type == "factor") {
      v <- sort(unique(na.omit(values)))
      colors <- pal(v)
      labels <- labFormat(type = "factor", v)
      if (decreasing == TRUE){
        colors <- pal(rev(v))
        labels <- rev(labFormat(type = "factor", v))
      }else{
        colors <- pal(v)
        labels <- labFormat(type = "factor", v)
      }
    }
    else stop("Palette function not supported")
    if (!any(is.na(values)))
      na.color <- NULL
  }
  else {
    if (length(colors) != length(labels))
      stop("'colors' and 'labels' must be of the same length")
  }
  legend <- list(colors = I(unname(colors)), labels = I(unname(labels)),
                 na_color = na.color, na_label = na.label, opacity = opacity,
                 position = position, type = type, title = title, extra = extra,
                 layerId = layerId, className = className, group = group)
  invokeMethod(map, data, "addLegend", legend)
}

