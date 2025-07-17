#' @importFrom ggplot2
#'  ggplot geom_sf aes coord_sf theme labs .pt geom_sf_label
#'  element_blank geom_point geom_density element_rect geom_smooth
#'  scale_linetype_manual scale_x_continuous scale_y_continuous scale_fill_manual
#'  xlim ylim guide_legend guides margin
#' @importFrom dplyr
#'  mutate select group_by summarise across if_else ungroup row_number as_tibble
#'  c_across case_when desc pull rename rowwise coalesce starts_with cur_column
#'  arrange inner_join all_of any_of
#' @importFrom rlang
#'  call_args call_args_names call_name cnd_entrace
#' @importFrom purrr
#'  map imap pmap map_chr
#' @importFrom rlang
#'  check_dots_empty inherits_any %||%
#' @importFrom leaflet
#'  addLayersControl addMapPane addRasterImage addPolylines layersControlOptions
#'  pathOptions leafletOptions
#' @importFrom sf
#'  st_coordinates st_crs st_difference st_geometry st_geometry_type
#'  st_intersects st_length st_line_merge st_make_valid st_multipolygon
#'  st_nearest_feature st_point_on_surface st_polygon st_set_geometry
#'  st_sfc st_snap st_transform st_area st_as_text st_cast st_collection_extract
#'  sf_use_s2
NULL


#  addRiskLegend
# countries_sf_lowres country_ref gcIntermediate get_label
# get_risks_levels_scale isTruthy neighbours_table
#  rescale_risk riLabelOptions risk_palette risk_palette_12
