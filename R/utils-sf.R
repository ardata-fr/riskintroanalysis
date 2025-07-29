
#' @importFrom sf st_union
st_union_quiet <- function(...) {
  suppressWarnings(suppressMessages(st_union(...)))
}

#' @importFrom dplyr summarise
summarise_quiet <- function(...) {
  suppressWarnings(suppressMessages(summarise(...)))
}

#' @importFrom sf st_join
st_join_quiet <- function(...) {
  suppressWarnings(suppressMessages(st_join(...)))
}

#' @importFrom sf st_intersection
st_intersection_quiet <- function(...){
  suppressWarnings(suppressMessages(st_intersection(...)))
}

#' @importFrom sf st_difference
st_difference_quiet <- function(...){
  suppressWarnings(suppressMessages(st_difference(...)))
}


safe_stat <- function(..., FUN = max, NA_value = NA_real_) {
  x <- na.omit(unlist(list(...)))
  if (length(x) > 0) {
    FUN(x)
  } else {
    NA_value
  }
}

#' @importFrom sf st_union
is_valid_bounding_box <- function(x) {
  if (!isTruthy(x)) return(FALSE)
  if (!all(sapply(x, is.finite))) return(FALSE)
  if (length(x) == 0) return(FALSE)

  round(x$north - x$south, 1) > 0 && round(x$east - x$west, 1) > 0
}

format_bb <- function(x) {
  sprintf("north: %.02f, south: %.02f, east: %.02f, west: %.02f", x$north, x$south, x$east, x$west)
}

prepare_sf_for_duckdb <- function(x) {
  stopifnot(is.data.frame(x))
  if (inherits(x, "sf")) {
    x <- mutate(.data = x, geometry = st_as_text(.data[["geometry"]]))
    x <- as_tibble(x)
  }
  x
}

latlng_to_sf <- function(dat) {
  if (all(c("lat", "lng") %in% colnames(dat))) {
    dat <- st_as_sf(dat, coords = c("lng", "lat"), crs = 4326)
  }
  dat
}

#' Fill in holes in a polygon
#'
#' @param x polgon
#'
#' @returns polygon with no holes in it
#' @noRd
#'
#' @examples
#' library(sf)
#' outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
#' hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
#' hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
#' pts = list(outer, hole1, hole2)
#' pl1 = st_geometry(st_polygon(pts))
#' plot(pl1, title= 'swiss cheese polygon')
#'
#' pl2 <- st_fill_holes(pl1)
#' plot(pl2, title = 'No holes!')
st_fill_holes <- function(x) {

  GEOM <- st_geometry(x)

  TYPES <- st_geometry_type(GEOM, by_geometry = TRUE)

  NEW_GEOMS <- vector("list", length(GEOM))

  for (i in seq_along(GEOM)) {
    if (TYPES[i] == "POLYGON") {
      NEW_GEOMS[[i]] <- fill_polygon(GEOM[[i]])
    } else if (TYPES[i] == "MULTIPOLYGON") {
      NEW_GEOMS[[i]] <- fill_multipolygon(GEOM[[i]])
    } else {
      stop("not supported type of geometry: ", TYPES[i])
    }
  }

  NEW_SFC <- st_sfc(NEW_GEOMS, crs = st_crs(GEOM))

  if (inherits(x, "sf")) {
    st_set_geometry(x, NEW_SFC)
  } else {
    NEW_SFC
  }
}

fill_polygon <- function(p) {
  st_polygon(list(p[[1]]))
}

fill_multipolygon <- function(mp) {
  st_multipolygon(lapply(mp, function(p) list(p[[1]])))
}


get_eu_country <- function(eu) {

  eu_country <- suppressMessages(
    suppressWarnings({
      sf::sf_use_s2(FALSE)
      x <- riskintrodata::world_sf[
        st_nearest_feature(get_sf_centerpoint(eu, sf = TRUE), riskintrodata::world_sf),
        ]
      sf::sf_use_s2(TRUE)
      x
    }))

  list(
    country_iso3 = eu_country$iso3,
    country_name = eu_country$country_name
  )
}

#' @importFrom sf st_make_valid st_point_on_surface st_coordinates
get_sf_centerpoint <- function(eu, union = TRUE,  sf = FALSE) {
  eu <- st_make_valid(eu)
  if(union){
    eu <- st_union_quiet(eu, by_feature = FALSE)
  }
  eu_center_point <- st_point_on_surface(eu)

  if (sf == TRUE) {
    return(eu_center_point)
  }

  list(
    x = st_coordinates(eu_center_point)[[1]],
    y = st_coordinates(eu_center_point)[[2]]
  )
}

iso3_to_name <- function(x, lang = "en") {
  x <- as.character(x)
  riskintrodata::country_ref[[paste0("name_", lang)]][match(x, riskintrodata::country_ref$iso3)] %||% NA
}
