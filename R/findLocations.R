#' findLocations
#'
#' Find locations inside a polygon, square, or circle drawn with leaflet.extras
#' drawing tools on a Shiny Leaflet map. Works with input locations as a
#' Spatial Points Dataframe or a Simple Features Dataframe
#'
#' @rdname findLocations
#' @param shape Shiny input (input$MAPID_draw_new_feature), representing shape
#'  drawn on the map by the user.
#' @param location_coordinates A SF point object or SpatialPointsDataframe
#' containing coordinates and ids for all map locations.
#' @param location_id_colname Column name from location_coordinates containing
#' desired names or ids for set of locations returned.
#' @return A vector of location ids.
#' @export
findLocations <- function(shape, location_coordinates, location_id_colname) {
  # Check that location_id_colname is present in data
  if (!(location_id_colname %in% colnames(location_coordinates))) {
    stop(sprintf("Column %s is not present in input data.", location_id_colname))
  }
  # Call function based on class of location coordinates
  if ("sf" %in% class(location_coordinates)) {
    return(findLocations_sf(shape, location_coordinates, location_id_colname))
  } else if ("sp" %in% class(location_coordinates)) {
    return(findLocations_sp(shape, location_coordinates, location_id_colname))
  } else {
    stop("Input data for argument location_coordinates must be SF or SP object.")
  }
}

#' findLocations_sf
#'
#' Find locations inside a polygon, square, or circle drawn with leaflet.extras
#' drawing tools on a Shiny Leaflet map, when locations are a Simple Features
#' Point Dataframe
#'
#' @rdname findLocations_sf
#' @param shape Shiny input (input$MAPID_draw_new_feature), representing shape
#'  drawn on the map by the user.
#' @param location_coordinates A SpatialPointsDataFrame containing coordinates
#' and ids for all map locations.
#' @param location_id_colname Column name from location_coordinates containing
#' desired names or ids for set of locations returned.
#' @return A vector of location ids.
#' @export
#' @import sf
#' @examples
#' mock_input.map_feature <- list(
#'   type = "Feature",
#'   properties = list(`_leaflet_id` = 13477, feature_type = "rectangle"),
#'   geometry = list(
#'     type = "Polygon",
#'     coordinates = list(list(
#'       list(-76.15723, 39.51252),
#'       list(-76.15723, 40.30467), list(-74.73999, 40.30467),
#'       list(-74.73999, 39.51252), list(-76.15723, 39.51252)
#'     ))
#'   )
#' )
#' airports <- data.frame(
#'   "locationID" = c("PHL", "DTW"),
#'   "Longitude" = c(-75.2408, -83.3533),
#'   "Latitude" = c(39.8722, 42.2125)
#' )
#' coords = sf::st_as_sf(airports, coords = c("Longitude", "Latitude"))
#' findLocations_sf(
#'  shape = mock_input.map_feature,
#'  location_coordinates = coords,
#'  location_id_colname = "locationID"
#' )
findLocations_sf <- function(shape, location_coordinates, location_id_colname) {
  # If the CRS is missing, assume it is lat lon
  if (sf::st_crs(location_coordinates) == sf::NA_crs_) {
    sf::st_crs(location_coordinates) <- 4326
  }
  # Derive polygon coordinates and feature_type from shape input
  polygon_coordinates <- matrix(unlist(shape$geometry$coordinates),
                                ncol = 2,
                                byrow = T)
  feature_type <- shape$properties$feature_type
  # For rectangles and polygons
  if (feature_type %in% c('rectangle', 'polygon')) {
    # Transform into a SF polygon
    drawn_polygon <- sf::st_polygon(list(polygon_coordinates))
    # Remove CRS from location coordinates
    sf::st_crs(location_coordinates) <- NA
    # Set relation to geometry to surpress warnings
    sf::st_agr(location_coordinates) <- 'constant'
    # Identify selected locations
    selected_locs <- sf::st_intersection(location_coordinates, drawn_polygon)
    # Get location ids
    selected_loc_id <- selected_locs[[location_id_colname]]
    return(selected_loc_id)
  }
  if (feature_type == 'circle') {
    # Radius in meters
    r <- units::set_units(shape$properties$radius, m)
    # Center to SF
    center <- sf::st_as_sf(as.data.frame(polygon_coordinates),
                           coords = c(1,2),
                           crs = 4326)
    # Find UTM zone of center point as EPSG code
    utm <- (floor((as.data.frame(polygon_coordinates)[1,1] + 180) / 6) %% 60) + 1
    if (as.data.frame(polygon_coordinates)[1,2] > 0) {
      utm <- utm + 32600
    } else {
      utm <- utm + 32700
    }
    # Project to UTM for buffering, using EPSG code of UTM zone
    center <- sf::st_transform(center, crs = utm)
    drawn_circle <- sf::st_buffer(center, dist = r)
    # Project location coordinates to UTM zone
    location_coordinates <- sf::st_transform(location_coordinates, crs = utm)
    # Set relation to geometry to surpress warnings
    sf::st_agr(location_coordinates) <- 'constant'
    sf::st_agr(drawn_circle) <- 'constant'
    # Identify selected locations
    selected_locs <- sf::st_intersection(location_coordinates, drawn_circle)
    # Get location ids
    selected_loc_id <- selected_locs[[location_id_colname]]
    return(selected_loc_id)
  }
}

#' findLocations_sp
#'
#' Find locations inside a polygon, square, or circle drawn with leaflet.extras
#' drawing tools on a Shiny Leaflet map, when locations are a Spatial Points
#' Dataframe
#'
#' @rdname findLocations_sp
#' @param shape Shiny input (input$MAPID_draw_new_feature), representing shape
#'  drawn on the map by the user.
#' @param location_coordinates A SpatialPointsDataFrame containing coordinates
#' and ids for all map locations.
#' @param location_id_colname Column name from location_coordinates containing
#' desired names or ids for set of locations returned.
#' @return A vector of location ids.
#' @export
#' @import sp
#' @examples
#' mock_input.map_feature <- list(
#'   type = "Feature",
#'   properties = list(`_leaflet_id` = 13477, feature_type = "rectangle"),
#'   geometry = list(
#'     type = "Polygon",
#'     coordinates = list(list(
#'       list(-76.15723, 39.51252),
#'       list(-76.15723, 40.30467), list(-74.73999, 40.30467),
#'       list(-74.73999, 39.51252), list(-76.15723, 39.51252)
#'     ))
#'   )
#' )
#' airports <- data.frame(
#'   "locationID" = c("PHL", "DTW"),
#'   "Longitude" = c(-75.2408, -83.3533),
#'   "Latitude" = c(39.8722, 42.2125)
#' )
#' coords <- sp::SpatialPointsDataFrame(airports[, c("Longitude", "Latitude")], airports)
#' findLocations_sp(
#'   shape = mock_input.map_feature,
#'   location_coordinates = coords,
#'   location_id_colname = "locationID"
#' )
findLocations_sp <- function(shape, location_coordinates, location_id_colname) {
  # Derive polygon coordinates and feature_type from shape input
  polygon_coordinates <- shape$geometry$coordinates
  feature_type <- shape$properties$feature_type
  # For rectangles and polygons
  if (feature_type %in% c("rectangle", "polygon")) {
    # Transform into a spatial polygon
    drawn_polygon <- sp::Polygon(do.call(rbind, lapply(polygon_coordinates[[1]], function(x) {
      c(x[[1]][1], x[[2]][1])
    })))
    # Identify selected locations
    selected_locs <- sp::over(location_coordinates, sp::SpatialPolygons(list(sp::Polygons(list(drawn_polygon), "drawn_polygon"))))
    # Get location ids
    x <- (location_coordinates[which(!is.na(selected_locs)), location_id_colname])

    selected_loc_id <- as.character(x[[location_id_colname]])

    return(selected_loc_id)
  } else if (feature_type == "circle") {
    center_coords <- matrix(c(
      polygon_coordinates[[1]],
      polygon_coordinates[[2]]
    ),
    ncol = 2
    )

    # Get distances to center of drawn circle for all locations in
    # location_coordinates distance is in kilometers
    dist_to_center <- sp::spDistsN1(location_coordinates,
      center_coords,
      longlat = T
    )

    # Get location ids - radius is in meters
    x <- location_coordinates[
      dist_to_center < shape$properties$radius / 1000,
      location_id_colname
    ]

    selected_loc_id <- as.character(x[[location_id_colname]])
    return(selected_loc_id)
  }
}
