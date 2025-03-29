#' @title A function to snap sf POINT data to LINESTRING
#'
#' @description The function is used to snap POINT data to the nearest
#'   LINESTRING.
#'
#' @param sf_lin_net An sf LINESTRING object.
#' @param sf_points An sf POINT object.
#' @param snap_distance numeric. Buffer distance around LINESTRING within which
#'   to search for intersecting points.
#' @param vertex_density Density of vertices assigned to LINESTRING. Smaller
#'   values lead to more precise snapping, which is increasingly useful with
#'   increased line longitudinal resolution.
#' @param verbose Boolean. If TRUE, returns messages during processing.
#'
#' @return An sf POINT object of snapped points with joined features from lines
#'   and points. Points outwith buffer distance from the nearest line are
#'   discarded.
#' @export
snap_points_to_lines_geos <- function(sf_points,
                                      sf_lin_net,
                                      snap_distance = 1000,
                                      vertex_density = 50,
                                      verbose = FALSE){
  
  if (verbose == TRUE) {
    message("Clipping lines within snap distance of all points...",
            paste0("[", Sys.time(), "]"))
  }
  
  # create buffers around all lines for extracting only lines that intersect buffers
  point_buffer = geos::geos_buffer(geos::as_geos_geometry(sf_points),
                                   distance = snap_distance)
  
  # clip lines
  sf_lin_net_clip_index = geos::geos_intersects_matrix(geos::as_geos_geometry(sf_lin_net),
                                                       geos::geos_strtree(point_buffer))
  sf_lin_net_clip = sf_lin_net[sapply(sf_lin_net_clip_index,
                                      function(xyz){length(xyz) > 0},
                                      simplify = TRUE),]
  
  points_unsnapped_geos = geos::as_geos_geometry(sf_points)
  
  if (verbose == TRUE) {
    message("Adding vertices to lines (densifying) and casting to POINT...",
            paste0("[", Sys.time(), "]"))
  }
  # "densify" lines with additional nodes and cast to points
  sf_lin_net_vertices = sf_lin_net_clip |>
    sf::st_sf(agr = "constant") |>
    sf::st_segmentize(dfMaxLength = vertex_density) |>
    sf::st_cast("POINT")
  
  if (verbose == TRUE) {
    message("Extracting geos geometries...",
            paste0("[", Sys.time(), "]"))
  }
  
  sf_lin_net_geos = geos::as_geos_geometry(sf_lin_net_vertices)
  sf_lin_net_geos_tree = geos::geos_strtree(sf_lin_net_geos)
  
  if (verbose == TRUE) {
    message("Identifying nearest line vertex to each point...",
            paste0("[", Sys.time(), "]"))
  }
  
  nearest_points_geos = geos::geos_nearest(geom = points_unsnapped_geos,
                                           tree = sf_lin_net_geos_tree)
  sf_lin_net_geos_nearest = sf_lin_net_geos[nearest_points_geos]
  
  if (verbose == TRUE) {
    message("Extracting distances to nearest vertex...",
            paste0("[", Sys.time(), "]"))
  }
  # get distances to nearest reaches
  nearest_points_geos_dist = geos::geos_distance_indexed(points_unsnapped_geos,
                                                         sf_lin_net_geos_nearest)
  if (verbose == TRUE) {
    message(paste0("Discarding points further than ",snap_distance,
                   "m from lines..."),
            paste0("[", Sys.time(), "]"))
  }
  # which are within 1000m
  barrier_indices = which(nearest_points_geos_dist <= snap_distance)
  
  # points within 1000
  points_unsnapped_within_1000_geos = geos::as_geos_geometry(sf_points[barrier_indices,])
  
  if (verbose == TRUE) {
    message(paste0("Snapping remaining points to nearest vertex..."),
            paste0("[", Sys.time(), "]"))
  }
  # snap these to
  nearest_points_geos_1000m = geos::geos_nearest(geom = points_unsnapped_within_1000_geos,
                                                 tree = sf_lin_net_geos_tree)
  sf_lin_net_geos_nearest_1000 = sf_lin_net_geos[nearest_points_geos_1000m]
  
  snapped_points <- geos::geos_snap(geom1 = points_unsnapped_within_1000_geos,
                                    sf_lin_net_geos_nearest_1000,tolerance = snap_distance) |>
    sf::st_as_sf() |>
    dplyr::rename("geom" = "geometry")
  
  sf_lin_net_nogeom = sf_lin_net_vertices |>
    sf::st_drop_geometry()
  points_unsnapped_nogeom = sf_points[barrier_indices,] |>
    sf::st_drop_geometry()
  
  result = cbind(snapped_points,
                 sf_lin_net_nogeom[nearest_points_geos_1000m,],
                 points_unsnapped_nogeom)
  
  if (verbose == TRUE) {
    message("Done!",
            paste0("[", Sys.time(), "]"))
  }
  
  return(result)
}
