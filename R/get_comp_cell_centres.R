#' @title Extract computational cell cetres from 2d model domain
#' @description HEC-RAS exports computational cell centres to hdf, but also
#'   exports the perimeter cells, which are not appropriate to use for
#'   validation. This function identifies only the true cells fully within the
#'   model domain.
#' @param hc an hdf file read in with the hec_file() function
#' @param crs character string (e.g., "EPSG:3035)
#' @param verbose boolean
#' @details This function removes perimeter cells by using geos to spatially
#'   intersect cells with the model domain perimeter. This is potentially not
#'   the most efficient method, but it provides the required functionality.
#' @return A sfc POINT object containing all computational cell points to be
#'   joined to modelled parameters (e.g., WSE)
#' @examples
#' path = system.file("extdata\\chippewa.hdf",package = "hecr")
#' h <- hecr::hec_file(path)
#' 
#' crs_prj <- sf::read_sf(system.file("extdata\\profile_lines.shp",
#'                                    package = "hecr")) |>
#'   sf::st_crs()
#' 
#' comp_cell_centres <- get_comp_cell_centres(hc = h,
#'                       crs = crs_prj,
#'                       verbose = TRUE)
#' 
#' #comp_cell_centres |>
#' #  ggplot()+
#' #  geom_sf(aes(geometry = geom))
#' @export
get_comp_cell_centres <- function(hc,
                                  crs,
                                  verbose = FALSE){
  
  if (verbose == TRUE) {
    message("Extracting all cell centres...",
            paste0("[", Sys.time(), "]"))
  }
  
  flow_area_name <- hec_flow_area(hc)
  
  flow_area <- hec_center_coords(f = hc,
                                        area_name = flow_area_name)|>
    tibble::as_tibble(.name_repair = 'unique') |>
    suppressMessages() |>
    t() |>
    as.data.frame() |>
    tibble::as_tibble(.name_repair = 'unique') |>
    suppressMessages() |>
    dplyr::rename("x" = "V1", "y" = "V2") |>
    dplyr::mutate("hdf_cell_index" = dplyr::row_number()-1) |>
    sf::st_as_sf(coords = c("x", "y"), crs = crs) |>
    dplyr::rename("geom" = "geometry") |>
    dplyr::mutate(
      "x" = sf::st_coordinates(.data$geom)[, "X"],
      "y" = sf::st_coordinates(.data$geom)[, "Y"]
    ) 
  
  if (verbose == TRUE) {
    message("Extracting 2d model domain perimeter points...",
            paste0("[", Sys.time(), "]"))
  }
  
  flow_perimeter_points <- hec_perimeter_coords(f = hc,
                                                area_name = flow_area_name) |>
    tibble::as_tibble(.name_repair = 'unique') |>
    suppressMessages() |>
    t() |>
    as.data.frame() |>
    tibble::as_tibble(.name_repair = 'unique') |>
    suppressMessages() |>
    dplyr::rename("x" = "V1", "y" = "V2") |>
    dplyr::mutate("perim_index" = dplyr::row_number()) |>
    sf::st_as_sf(coords = c("x", "y"), crs = crs,remove= FALSE) |>
    dplyr::rename("geom" = "geometry")  |>
    dplyr::mutate("dummy" = "dummy")
  
  if (verbose == TRUE) {
    message("Converting 2d model domain perimeter points to linestring...",
            paste0("[", Sys.time(), "]"))
  }
  
  perimeter_lines = do.call(sf::st_sfc, 
                          lapply(split(flow_perimeter_points,
                                       flow_perimeter_points$dummy), function(d) {
    sf::st_cast(sf::st_linestring(cbind(d$x, d$y)), "POLYGON")
  })) |>
    sf::st_set_crs(crs) |>
    sf::st_cast("LINESTRING") |>
    sf::st_as_sf() |>
    dplyr::rename("geom" = "x")

  if (verbose == TRUE) {
    message("Identifying cells on perimeter to remove...",
            paste0("[", Sys.time(), "]"))
  }
  
  # get variable names for all cell coords and time stamps
  cells_to_remove = hecr::snap_points_to_lines_geos(
    sf_points = flow_area |>
      dplyr::group_by(.data$hdf_cell_index) |>
      dplyr::slice(1) |>
      dplyr::ungroup() |>
      sf::st_as_sf(coords = c("x","y"), crs = crs),
    sf_lin_net = perimeter_lines,
    snap_distance = 10,
    vertex_density = 1,
    verbose = verbose
  ) |>
    dplyr::pull("hdf_cell_index")
  
  flow_area_no_perim <- flow_area |>
    dplyr::filter(!c(.data$hdf_cell_index %in% cells_to_remove))
  

  return(flow_area_no_perim)
  
}