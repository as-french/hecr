#' @title Extract boundary condition lines from HEC-RAS hdf file
#' @description This function receives a hc object and crs and returns at least
#'   two boundary condition LINESTRINGs.
#' @param hc an hdf file read in with the hec_file() function
#' @param crs character string (e.g., "EPSG:3035)
#' @param verbose boolean
#' @return A sfc LINESTRING object
#' @examples
#' path = system.file("extdata\\chippewa.hdf",package = "hecr")
#' h <- hecr::hec_file(path)
#' 
#' crs_prj <- sf::read_sf(system.file("extdata\\profile_lines.shp",
#'                                    package = "hecr")) |>
#'   sf::st_crs()
#' 
#' bc_lines <- get_boundary_condition_lines(hc = h,
#'                       crs = crs_prj,
#'                       verbose = TRUE)
#' 
#' #bc_lines |>
#' #  ggplot()+
#' #  geom_sf(aes(geometry = geom,
#' #  col = boundary_condition_pairs))
#' @export
get_boundary_condition_lines <- function(hc,
                                  crs,
                                  verbose = FALSE){
  
  if (verbose == TRUE) {
    message("Extracting 2d model domain boundary condition lines...",
            paste0("[", Sys.time(), "]"))
  }
  
  boundary_condition_lines_prelim <- hec_boundary_condition_lines(f = hc)|>
    tibble::as_tibble(.name_repair = 'unique') |>
    suppressMessages() |>
    t() |>
    as.data.frame() |>
    tibble::as_tibble(.name_repair = 'unique') |>
    suppressMessages() |>
    dplyr::rename("x" = "V1", "y" = "V2")
  
  boundary_condition_lines <- boundary_condition_lines_prelim |>
    dplyr::mutate(
      "boundary_condition_pairs" = 
       paste0("BC_",rep(seq(1:(nrow(boundary_condition_lines_prelim)/2)),2) |>
        sort())
      ) |>
    sf::st_as_sf(coords = c("x", "y"), crs = crs,remove= FALSE) |>
    dplyr::rename("geom" = "geometry") |>
    dplyr::group_by(.data$boundary_condition_pairs) |>
    dplyr::reframe("geom" = .data$geom |>
                     sf::st_union() |>
                     sf::st_cast("LINESTRING")) |>
    dplyr::ungroup() |>
    sf::st_as_sf()
  
  if (verbose == TRUE) {
    message("Done!",
            paste0("[", Sys.time(), "]"))
  }

  return(boundary_condition_lines)
  
}