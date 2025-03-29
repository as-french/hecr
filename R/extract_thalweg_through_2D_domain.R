#' @title Extract thalweg line or 
#' @description HEC-RAS exports computational cell centres and cell elevations
#'   to hdf. We can use these to identify a thalweg through the 2D model domain.
#'   This function can return either the cell points as sfc POINTs, or sfc
#'   LINESTRING built from these points. The motivation for this function is to
#'   facilitate testing of HEC-RAS model outputs along the deepest stretch of
#'   the river. For example, we can check the attenuation of a flood wave from
#'   upstream to downstream through a 2D domain. This function uses Barry
#'   Rowlingson's code provied on the r-sig-geo mailing list on 5 June 2013 (see
#'   reference).
#' @param hc an hdf file read in with the hec_file() function
#' @param crs character string (e.g., "EPSG:3035)
#' @param number_of_sections number of cross sections to use to derive thalweg
#' @param time_stamp a timestamp length = 1 POSIXct object
#' @param verbose boolean
#' @details Note, only one thalweg will be generated, which can lead to
#'   unexpected results in cases of multi-channel (e.g.., braided) 2D model
#'   domains.
#' @return A list of sfc POINT and LINESTRING objects. "thalweg_points",
#'   "thalweg_line", and "transect_lines"
#' @examples
#' path = system.file("extdata\\Chippewa_2D.p05.hdf",package = "hecr")
#' h <- hecr::hec_file(path)
#' 
#' crs_prj <- sf::read_sf(system.file("extdata\\profile_lines.shp",
#'                                    package = "hecr")) |>
#'   sf::st_crs()
#' 
#' thalweg_points <- extract_thalweg_through_2D_domain(hc = h,
#'                       crs = crs_prj,
#'                       number_of_sections = 10,
#'                       verbose = TRUE)
#' 
#' #   do.call(dplyr::bind_rows,thalweg_points)|>
#' #  ggplot()+
#' #  geom_sf(aes(geometry = geom))
#' @source \url{https://stat.ethz.ch/pipermail/r-sig-geo/2013-June/018449.html}
#' @export
extract_thalweg_through_2D_domain <- function(hc,
                                              crs,
                                              number_of_sections,
                                              time_stamp = NULL,
                                              verbose = FALSE) {
  
  if (verbose == TRUE) {
    message("Extracting all timestamps...",
            paste0("[", Sys.time(), "]"))
  }
  
  all_time_stamps <- hecr::hec_timestamps(
    hc = hc,time_zone = "America/Chicago")
  
  if(is.null(time_stamp)){
    time_stamp_to_extact = 1
  } else if(!is.null(time_stamp)){
    time_stamp_to_extact = which(all_time_stamps == time_stamp)
  }
  
  if (verbose == TRUE) {
    message("Extracting all cell centres...",
            paste0("[", Sys.time(), "]"))
  }
  
  comp_cell_centres <- hecr::get_comp_cell_centres(hc = hc,
                                                   crs = crs,
                                                   verbose = verbose)
  
  cells_to_keep <- comp_cell_centres |>
    dplyr::pull("hdf_cell_index")
  
  mobile_bed_elev <- hecr::hec_two(hc = hc,
                                   xy = comp_cell_centres |>
                                     sf::st_drop_geometry() |>
                                     dplyr::select("x","y"),
                                   ts_type = "Cell Bed Elevation",
                                   time_zone = "America/Chicago") |>
    dplyr::filter(.data$hdf_cell_index %in% cells_to_keep) |>
    dplyr::rename("bed_elev" = "value") |>
    dplyr::filter(.data$datetime == all_time_stamps[[time_stamp_to_extact]]) |>
    sf::st_as_sf(coords = c("xin","yin"),crs = crs)

  # mobile_bed_elev |>
  #   ggplot()+
  #   labs(title = "Cell bed elevation")+
  #   geom_sf(aes(geometry = geometry, col = bed_elev))+
  #   theme(axis.text.x = element_text(angle = 45, vjust = 1,hjust = 1)) +
  #   facet_wrap(vars(datetime))
  
  if (verbose == TRUE) {
    message("Extracting boundary condition lines...",
            paste0("[", Sys.time(), "]"))
  }
  
  
  bc_lines <- get_boundary_condition_lines(hc = hc,
                                           crs = crs,
                                           verbose = verbose) 
  
  # bc_lines |>
  #   ggplot()+
  #   geom_sf(aes(geometry = geom,
  #               col = boundary_condition_pairs))
  
  min_bc_line_length = bc_lines |>
    dplyr::mutate("line_length" = sf::st_length(.data$geom) |> units::drop_units()) |>
    dplyr::pull("line_length") |>
    min()
  
  bc_lines_upstream_point <- bc_lines |>
    dplyr::filter(.data$boundary_condition_pairs == "BC_1") |>
    dplyr::mutate("geom" = sf::st_segmentize(
      x = .data$geom, dfMaxLength = min_bc_line_length /
                                               10)) |>
    dplyr::mutate("geom" = sf::st_line_sample(.data$geom,
                                              n = 1, sample = 0.5))
  
  bc_lines_downstream_point <- bc_lines |>
    dplyr::filter(.data$boundary_condition_pairs == "BC_2") |>
    dplyr::mutate("geom" = sf::st_segmentize(
      x = .data$geom, dfMaxLength = min_bc_line_length /
                                               10)) |>
    dplyr::mutate("geom" = sf::st_line_sample(.data$geom,
                                              n = 1, sample = 0.5))
  
  # mobile_bed_elev |>
  #   ggplot() +
  #   labs(title = "Cell bed elevation") +
  #   geom_sf(aes(geometry = geometry, col = bed_elev)) +
  #   ggnewscale::new_scale_colour() +
  #   geom_sf(data = bc_lines, aes(
  #     geometry = geom,
  #     col = factor(boundary_condition_pairs)
  #   )) +
  #   theme(axis.text.x = element_text(
  #     angle = 45,
  #     vjust = 1,
  #     hjust = 1
  #   )) +
  #   geom_sf(data = bc_lines_downstream_point, aes(geometry = geom)) +
  #   geom_sf(data = bc_lines_upstream_point, aes(geometry = geom)) +
  #   facet_wrap(vars(datetime))
  
  if (verbose == TRUE) {
    message("Identifying upstream and downstream starting cells...",
            paste0("[", Sys.time(), "]"))
  }
  
  starting_point <- sf::st_nearest_feature(
    x = bc_lines_upstream_point,
    y = mobile_bed_elev
  )
  ending_point <- sf::st_nearest_feature(
    x = bc_lines_downstream_point,
    y = mobile_bed_elev
  )
  
  bed_point_start = mobile_bed_elev[starting_point,]
  bed_point_end = mobile_bed_elev[ending_point,]

  if (verbose == TRUE) {
    message("Connecting start and end points with LINESTRING...",
            paste0("[", Sys.time(), "]"))
  }
  
  basic_centreline <- bed_point_start |>
    dplyr::bind_rows(bed_point_end) |>
    sf::st_union() |>
    sf::st_cast("LINESTRING")
  
  basic_centreline_df <- basic_centreline |>
    sf::st_as_sf() |>
    sf::st_cast("POINT") |>
    sf::st_as_sf() |>
    dplyr::rename("geom" = "x") |>
    dplyr::mutate("x" = sf::st_coordinates(.data$geom)[,"X"],
                  "y" = sf::st_coordinates(.data$geom)[,"Y"]) |>
    sf::st_drop_geometry()
  
  if (verbose == TRUE) {
    message("Generating points along LINESTRING at which to generate cross-sections...",
            paste0("[", Sys.time(), "]"))
  }
  
  seg_length <- (sf::st_length(x = basic_centreline) |> units::drop_units())/number_of_sections
  
  tspts <- evenspace(basic_centreline_df,
                     seg_length)
  
  if (verbose == TRUE) {
    message("Generating cross-section start and end points...",
            paste0("[", Sys.time(), "]"))
  }
  
  tslines <- transect(tspts, 1500)
  
  tslines_df_start <- tslines |>
    dplyr::filter(complete.cases(.data$x0)) |>
    dplyr::rowwise() |>
    sf::st_as_sf(coords = c("x0","y0"), crs = crs) |>
    dplyr::rename("start_point_geom" = "geometry") |>
    dplyr::select("start_point_geom")
  
  tslines_df_end <- tslines |>
    dplyr::filter(complete.cases(.data$x0)) |>
    dplyr::rowwise() |>
    sf::st_as_sf(coords = c("x1","y1"), crs = crs) |>
    dplyr::rename("end_point_geom" = "geometry") |>
    dplyr::select("end_point_geom")
  
  if (verbose == TRUE) {
    message("Estimating mean HEC-RAS unstructured grid resoltution...",
            paste0("[", Sys.time(), "]"))
  }
  
  mobile_bed_elev_sf <- mobile_bed_elev |>
    dplyr::filter(.data$datetime == all_time_stamps[[time_stamp_to_extact]]) |>
    sf::st_as_sf(coords = c("xin", "yin"), crs = crs)
  
  resolution <- vapply(1:nrow(mobile_bed_elev_sf), function(i){
    
    res <-  nngeo::st_nn(x = mobile_bed_elev_sf[i,],
                         y = mobile_bed_elev_sf[-i,],
                         returnDist = TRUE,
                         progress = FALSE) |>
      suppressMessages() |>
      unlist(recursive = TRUE)
    
    result <- res[[2]]
    
  },
  FUN.VALUE = numeric(1)) |>
    mean() |>
    round(1)
  
  if (verbose == TRUE) {
    message("Converting cross-section to segmentised linestrings...",
            paste0("[", Sys.time(), "]"))
  }
  
  tslines_df <- dplyr::bind_cols(tslines_df_start,
                                 tslines_df_end) |>
    dplyr::rowwise() |>
    dplyr::mutate("geom" = sf::st_union(.data$start_point_geom,.data$end_point_geom) |>
                    sf::st_cast("LINESTRING")) |>
    dplyr::ungroup() |>
    sf::st_set_geometry("geom")|>
    dplyr::select("geom") |>
    dplyr::mutate("geom" = sf::st_segmentize(.data$geom,resolution)) |>
    dplyr::arrange(-dplyr::row_number()) |>
    dplyr::mutate("transect_id" = dplyr::row_number())
  
  if (verbose == TRUE) {
    message("Snapping nearest computational cells to transect...",
            paste0("[", Sys.time(), "]"))
  }
  
  elev_values_snapped_to_transects <-
    snap_points_to_lines_geos(
      sf_points = mobile_bed_elev_sf,
      sf_lin_net = tslines_df,
      snap_distance = resolution,
      vertex_density = resolution/2,
      verbose = verbose
    )
  
  if (verbose == TRUE) {
    message("Extracting thalweg POINTs...",
            paste0("[", Sys.time(), "]"))
  }
  
  hdf_index_cell_at_thalweg <- 
    elev_values_snapped_to_transects |>
    dplyr::slice_min(.data$bed_elev, by = "transect_id") |>
    sf::st_drop_geometry() |>
    dplyr::select("hdf_cell_index","transect_id")
  
  thalweg_points <- mobile_bed_elev_sf |>
    dplyr::right_join(hdf_index_cell_at_thalweg, by = "hdf_cell_index") |>
    dplyr::rename("geom" = "geometry")|>
    dplyr::arrange(.data$transect_id)
  
  #  mobile_bed_elev |>
  #    ggplot() +
  #    labs(title = "Cell bed elevation") +
  #    geom_sf(aes(geometry = geometry, col = bed_elev), alpha = .3) +
  #    ggnewscale::new_scale_colour() +
  # #   geom_sf(data = bc_lines, aes(
  # #     geometry = geom,
  # #     col = factor(boundary_condition_pairs)
  # #   ), alpha = .5) +
  # #   theme(axis.text.x = element_text(
  # #     angle = 45,
  # #     vjust = 1,
  # #     hjust = 1
  # #   )) +
  # #   geom_sf(data = bed_point_start, aes(geometry = geometry), col = "blue") +
  # #   geom_sf(data = bed_point_end, aes(geometry = geometry), col = "red") +
  # #   geom_sf(data =basic_centreline,aes(geometry = geometry))+
  # #   geom_sf(data = tslines_df,aes(geometry = geom))+
  # #   geom_sf(data = elev_values_snapped_to_transects,aes(geometry = geom), col = "red")+
  #    geom_sf(data = thalweg_points, aes(geometry = geom, col = transect_id))+
  #    facet_wrap(vars(datetime))
  
  if (verbose == TRUE) {
    message("Creating LINESTRING from thalweg points...",
            paste0("[", Sys.time(), "]"))
  }
  
   # ------------------------------------------------------------------- #
   # Thomas Moore's solution https://stackoverflow.com/a/76197010
  thalweg_linestring <- thalweg_points |>
    dplyr::mutate("dummy" = "dummy") |>
    dplyr::group_by(.data$dummy) |>
    dplyr::summarize(do_union = FALSE) |>
    sf::st_cast("LINESTRING") |>
    sf::st_as_sf()
  # -------------------------------------------------------------------- #
 
  #  mobile_bed_elev |>
  #   ggplot() +
  #   labs(title = "Cell bed elevation") +
  #   geom_sf(aes(geometry = geometry, col = bed_elev), alpha = .3) +
  #   ggnewscale::new_scale_colour() +
  #   geom_sf(data = bc_lines, aes(
  #     geometry = geom,
  #     col = factor(boundary_condition_pairs)
  #   ), alpha = .5) +
  #   theme(axis.text.x = element_text(
  #     angle = 45,
  #     vjust = 1,
  #     hjust = 1
  #   )) +
  # #  geom_sf(data = bed_point_start, aes(geometry = geometry), col = "blue") +
  #   #geom_sf(data = bed_point_end, aes(geometry = geometry), col = "red") +
  #   #geom_sf(data =basic_centreline,aes(geometry = geometry))+
  #   geom_sf(data = tslines_df,aes(geometry = geom))+
  #   #geom_sf(data = elev_values_snapped_to_transects,aes(geometry = geom), col = "red")+
  #   #geom_sf(data = thalweg_linestring, aes(geometry = geom),col = "green3")+
  #   facet_wrap(vars(datetime))
  
  if (verbose == TRUE) {
    message("Done!",
            paste0("[", Sys.time(), "]"))
  }
  
  return(list("thalweg_points" = thalweg_points,
              "thalweg_line" = thalweg_linestring,
              "transect_lines" = tslines_df))
  
}

# --------------------------------------------------------------------------- #
# INTERNALS (Barry Rowlingson's CODE)

evenspace <- function(xy, sep, start = 0, size) {
  dx <- c(0, diff(xy[, 1]))
  dy <- c(0, diff(xy[, 2]))
  dseg <- sqrt(dx^2 + dy^2)
  dtotal <- cumsum(dseg)
  
  linelength <- sum(dseg)
  
  pos <- seq(start, linelength, by = sep)
  
  whichseg <- unlist(lapply(pos, function(x) {
    sum(dtotal <= x)
  }))
  
  pos <- data.frame(
    pos = pos, whichseg = whichseg,
    x0 = xy[whichseg, 1],
    y0 = xy[whichseg, 2],
    dseg = dseg[whichseg + 1],
    dtotal = dtotal[whichseg],
    x1 = xy[whichseg + 1, 1],
    y1 = xy[whichseg + 1, 2]
  )
  
  pos$further <- pos$pos - pos$dtotal
  pos$f <- pos$further / pos$dseg
  pos$x <- pos$x0 + pos$f * (pos$x1 - pos$x0)
  pos$y <- pos$y0 + pos$f * (pos$y1 - pos$y0)
  
  pos$theta <- atan2(pos$y0 - pos$y1, pos$x0 - pos$x1)
  
  return(pos[, c("x", "y", "x0", "y0", "x1", "y1", "theta")])
}

transect <- function(tpts, tlen) {
  tpts$thetaT <- tpts$theta + pi / 2
  dx <- tlen * cos(tpts$thetaT)
  dy <- tlen * sin(tpts$thetaT)
  return(
    data.frame(
      x0 = tpts$x + dx,
      y0 = tpts$y + dy,
      x1 = tpts$x - dx,
      y1 = tpts$y - dy
    )
  )
}
# --------------------------------------------------------------------------- #