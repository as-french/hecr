#' @title Extract upstream hydrograph boundary condition
#' @description HEC-RAS exports boundary condition hydrographs.
#' @param hc an hdf file read in with the hec_file() function
#' @param verbose boolean
#' @param time_zone character string (e.g. "UTC)
#' @details This function retrieves boundary condition time series related to
#'   upstream boundary condition lines.
#' @examples
#' path = system.file("extdata\\Chippewa_2D.p05.hdf",package = "hecr")
#' h <- hecr::hec_file(path)
#' 
#' upstream_bc <- get_upstream_boundary_condition(hc = h,
#'                                                time_zone = "America/Chicago",
#'                       verbose = TRUE)
#' 
#' # upstream_bc |>
#' #  ggplot()+
#' #  geom_line(aes(x = datetime, y = discharge))
#' @export
get_upstream_boundary_condition <- function(hc,
                                            time_zone,
                                  verbose = FALSE){
  
  flow_area_name <- hec_flow_area(hc)
  
  # for hec ras 6.6
  if(!all(is.na(unlist(hc$attrs)))){
  upstream_bc_name <- hc$object[["Event Conditions"]][["Unsteady"]][["Boundary Conditions"]][["Flow Hydrographs"]]$names
  } else if(all(is.na(unlist(hc$attrs)))){
    # for RAS 2025
    message("RAS2025 results only. See separate plan.h5 file in Plan folder")
    upstream_bc_name <- hc$object[["Boundary Conditions"]][["BC Lines"]][["Stage-Flow Hydrograph"]]$names
  }
  
  
  timestamps <- hec_timestamps(hc,time_zone = time_zone)
  
  # hydrograph is not necessarily same time step as output. e.g., it might be
  # 24h input hydrograph , but output at 1 hour step
  
  if (verbose == TRUE) {
    message("Extracting upstream boundary conditions...",
            paste0("[", Sys.time(), "]"))
  }
  
  time_series <- hc$object[["Event Conditions"]][["Unsteady"]][["Boundary Conditions"]][["Flow Hydrographs"]][[upstream_bc_name]]
  
  upstream_bc_prelim <- hdf5r::readDataSet(time_series) |>
    tibble::as_tibble(.name_repair = 'unique') |>
    suppressMessages() |>
    t() |>
    as.data.frame() |>
    tibble::as_tibble(.name_repair = 'unique') |>
  suppressMessages()
  
  upstream_bc <- 
    upstream_bc_prelim |>
    dplyr::mutate("datetime" = seq(
         from = timestamps[[1]],
         to = timestamps[[length(timestamps)]],
         length.out = nrow(upstream_bc_prelim)
       )) |>
    dplyr::rename("discharge" = "V2") |>
    dplyr::select(-c("V1")) |>
    dplyr::relocate("datetime",.before = "discharge")
  
  if (verbose == TRUE) {
    message("Done!",
            paste0("[", Sys.time(), "]"))
  }
  
  return(upstream_bc)
  
}