#' @title Query 2d Model Results
#' @description hec_two allows you query a time series from a hec-ras model result
#' using one or more coordinates and an optional time stamp.
#' @param hc an hdf file read in with the hec_file() function
#' @param ts_type the time series to extract
#' @param xy a coordinate or set of coordinates either in a dataframe or matrix structure. See
#' details below for more information. 
#' @param time_stamp return only values with this timestamp
#' @param time_zone character string (e.g.., "UTC")
#' @details 
#' You can supply coordinates in as a matrix. The matrix must have two columns, 
#' the first corresponding to the x the second to y. You can supply a dataframe 
#' with two columns, the first for x and the second for y.
#' @examples
#' \dontrun{
#' ## first read in file
#' f <- hec_file("examples/ArdenwoodCreek.p50.hdf")
#' 
#' ## water surface time series at the coordinate 4567654.0, 2167453.0
#' ws <- hec_two(f, xy=c(4567654.0, 2167453.0), "Water Surface")
#' 
#' ## water surface time series at multiple coordinates
#' coords <- c(4567654.0, 2167453.0, 3456124.0, 7856124.0)
#' ws <- hec_two(f, xy=coords, "Water Surface")
#' 
#' ## water surface for a fixed timestamp, useful when querying for large amounts of coordinates. 
#' ws <- hec_two(f, xy=c(4567654.0, 2167453.0), "Water Surface", timestamp="2005-09-12 00:00:00")
#' }
#' @export
hec_two <- function(hc, xy, ts_type = "Water Surface", time_stamp = NULL, time_zone = NULL) {
  
  if(is.null(time_zone)){
    stop("Function cannot extract timestamps.\nSupply a string to the time_zone argument (e.g., 'UTC').")
  }
  
  timestamps <- hec_timestamps(hc,time_zone = time_zone)
  area_name <- hec_flow_area(hc)
  model_center_coordinates <- hec_center_coords(hc, area_name)
  
  # if stamp is supplied make sure it exists, if it does use this as the 
  # single time to extract otherwise use all timestamps
  if (!is.null(time_stamp)) {
    time_idx <- which(timestamps %in% time_stamp)
    if (length(time_idx) == 0) stop("supplied value for time_stamp was not found in the model", 
                                    call. = FALSE)
  } else {
    time_idx <- seq_len(length(timestamps))
  }
  
  input_coordinates <- make_coordinate_df(xy)
  
  coordinates_df <- input_coordinates %>% 
    dplyr::mutate(
      nearest_cell_index = 
        purrr::map2_dbl(.data$x, .data$y, ~get_nearest_cell_center_index(c(.x, .y), model_center_coordinates))
    ) %>% 
    dplyr::distinct(.data$nearest_cell_index, .keep_all = TRUE) %>% 
    dplyr::arrange(.data$nearest_cell_index)

  # checking data type
  # water and sediment
  sed_bed_names <- hc$object[["Results"]][["Unsteady"]][["Output"]][["Output Blocks"]][["Sediment Bed"]][["Unsteady Time Series"]][["2D Flow Areas"]][["Perimeter 1"]]$names
  sed_trans_names <- hc$object[["Results"]][["Unsteady"]][["Output"]][["Output Blocks"]][["Sediment Transport"]][["Unsteady Time Series"]][["2D Flow Areas"]][["Perimeter 1"]]$names
  hydraulic_var_names <- hc$object[["Results"]][["Unsteady"]][["Output"]][["Output Blocks"]][["Base Output"]][["Unsteady Time Series"]][["2D Flow Areas"]][["Perimeter 1"]]$names
  
  # terrain
  fixed_bed_var_names <- hc$object[["Geometry"]][["2D Flow Areas"]][["Perimeter 1"]]$names

  if(ts_type %in% hydraulic_var_names){
    time_series <- hc$object[[hdf_paths$RES_2D_FLOW_AREAS]][[area_name]][[ts_type]][coordinates_df[["nearest_cell_index"]], time_idx]
  } else if(ts_type %in% sed_bed_names){
    time_series <- hc$object[[hdf_paths$SED_BED_RES_2D_FLOW_AREAS]][[area_name]][[ts_type]][coordinates_df[["nearest_cell_index"]], time_idx]
  } else if(ts_type %in% sed_trans_names){
    time_series <- hc$object[[hdf_paths$SED_TRANS_RES_2D_FLOW_AREAS]][[area_name]][[ts_type]][coordinates_df[["nearest_cell_index"]], time_idx]
  } else if(ts_type %in% fixed_bed_var_names){
    time_series_prelim <- hc$object[[hdf_paths$GEOM_2D_AREAS]][[area_name]][[ts_type]][coordinates_df[["nearest_cell_index"]]]
  
    time_series_mat <- matrix(nrow = length(time_series_prelim),
                                      ncol = length(timestamps))
    
    time_series_mat[,1:ncol(time_series_mat)] = time_series_prelim +0.005
    colnames(time_series_mat) = timestamps
    
    time_series <- time_series_mat[coordinates_df[["nearest_cell_index"]], time_idx]
    
    }

  stacked_time_series <- matrix(t(time_series), ncol=1, byrow = TRUE)
  
  hdf_cell_index <- coordinates_df[["nearest_cell_index"]] - 1
  
  tibble::tibble(
    "datetime" = rep(timestamps[time_idx], nrow(coordinates_df)), 
    "plan_id" = hc$attrs$plan_short_id,
    "plan_file" = hc$attr$plan_file,
    "time_series_type" = ts_type, 
    "hdf_cell_index" = rep(hdf_cell_index, each = length(time_idx)), 
    "xin" = rep(coordinates_df$x, each = length(time_idx)),
    "yin" = rep(coordinates_df$y, each = length(time_idx)),
    "value" = as.vector(stacked_time_series)
  )

}

# INTERNALS

hec_flow_area <- function(f) {
  
  if(!all(is.na(unlist(f$attrs)))){
    # for ras 6.6
  path_to_areas <- "Results/Unsteady/Output/Output Blocks/Base Output/Unsteady Time Series/2D Flow Areas"
  names(f$object[[path_to_areas]])
  } else if(all(is.na(unlist(f$attrs)))){
    # for RAS 2025
    path_to_areas <- "Results/Output Blocks/Base Output/2D Flow Areas"
    names(f$object[[path_to_areas]])
  }
  
  }

hec_center_coords <- function(f, area_name) {
  d <- f$object[[hdf_paths$GEOM_2D_AREAS]][[area_name]][["Cells Center Coordinate"]]
  on.exit(d$close())
  
  return(d[,])
}

hec_perimeter_coords <- function(f, area_name) {
  d <- f$object[[hdf_paths$GEOM_2D_AREAS]][[area_name]][["Perimeter"]]
  on.exit(d$close())
  
  return(d[,])
}

hec_boundary_condition_lines <- function(f) {
  d <- f$object[[hdf_paths$GEOM_BC_LINES]][["Polyline Points"]]
  on.exit(d$close())
  
  return(d[,])
}

get_nearest_cell_center_index <- function(coords, nodes) {
  dist <- colSums(sqrt((coords - nodes)^2))
  which.min(dist)[1]
}

make_coordinate_df <- function(x) {
  if (is.matrix(x)) {
    if (anyDuplicated(x)) {
      warning("Duplicate values found in coordinate pairs, only unique pairs were kept")
      return(as.data.frame(matrix(x[!duplicated(x), ], ncol=2, byrow=TRUE, dimnames = list(NULL, c("x", "y")))))
    } else 
      return(as.data.frame(matrix(x, ncol=2, dimnames = list(NULL, c("x", "y"))))) 
  } else if (is.data.frame(x)) {
    if (anyDuplicated(x)) {
      warning("Duplicate values found in coordinates, only unique pairs will be used")
      colnames(x) <- c("x", "y")
      return(x[!duplicated(x), ])
    } else {
      colnames(x) <- c("x", "y")
      return(x)
    }
  } else {
    stop("input coordinates format must be one of matrix or data.frame", call. = FALSE)
  }
}
