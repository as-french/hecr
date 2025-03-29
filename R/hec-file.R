#' @title Read hec file
#' @description read in a hdf file resulting from a hec-ras model run
#' @param path string path to hdf file
#' @export
#' @return a hec object
hec_file <- function(path) {
  filename <- basename(path)
  
  # if hecras up to version 6.6
  hec <- hdf5r::H5File$new(path, mode="r")

  #if(hec$attr_exists(attr_name = "File Version")){
  info <- hec_info(hec)
 # }
  
  structure(
    list(
      filename=basename(hec$filename),
      attrs=info,
      object=hec
    ), 
    class="hec"
  )
}

#' @method print hec 
#' @export
print.hec <- function(x, ...) {
  cat("A hec object----\n")
  cat("Plan File:", x$attrs$plan_file, "\n")
  cat("Plan Name:", x$attrs$plan_name, "\n")
  cat("Geom Name:", x$attrs$geometry_name, "\n")
  
  cat("\n")
  NextMethod()
}

#' check hecras version 
#' @param hdf5_object string path to hdf file
hecras_version <- function(hdf5_object) {
  if(hdf5_object$attr_exists(attr_name = "File Version")){
  x <- stringr::str_match(hdf5r::h5attr(hdf5_object, "File Version"),
                          "([0-9]+)\\.([0-9]+)\\.*([0-9])*")
  
  
  list(full=x[1, 1],first=x[1,2], second=x[1,3], third=x[4])
  
  } else if(!hdf5_object$attr_exists(attr_name = "File Version")){
    list(full="RAS2025",first=NA, second="RAS2025", third=NA)
  }
}

# get all the top level attributes for the hecras hdf5 file
hec_info <- function(hc) {
  
  info_path <- "Plan Data/Plan Information"
  
  hecras_file_version <- hecras_version(hc)
  
  hecras_file_version$second <- as.numeric(hecras_file_version$second)
  
  # these new versions have a new names for the attributes 
  if (!is.na(hecras_file_version$second)) {
    
  if (as.numeric(hecras_file_version$second) >= 6) {
    
    list(
      plan_short_id = hdf5r::h5attr(hc[[info_path]], 
                                    which = "Plan ShortID"),
      plan_name = hdf5r::h5attr(hc[[info_path]], 
                                which = "Plan Title"), 
      plan_file = stringr::str_extract(hdf5r::h5attr(hc[[info_path]], 
                                                     which = "Plan Filename"), 
                                       "[A-Za-z0-9_-]+\\.[a-z0-9]+$"), 
      computation_time_step = hdf5r::h5attr(hc[[info_path]], 
                                            which = "Computation Time Step Base"), 
      geometry_name = stringr::str_extract(hdf5r::h5attr(hc[[info_path]], 
                                                         which = "Geometry Filename"), "[A-Za-z0-9_-]+\\.[a-z0-9]+$"), 
      geometry_title = hdf5r::h5attr(hc[[info_path]], 
                                     which = "Geometry Title"), 
      output_interval = hdf5r::h5attr(hc[[info_path]], 
                                      which = "Base Output Interval")
    )
    
  } else if(as.numeric(hecras_file_version$second) < 6){
    list(
      plan_short_id = hdf5r::h5attr(hc[[info_path]], 
                                    which = "Plan ShortID"),
      plan_name = hdf5r::h5attr(hc[[info_path]], 
                                which = "Plan Name"), 
      plan_file = stringr::str_extract(hdf5r::h5attr(hc[[info_path]], 
                                                     which = "Plan File"), 
                                       "[A-Za-z0-9_-]+\\.[a-z0-9]+$"), 
      computation_time_step = hdf5r::h5attr(hc[[info_path]], 
                                            which = "Computation Time Step"), 
      geometry_name = stringr::str_extract(hdf5r::h5attr(hc[[info_path]], 
                                                         which = "Geometry Name"), "[A-Za-z0-9_-]+\\.[a-z0-9]+$"), 
      geometry_title = hdf5r::h5attr(hc[[info_path]], 
                                     which = "Geometry Title"), 
      output_interval = hdf5r::h5attr(hc[[info_path]], 
                                      which = "Output Interval")
    )
   
  } 
    } else if(is.na(hecras_file_version$second)){
    message("RAS2025 results only. See separate plan.h5 file in Plan folder")
  list(
      plan_short_id = NA,
      plan_name = NA, 
      plan_file = NA, 
      computation_time_step = NA, 
      geometry_name = NA, 
      geometry_title = NA, 
      output_interval = NA
    )
    
  }
  
  
}
