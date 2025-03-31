---
title: "Mobile bed 2D example HEC-RAS 6.6"
author: "Andrew French"
date: "2025-03-31"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Mobile bed 2D example HEC-RAS 6.6}
  %\VignetteEncoding{UTF-8}
  %\VignetteEngine{knitr::rmarkdown}
editor_options: 
  chunk_output_type: console
---

# Introduction

The purpose of the following section is to illustrate how variables can be
extracted from a HEC-RAS 2D sediment model output hdf file. To facilitate this,
an example hdf file is included, which can be derived by downloading HEC-RAS
example projects, and running a new project plan in the Chippewa_2D sediment
project. Here, all parameters have been left as they were upon download, with
the exception of selecting the conservative turbulence model box, truncating
the simulated time series to finish on 18 April, and setting mapping output
interval to 2h to reduce file size.

Note that cell center velocities are not exported by HEC-RAS 6.6 as default.
These need to be set in the new plan "output options". If not set before running
the plan, these will not be calculated or exported to the hdf file.

## Known artifacts of HEC-RAS 6.6 generated hdf files

For some model variables stored in the hdf file, default values are assigned to
non-wetted areas at each time step. For example, if a cell is not wet, the
default value of water surface elevation is the value of terrain elevation at
that cell. To adjust for this, values for these non-wet areas as set as NA. This
applies to water surface elevation, cell center velocity, sediment load (among
many potential others).

## Read and Explore 



#### Read hdf


``` r

 path = system.file("extdata\\chippewa.hdf",
                    package = "hecr")

 # RAS 2025 compatability is not yet implemented because h5 files are separate
 # for plan, results and geometry etc.
 #   path = system.file("extdata\\Plan (2).h5",
 #                    package = "hecr")

h <- hecr::hec_file(path)

```

#### Get time stamps


``` r

all_time_stamps <- hecr::hec_timestamps(hc = h,
                                        time_zone = "America/Chicago")

range(all_time_stamps)
#> [1] "2019-04-05 00:00:00 CDT" "2019-04-18 22:00:00 CDT"
```

#### Get upsteam hydrograph boundary condition

Here, we want to see what is fed into the upstream boundary condition of the
model 2D domain.


``` r

 upstream_bc <- hecr::get_upstream_boundary_condition(hc = h,
                                                time_zone = "America/Chicago",
                       verbose = TRUE)
#> Extracting upstream boundary conditions...[2025-03-31 13:26:14.737457]
#> Done![2025-03-31 13:26:14.82264]
 
# p1 <-
  upstream_bc |>
  ggplot()+
   labs(title = "Upstream boundary condition",
        y = "Discharge (cfs)")+
  geom_line(aes(x = datetime, y = discharge), col = "blue")+
   scale_x_datetime(breaks = "day")+
   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
```

<img src="mobile_bed_2D_tutorial_files/figure-html/upstream_boundary_condition-1.png" style="display: block; margin: auto;" />

``` r
 
# ggplot2::ggsave(plot = p1,
#                 filename = paste0(system.file("man/figures",package = "hecr"),
#   "/upstream_BC",
#   ".png"
# ),device = "png", width = 15, height = 8, units = "cm")
 
```


#### Get cell coordinates


``` r

 crs_prj <- sf::read_sf(system.file("extdata\\profile_lines.shp",
                                    package = "hecr")) |>
   sf::st_crs()

 comp_cell_centres <- hecr::get_comp_cell_centres(hc = h,
                       crs = crs_prj,
                       verbose = FALSE)
 
# p2 <- 
  comp_cell_centres |>
  ggplot()+
  labs(title = "Computational cell centres",
       subtitle = paste0("Coord system:\n",crs_prj[[1]]))+
  geom_sf(aes(geometry = geom), size = 2)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,hjust = 1))
```

<img src="mobile_bed_2D_tutorial_files/figure-html/get_cell_coords-1.png" style="display: block; margin: auto;" />

``` r

# ggplot2::ggsave(plot = p2,
#                 filename = paste0(system.file("man/figures",package = "hecr"),
#   "/computational_cell_centers",
#   ".png"
# ),device = "png", width = 20, height = 15, units = "cm")

```


#### Get cell bed elevation

Note here that cell bed elevation can be dynamic in sediment models, or static
in hydraulic only models.




``` r
cells_to_keep <- comp_cell_centres |>
  dplyr::pull("hdf_cell_index")

mobile_bed_elev <- hecr::hec_two(hc = h,
                     xy = comp_cell_centres |>
  sf::st_drop_geometry() |>
  dplyr::select("x","y"),
                     ts_type = "Cell Bed Elevation",
  time_zone = "America/Chicago") |>
  dplyr::filter(.data$hdf_cell_index %in% cells_to_keep) |>
  dplyr::rename("bed_elev" = "value")

# p3 <- 
  mobile_bed_elev |>
    dplyr::filter(.data$datetime == all_time_stamps[[1]]) |>
  sf::st_as_sf(coords = c("xin","yin"),crs = crs_prj) |>
  ggplot()+
    labs(title = "Cell bed elevation")+
  geom_sf(aes(geometry = geometry, col = bed_elev), size = 2)+
  scale_color_gradientn(colours = c("brown4","tan3","tan","grey")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,hjust = 1)) +
  facet_wrap(vars(datetime))
```

<img src="mobile_bed_2D_tutorial_files/figure-html/cell_bed_elevation_mobile_bed-1.png" style="display: block; margin: auto;" />

``` r

# ggplot2::ggsave(plot = p3,
#                 filename = paste0(system.file("man/figures",package = "hecr"),
#   "/bed_elevation",
#   ".png"
# ),device = "png", width = 20, height = 15, units = "cm")

```

#### Read hydraulic variables for selected time steps

The structure of the data returned by `hecr::hec_two` is as follows:

|Column Nmae      |Description                                                         |
|:----------------|:-------------------------------------------------------------------|
|datetime         |a datatime object with date and time of record                      |
|plan_id          |a string with the name of the plan                                  |
|plan_file        |a string with the name of file queried from                         |
|time_series_type |the time series that was queried                                    |
|hdf_cell_index   |the cell index from the hdf file that was chosen for the coordinate |
|xin              |the corresponding x-coordinate supplied                             |
|yin              |the corresponding y-coordinate supplied                             |
|values           |the value of the time series                                        |

##### Water Surface Elevation (raw)



##### WSE adjusted and depth

Need to resolve wet from dry cells. This is important because velocity of zero
could mean still water or zero depth.


``` r

wse <- hecr::hec_two(hc = h,
                     xy = comp_cell_centres |>
  sf::st_drop_geometry() |>
  dplyr::select("x","y"),
                     ts_type = "Water Surface",
  time_zone = "America/Chicago") |>
  dplyr::filter(.data$hdf_cell_index %in% cells_to_keep)

wse_adj <- wse |>
  dplyr::left_join(mobile_bed_elev |>
                     dplyr::select(c("hdf_cell_index","datetime","bed_elev")),
                   by = c("hdf_cell_index","datetime")) |>
  dplyr::mutate("value"= dplyr::case_when(
    .data$bed_elev >= value ~ NA,
    .default = value
  )) |>
  dplyr::mutate("depth" = dplyr::case_when(
   !is.na(value) ~ .data$value -.data$bed_elev,
   .default = NA))

# p4 <- 
  wse_adj |>
    dplyr::filter(.data$datetime %in% c(all_time_stamps[[1]],
                                        all_time_stamps[[80]],
                                        all_time_stamps[[100]],
                                        all_time_stamps[[168]])) |>
  sf::st_as_sf(coords = c("xin","yin"),crs = crs_prj) |>
  ggplot()+
        labs(title = "Water surface elevation")+
  geom_sf(aes(geometry = geometry, col = value), size = 0.8)+
  scale_colour_gradientn(colours = c("green3","yellow","orange","red","pink","purple"))+
    scale_x_continuous(
    expand = expansion(mult = c(.8, .8))
  ) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,hjust = 1))+
  facet_wrap(vars(datetime))
```

<img src="mobile_bed_2D_tutorial_files/figure-html/wse_adj-1.png" style="display: block; margin: auto;" />

``` r

# ggplot2::ggsave(plot = p4,
#                 filename = paste0(system.file("man/figures",package = "hecr"),
#   "/wse",
#   ".png"
# ),device = "png", width = 20, height = 15, units = "cm")
```



``` r
# p5 <- 
  wse_adj |>
    dplyr::filter(.data$datetime %in% c(all_time_stamps[[1]],
                                        all_time_stamps[[80]],
                                        all_time_stamps[[100]],
                                        all_time_stamps[[168]])) |>
  sf::st_as_sf(coords = c("xin","yin"),crs = crs_prj) |>
  ggplot()+
        labs(title = "Water depth")+
  geom_sf(aes(geometry = geometry, col = depth), size = 0.8)+
  scale_colour_gradientn(colours = c("tan","skyblue","blue","darkblue"))+
      scale_x_continuous(
    expand = expansion(mult = c(.8, .8))
  ) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,hjust = 1))+
  facet_wrap(vars(datetime))
```

<img src="mobile_bed_2D_tutorial_files/figure-html/depth-1.png" style="display: block; margin: auto;" />

``` r

# ggplot2::ggsave(plot = p5,
#                 filename = paste0(system.file("man/figures",package = "hecr"),
#   "/depth",
#   ".png"
# ),device = "png", width = 20, height = 15, units = "cm")

```

##### Velocity


``` r

velocity_x <- hecr::hec_two(hc = h,
                     xy = comp_cell_centres |>
  sf::st_drop_geometry() |>
  dplyr::select("x","y"),
                     ts_type = "Cell Velocity - Velocity X",
  time_zone = "America/Chicago") |>
  dplyr::rename("vel_x" = "value") |>
  dplyr::filter(.data$hdf_cell_index %in% cells_to_keep)

velocity_y <- hecr::hec_two(hc = h,
                     xy = comp_cell_centres |>
  sf::st_drop_geometry() |>
  dplyr::select("x","y"),
                     ts_type = "Cell Velocity - Velocity Y",
  time_zone = "America/Chicago")|>
  dplyr::rename("vel_y" = "value") |>
  dplyr::filter(.data$hdf_cell_index %in% cells_to_keep)


velocity <- velocity_x |>
  dplyr::left_join(velocity_y |>
                     dplyr::select("datetime","xin","yin","vel_y"),
                   by = c("datetime","xin","yin")) |>
  dplyr::mutate("velocity" = sqrt(.data$vel_y^2 + .data$vel_x^2),
                "velocity_angle" = metR::Angle(x = .data$vel_x,
                                         y = .data$vel_y))

# adjust for dry cells
velocity_adj <- velocity |>
 dplyr::left_join(wse_adj |>
                     dplyr::select(c("hdf_cell_index","datetime","depth")),
                   by = c("hdf_cell_index","datetime")) |>
  dplyr::mutate("velocity"= dplyr::case_when(
    is.na(.data$depth) ~ NA,
    .default = velocity
  ))

# p6 <- 
  velocity_adj |>
  sf::st_as_sf(coords = c("xin","yin"),
               crs = crs_prj, remove = FALSE) |>
      dplyr::filter(.data$datetime == all_time_stamps[[1]]) |>
  ggplot()+
          labs(title = "Velocity")+
  geom_sf(aes(geometry = geometry, col = velocity), size = 2)+
  geom_arrow(aes(x = xin,
                 y = yin,
                 mag = velocity,
                 angle = velocity_angle),
#             arrow.length = 0.5,
             size = 0.2
)+
  scale_colour_gradientn(colours = c("blue","skyblue","yellow","red"))+
        scale_x_continuous(
    expand = expansion(mult = c(.5, .5))
  ) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,hjust = 1),axis.title = element_blank())+
  facet_wrap(vars(datetime))
```

<img src="mobile_bed_2D_tutorial_files/figure-html/velocity-1.png" style="display: block; margin: auto;" />

``` r

# ggplot2::ggsave(plot = p6,
#                 filename = paste0(system.file("man/figures",package = "hecr"),
#   "/velocity",
#   ".png"
# ),device = "png", width = 20, height = 15, units = "cm")
```


#### Sediment model variables

##### Bed Change


``` r
bed_change <- hecr::hec_two(hc = h,
                     xy = comp_cell_centres |>
  sf::st_drop_geometry() |>
  dplyr::select("x","y"),
                     ts_type = "Cell Bed Change",
  time_stamp = all_time_stamps[1:100],
  time_zone = "America/Chicago")  |>
  dplyr::filter(.data$hdf_cell_index %in% cells_to_keep)

# We want to identify cells that never get wet during period in question. This
# is because we are interested net change across the entire period. During this
# period, some areas will go through wet and dry phases, but we do not want to
# set individual cell timestamps to NA as we would for instantaneous variables
# (e.g., velocity and depth), because it would be misleading. Therefore, we only
# filter out cells that are always dry.

always_dry_index <- wse_adj |>
  dplyr::filter(.data$datetime >= min(bed_change$datetime) &
                  .data$datetime <= max(bed_change$datetime)) |>
dplyr::select(c("hdf_cell_index","datetime","depth")) |>
  dplyr::group_by(.data$hdf_cell_index) |>
  dplyr::reframe("always_dry" = dplyr::case_when(
    all(is.na(.data$depth)) ~ TRUE,
    .default = FALSE
  ))

bed_change_adj <- bed_change|>
 dplyr::left_join(always_dry_index |>
                     dplyr::select(c("hdf_cell_index","always_dry")),
                   by = c("hdf_cell_index")) |>
  dplyr::mutate("value"= dplyr::case_when(
    always_dry == TRUE ~ NA,
    .default = value
  ))


# p7 <- 
  bed_change_adj |>
      dplyr::filter(.data$datetime == all_time_stamps[[100]]) |>
  sf::st_as_sf(coords = c("xin","yin"),crs = crs_prj) |>
  ggplot()+
            labs(title = "Bed change")+
  geom_sf(aes(geometry = geometry, col = value), size = 1.5)+
  scale_colour_gradient2(low = "red",mid = "white",high = "blue")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,hjust = 1))+
  facet_wrap(vars(datetime))
```

<img src="mobile_bed_2D_tutorial_files/figure-html/bed_change-1.png" style="display: block; margin: auto;" />

``` r

# ggplot2::ggsave(plot = p7,
#                 filename = paste0(system.file("man/figures",package = "hecr"),
#   "/bed_change",
#   ".png"
# ),device = "png", width = 20, height = 15, units = "cm")

```

##### Transport capacity total


``` r
sed_capacity <- hecr::hec_two(hc = h,
                     xy = comp_cell_centres |>
  sf::st_drop_geometry() |>
  dplyr::select("x","y"),
                     ts_type = "Cell Total-load Capacity - Total",
  time_zone = "America/Chicago")  |>
  dplyr::filter(.data$hdf_cell_index %in% cells_to_keep)

sed_capacity_adj <- sed_capacity |>
  dplyr::left_join(wse_adj |>
                     dplyr::select(c("hdf_cell_index","datetime","depth")),
                   by = c("hdf_cell_index","datetime")) |>
  dplyr::mutate("value"= dplyr::case_when(
    is.na(.data$depth) ~ NA,
    .default = value
  ))

# p8 <- 
  sed_capacity_adj |>
    dplyr::filter(.data$datetime %in% c(all_time_stamps[[1]],
                                        all_time_stamps[[80]],
                                        all_time_stamps[[100]],
                                        all_time_stamps[[168]])) |>
  sf::st_as_sf(coords = c("xin","yin"),crs = crs_prj) |>
  ggplot()+
                labs(title = "Sediment total load capacity")+
  geom_sf(aes(geometry = geometry, col = value), size = 0.8)+
  scale_colour_gradientn(colours = c("skyblue","tan","brown4"),trans = "sqrt")+
          scale_x_continuous(
    expand = expansion(mult = c(.5, .5))
  ) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,hjust = 1))+
  facet_wrap(vars(datetime))
```

<img src="mobile_bed_2D_tutorial_files/figure-html/transport_capacity_total-1.png" style="display: block; margin: auto;" />

``` r

# ggplot2::ggsave(plot = p8,
#                 filename = paste0(system.file("man/figures",package = "hecr"),
#   "/sediment_load_capacity",
#   ".png"
# ),device = "png", width = 20, height = 15, units = "cm")
```

#### Extract thalweg at time stamp zero


``` r
 
 possible_timestamps <- all_time_stamps <- hecr::hec_timestamps(
    hc = h,time_zone = "America/Chicago")

# hc = h
# crs = crs_prj
# number_of_sections = 10
# time_stamp = possible_timestamps[[1]]
# verbose = FALSE

thalweg_points_start <- extract_thalweg_through_2D_domain(hc = h,
                       crs = crs_prj,
                       number_of_sections = 10,
                       time_stamp = possible_timestamps[[1]],
                       verbose = FALSE) 

thalweg_points_end <- extract_thalweg_through_2D_domain(hc = h,
                       crs = crs_prj,
                       number_of_sections = 10,
                       time_stamp = possible_timestamps[[168]],
                       verbose = FALSE)
 
  # plot all geomm outputs
  #  do.call(dplyr::bind_rows,thalweg_points)|>
  # ggplot()+
  #    labs(title = "Approximate thalweg") +
  # geom_sf(aes(geometry = geom, col = transect_id))
  
thalwegs <- thalweg_points_start$thalweg_line |>
  dplyr::mutate("datetime" = as.character(possible_timestamps[[1]])) |>
  dplyr::bind_rows(thalweg_points_end$thalweg_line |>
  dplyr::mutate("datetime" = as.character(possible_timestamps[[168]])))

  # p9 <- 
    wse_adj |>
      dplyr::filter(.data$datetime <= all_time_stamps[[1]]) |>
  sf::st_as_sf(coords = c("xin","yin"),crs = crs_prj) |>
  ggplot()+
        labs(title = "Thalweg planform change")+
    geom_sf(aes(geometry = geometry, col = depth), size = 2, alpha = .2)+
        scale_colour_gradientn(colours = c("tan","skyblue","blue","darkblue"))+
ggnewscale::new_scale_colour()+
           geom_sf(data = thalwegs,
               aes(geometry = geom, col = datetime,
                   linetype = datetime), alpha = .7, linewidth = 1.5) +
    scale_color_manual("Date thalweg extracted",
                       values = c("2019-04-05" = "black",
                       "2019-04-18 22:00:00" = "red"))+
        scale_linetype_manual("Date thalweg extracted",
                              values = c(1,2))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,hjust = 1))
```

<img src="mobile_bed_2D_tutorial_files/figure-html/thalweg_planform-1.png" style="display: block; margin: auto;" />

``` r
   
#    ggplot2::ggsave(plot = p9,
#                 filename = paste0(system.file("man/figures",package = "hecr"),
#   "/thalweg_plan",
#   ".png"
# ),device = "png", width = 20, height = 15, units = "cm")
```


``` r
   # depth change along thalweg
   thalweg_depths <- thalweg_points_start$thalweg_points |>
  dplyr::mutate("datetime" = as.character(possible_timestamps[[1]])) |>
  dplyr::bind_rows(thalweg_points_end$thalweg_points |>
  dplyr::mutate("datetime" = as.character(possible_timestamps[[168]])))
   
   # get longitudinal distances along thalweg
   thalweg_line_to_measure = thalweg_points_start$thalweg_line
   
   thalweg_points_along_line = thalweg_line_to_measure |>
     sf::st_set_agr("constant") |>
     sf::st_cast("POINT")
   
   thalweg_depth_distances <- thalweg_points_along_line |>
     dplyr::mutate("dummy" = NULL,
       "transect_id" = dplyr::row_number(),
       "longitudinal_distance" = sf::st_line_project(line = thalweg_line_to_measure |>
                         dplyr::pull("geom"),
                       point = thalweg_points_along_line|>
                         dplyr::pull("geom"))) |>
     sf::st_drop_geometry() |>
     dplyr::left_join(thalweg_depths, by = "transect_id") |>
     sf::st_as_sf()
   
 # p10 <-
   thalweg_depth_distances |>
     ggplot()+
     labs(title = "Thalweg longitudinal profile",
          y = "Bed elevation",
          x = "Longitudinal distance from upstream reference")+
     geom_line(aes(x = longitudinal_distance, y = bed_elev, col = datetime,
                   linetype = datetime), linewidth = 1)+
         scale_color_manual("Date thalweg extracted",
                       values = c("2019-04-05" = "black",
                       "2019-04-18 22:00:00" = "red"))  +
     scale_linetype_manual("Date thalweg extracted",
                              values = c(1,2))
```

<img src="mobile_bed_2D_tutorial_files/figure-html/thalweg_longitudinal-1.png" style="display: block; margin: auto;" />

``` r
   
#    ggplot2::ggsave(plot = p10,
#                 filename = paste0(system.file("man/figures",package = "hecr"),
#   "/thalweg_long",
#   ".png"
# ),device = "png", width = 20, height = 5, units = "cm")
   
```

## Check attenuation of flood wave along channel

Here, we can extract model results at specific computational nodes to compare
results along the thalweg, moving from upstream to downstream. Comparisons can
be effectively communicated using ggplot visualization.


``` r

thalweg_transect_points <- thalweg_points_start$thalweg_points |>
  sf::st_drop_geometry() |>
  dplyr::select("hdf_cell_index","transect_id")

# WSE
wse_thalweg <- wse_adj |>
  dplyr::left_join(thalweg_transect_points, by = "hdf_cell_index") |>
      dplyr::filter(!is.na(.data$transect_id)) |>
  dplyr::group_by(.data$transect_id) |>
  dplyr::mutate("wse_0_1" = 
                  scales::rescale(
    .data$value,
    to = c(0,1)),
    "wse_0_1_diff" = c(0,diff(.data$wse_0_1)))

# p11 <-
  wse_thalweg |>
  dplyr::filter(.data$transect_id %in% c(3,5,10)) |>
  ggplot()+
  labs(title = "Water surface elevation",
       subtitle = "transect_id 1 is at upstream boundary, 10 is at downstream",
       y = "Standardised water level (0 - 1)")+
  geom_line(aes(x = datetime,y = wse_0_1, col = factor(transect_id)), alpha = .6)+
  scale_x_datetime(breaks = "day")+
  theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1))
```

<img src="mobile_bed_2D_tutorial_files/figure-html/explore_params_along_thalweg-1.png" style="display: block; margin: auto;" />

``` r

#    ggplot2::ggsave(plot = p11,
#                 filename = paste0(system.file("man/figures",package = "hecr"),
#   "/flood_wave_attenuation",
#   ".png"
# ),device = "png", width = 20, height = 10, units = "cm")

```

## Interpolate cells to surfaces for efficient spatial data extraction

In RAS mapper, all displayed variables have been derived from two variables that
were computed by HEC-RAS for each time step during a run: (i) face normal
velocity for each cell face, and (ii) water surface elevation. Other variables
are calculated from these (e.g., cell center velocity X and Y components). The
hdf file allows us to extract these other variables, which we can then
interogate at cell center level, or we can interplolate outputs (similarly to
RAS Mapper), and extract values for specific coordinates. The value of
converting results to a multi-layer rasters, where each layer of a raster
relates to a single timestamp, is that we can use tools such as `terra` for data
extraction, which can be computationally efficient.

