#coonvert a list of species occurrences with coordinates to a data cube
floppydisk2cube <- function(data_in,
                            aggregate_columns,
                            target_grid,
                            grid_crs,
                            seed, y_col='decimalLatitude', x_col='decimalLongitude') {
  
  #ensure that coordinate columns are numeric
  data_in[[y_col]] <- as.numeric(data_in[[y_col]])
  data_in[[x_col]] <- as.numeric(data_in[[x_col]])
  
  #new coordinates will be created based on randomization found in Oldoni et al.2020
  data_2 <- assign_occurrence_within_uncertainty_circle(
    data_in, 
    seed, 
    #get the ESPG code from grid_crs
    as.character(strsplit(grid_crs$input,':')[[1]][2]),
    y_col = y_col,
    x_col = x_col)
  
  # convert data to a vector layer
  occ <- st_as_sf(data_2, coords = c("x", "y"), crs = grid_crs)

  # project vector layer to EEA grid, in this example EPGS:3035
  occ_proj <- st_transform(occ, crs(target_grid))
  
  #s2 takes too long for even just a few occurrences when the data is in degrees
  sf_use_s2(FALSE)
  
  # intersect spatial occurrences with target grid
  occ_dat <- st_intersection(occ_proj, target_grid)

  occ_dat <- as.data.frame(occ_dat)
  
 
  # aggregate occurrences over user-specified columns AND CellCode
  # keep min of uncertainty column and create column with number of occurrences

  occ_agg <- as.data.frame(occ_dat %>% group_by(across(all_of(c(aggregate_columns, "CellCode")))) %>%
                                  summarise(across('coordinateUncertainty', min), count=n(), .groups="drop")
                            )
  
  occ_agg$coordinateUncertainty <- as.integer(occ_agg$coordinateUncertainty)
  
  colnames(occ_agg)[which(colnames(occ_agg) == "n")] <- "count"
  

  return(occ_agg)
}


filter_missing_coords <- function(data, y_col = "decimalLatitude", x_col = "decimalLongitude") {
  # changed to user-defined columns of coordinates
  data_filt <- data[complete.cases(data[, c(y_col, x_col)]), ]
  #complete cases doesn't recognize certain cases in which there is nothing in these columns
  data_filt <- data_filt[which(data_filt[y_col]!="" | data_filt[x_col]!="" ),]

  return(data_filt)
}

"%nin%" <- Negate("%in%")


# load preset grids
grid_10km <- st_read("eea_grid/Grid_ETRS89-LAEA_10K.shp")
grid_100km <- st_read("eea_grid/Grid_ETRS89-LAEA_100K.shp")


get_corresponding_preset_grid <- function(km) {
  if (km == "10km") {
    
    return(grid_10km)
  } else if (km == "100km") {
    return(grid_100km)
    
  } else if (km == "1km") {
    # not implemented right now because it takes ages to load
    return(grid_1km)
   
  }
}

get_preset_grid_crs <- function(grid){
  espg_code <- switch(grid, '100km'=4326, '1km'=4326, '10km'=4326)
  return(espg_code)
}
merge_cubes <- function(processed_cube, new_cube, map_df) {
  # merge processed cube with a new one e.g. downloaded from GBIF
  print('merging cubes!')
  names(new_cube)[match(map_df$b, names(new_cube))] <- map_df$a
  
  agg_cols <- map_df$a
  
  
  merged_cube <- bind_rows(processed_cube, new_cube) %>% group_by(across(all_of(agg_cols))) %>%
    summarise(
      coordinateUncertainty =
        min(coordinateUncertainty, na.rm = TRUE),
      
      count =
        sum(count, na.rm = TRUE),
      
      .groups = "drop"
    )
  
  
  # delete unnecessary count columns
  merged_cube <- merged_cube %>% select(c(map_df$a, coordinateUncertainty, count))
  
  
  
  
  return(merged_cube)
}

get_uncertainty_time_period <- function(time_periods, value, default_na){
  #read the configuration of period-specific coordinate uncertainty 
  #in the following format e.g. 2000-2010, 500; 2011-2020, 200; 2021-2026, 50
  for (period in time_periods[[1]]){
    
    years <- str_trim(strsplit(period, ',')[[1]][1])
    year_min <- as.integer(str_trim(strsplit(years, '-')[[1]][1]))
    year_max <- as.integer(str_trim(strsplit(years, '-')[[1]][2]))
    
    period_default_na <- as.integer(str_trim(strsplit(period, ',')[[1]][2]))
    
    if (between(value[1], year_min, year_max)){
      return(period_default_na)
    }
      
  }
  #if a corresponding time period was not found return the previously established default
  return(as.integer(default_na))
  
}

assess_uncertainty <- function(data, coord_uncertainty_col = "coordinateUncertainty", default_na = 1000, special_rule=NA) {
  if (is.na(special_rule) ){
    
     
        
      data <- data %>% mutate(coordinateUncertainty = ifelse(is.na(coordinateUncertainty), 
                                                                       default_na, coordinateUncertainty))
      
    
  } else {
    
    time_periods <- strsplit(special_rule, ';')
    
    data <- data %>% mutate(coordinateUncertainty = ifelse(is.na(coordinateUncertainty), 
                                                                   mapply(function(y) get_uncertainty_time_period(time_periods, y, default_na), year), coordinateUncertainty))
    
  }
  
  return(data)
}

assign_occurrence_within_uncertainty_circle <- function(geodata_df, seed, data_projection="4326", y_col = "decimalLatitude", x_col = "decimalLongitude") {
  #gets a random point from within the circle created from the uncertainty values
  #is necessary for random grid allocation
  

  coordinates(geodata_df) <-as.formula(paste("~", x_col, "+", y_col))
  proj4string(geodata_df) <- CRS(paste("+init=epsg",data_projection, sep=":"))
  geodata_df <- spTransform(geodata_df, CRS("+init=epsg:3035"))
  colnames(geodata_df@coords) <- c("x", "y")
  
  nrow_geodata_df <- nrow(geodata_df)
  
  # Set the seed for reproducibility
  set.seed(as.integer(seed) )
  
  geodata_df@data <-
    geodata_df@data %>%
    mutate(random_angle = runif(nrow_geodata_df, 0, 2*pi))
  geodata_df@data <-
    geodata_df@data %>%
    mutate(random_r = sqrt(runif(
      nrow_geodata_df, 0, 1)) * coordinateUncertainty)
  geodata_df@data <-
    geodata_df@data %>%
    mutate(x = geodata_df@coords[, "x"],
           y = geodata_df@coords[, "y"])
  geodata_df@data <-
    geodata_df@data %>%
    mutate(x = x + random_r * cos(random_angle),
           y = y + random_r * sin(random_angle))
  # x` and `y` are the new coordinates while in `@coords` we keep track of the original coordinates:
  geodata_df@data <-
    geodata_df@data %>%
    select(-c(random_angle, random_r))
  
  return(geodata_df)
}


