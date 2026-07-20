library(maps)
library(mapproj)
library(shiny)
library(bslib)
library(sf)
library(dplyr)
library(terra)
library(shinyjs)
library(stringr)
library(sp)
#library(R.utils)
#library(data.table)
library(LaF)

source('utils.R')

process_big_file <- function(file_path, output_file, input_sep, input_grid_source, 
                             input_preset_choice='', input_file_grid_datapath='', input_grid_crs='', input_aggregate_cols,
                             input_coordinate_uncertainty_col= 'coordinateUncertainty',
                             input_coordinate_uncertainty_na= 1000,
                             input_use_custom_uncertainty = FALSE,
                             input_custom_uncertainty= '',
                             input_seed=42){
  
  proj_dir <- dirname(file_path)
  
  model <- detect_dm_csv(file_path, sep=input_sep, header=T, fill=T)
  df.laf <- laf_open(model)
  
  n_lines <- determine_nlines(file_path)
  
  index_file <- 1
  
  
  while (index_file < n_lines){
    print(index_file)
    goto(df.laf, index_file)
    df <- next_block(df.laf,nrows=1e5)
    
    
    if(input_coordinate_uncertainty_col %in% names(df)){
    
      df <- df %>% 
        rename("coordinateUncertainty" = input_coordinate_uncertainty_col)
    } else{
      df[,'coordinateUncertainty'] = NA
    }
      
    # every occurrence must have corresponding coordinates
    df_filt <- filter_missing_coords(df)
    
    if (input_grid_source == "preset") {
      # load pre-built grid (e.g. EEA grid 10 km)
      target_grid <- get_corresponding_preset_grid(input_preset_choice)
     
      # define data layer projection
      print(paste('ESPG code of preset grid:', toString(get_preset_grid_crs(input_preset_choice)), sep=' '))
      grid_crs <- st_crs(get_preset_grid_crs(input_preset_choice))
    }
    
    if (input_grid_source == "custom") {
      
      
      target_grid <- st_read(input_file_grid_datapath)
     
      grid_crs <- st_crs(input_grid_crs)
    }
    
    
    if (isFALSE(input_use_custom_uncertainty)){
      
      
       
       corrected_uncertainty <- assess_uncertainty(df_filt, default_na = input_coordinate_uncertainty_na)
       
       
    } else {
      
      #if the user chose time period-specific default coordinate uncertainty
      corrected_uncertainty <- assess_uncertainty(df_filt, default_na= input_coordinate_uncertainty_na, special_rule=input_custom_uncertainty)
      
     
    }
    
    
    # main function for cubing data
    print('cubing')
    floppydatacube_cur <- floppydisk2cube(data_in = corrected_uncertainty,
                                      aggregate_columns = input_aggregate_cols, 
                                      target_grid = target_grid,
                                      grid_crs = grid_crs,
                                      seed=input_seed)
    
    
    
    if(exists('floppydatacube_all')){
      floppydatacube_all <- bind_rows(floppydatacube_all, floppydatacube_cur)
    } else {
      floppydatacube_all <- floppydatacube_cur
    }
  
    index_file <- index_file + 1e5
  
  }
  print('merging')
  
  merged_chunks <- floppydatacube_all %>% group_by(across(all_of(c(input_aggregate_cols, 'CellCode')))) %>%
    summarise(
      coordinateUncertainty =
        min(coordinateUncertainty, na.rm = TRUE),
      
      count =
        sum(count, na.rm = TRUE),
      
      .groups = "drop"
    )
  
  if (input_coordinate_uncertainty_col != 'None'){
    merged_chunks <- merged_chunks %>%
      rename(!!input_coordinate_uncertainty_col := "coordinateUncertainty")
  }
  print(proj_dir)
  output_file_datapath = paste(proj_dir, output_file, sep='/')
  
  write.csv(merged_chunks, output_file_datapath, row.names = FALSE, quote = F )
}
