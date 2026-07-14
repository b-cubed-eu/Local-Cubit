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
library(LaF)
library(shinyFiles)

source("utils.R")
source("automatic_workflow.R")


# User interface ----
ui <- page_sidebar(
  useShinyjs(),
  titlePanel(
    title = "Cubit"
  ),
  
  # Sidebar panel for inputs ----
  sidebar = sidebar(  
    tags$div(
         tags$img(src = "Cubit-logo.png", height="100%", width="100%", style = "margin-right:10px;")
       ),
    
   
   
    
    selectInput("grid_source", "Choose grid:",
                choices = c(
                  "Use built-in grid" = "preset",
                  "Upload your own" = "custom"
                )
    ),
    
    # Show preset dropdown only if "preset" selected
    conditionalPanel(
      condition = "input.grid_source == 'preset'",
      selectInput("preset_choice", "Select a preset grid:",
                  choices = c(
                    "Grid 100km" = "100km",
                    "Grid 10km" = "10km"
                   # "Grid 1km" = "1km" #too slow so not yet implemented here
                  )
      )
    ),
    
    # Show file upload only if "custom" selected
    conditionalPanel(
      condition = "input.grid_source == 'custom'",
      fileInput("file_grid", "Upload your file:", accept = ".gpkg"),
      
      numericInput(
        "grid_crs",
        "Set data layer projection (EPSG code)",
        value = 4326,
        min = 0
      )
    ),
  
    
    # Horizontal line ----
    tags$hr(),
    
    uiOutput("csv_options_ui"),
    
  
    
    # Horizontal line ----
    tags$hr()
    
    
  ),   
  
  # Output: Data file ----
  navset_card_underline(
    # Show first lines of input data
    nav_panel("Input data", 
              shinyFilesButton('files', label='File select', title='Please select a file', multiple=FALSE),
              verbatimTextOutput("filepaths"),
              tableOutput(outputId = "contents")),
    
    # Show data table
    nav_panel(
      "Cube data",
      uiOutput("cube_config_ui"),
      textOutput("processed")
    ),
    
  
  
    nav_panel(
      "Merge Cubes",
      
      
      tagList(
        fluidRow(
          column(6, strong("Cube A"), 
                  style = "overflow-x: auto;",
                   shinyFilesButton('cubeA', label='File select', title='Please select a file', multiple=FALSE),
                  
                 
                 verbatimTextOutput("cubeA_filepath"),
                
                 tableOutput(outputId = "contents_cubeA") ) ,
         
           
            
          column(6, strong("Cube B"),
                 style = "overflow-x: auto;",
                 shinyFilesButton('cubeB', label='File select', title='Please select a file', multiple=FALSE),
                 verbatimTextOutput("cubeB_filepath"),
               
                 tableOutput(outputId = "contents_cubeB")) 
          
        )
        
      ),
      
      uiOutput("premadecube_config_ui"),
      tags$hr(),
      tagList(
      div(id = "mapping_container")
      ),
      tags$hr(),
      #mapping should disappear automatically if one of the files is changed
      uiOutput("mapping_builder_ui"),
      
      actionButton("add_mapping", "➕ Add mapping"),
      actionButton("run_merge", "Merge cubes"),
      
      tags$hr(),
      
      tableOutput("merged")
      
      
    )
)
)

server <- function(input, output) {
  # set max size of upload to 30MB
  options(shiny.maxRequestSize = 1000 * 1024^2)
  #display generic error message instead of full error trace
  #options(shiny.sanitize.errors = TRUE)
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  shinyFileChoose(input, 'files', roots=volumes, filetypes=c('', 'txt', 'csv', 'tsv'))
  
  #cannot create cubes with new file when one cube has already been created
  
  file_selected <- eventReactive(input$files, {
    req(input$files)
    parseFilePaths(volumes, input$files)
    
  })
  
  
  
  output$csv_options_ui <- renderUI({
    
    req(input$files)
    req(file_selected()$datapath)
    
    tagList(
      
      # Input: Select separator ----
      radioButtons(
        "sep",
        "Separator",
        choices = c(
          Comma = ",",
          Semicolon = ";",
          Tab = "\t"
        ),
        selected = ","
      ),
      
      #unlike the web version, there is no button to choose whether the file has quotes or not
      #cause I cannot pass that argument to detect_dm_csv
      
      actionButton(
        "load_file",
        "Load file"
      )
    )
    
    
  })
  
  # read uploaded file
  retrieve_file <- eventReactive(input$load_file, {
    
    
    model <- detect_dm_csv(file_selected()$datapath, sep=input$sep, header=T, fill=T)
    df.laf <- laf_open(model)
    
    
    #get the first 10 rows of data,
    #enough to show the data was correctly loaded and to get the columns names for the configuration panel
    goto(df.laf, 1)
    df <- next_block(df.laf,nrows=10)
    
    return(df)
  })
  
  output$contents <- renderTable({
    
    return(retrieve_file())
    
  })
  output$filepaths <- renderPrint({
    if (is.integer(input$files)) {
      cat("No files have been selected, please choose one!")
    } else {
      
      file_selected()$datapath[[1]]
      
    }
  })
  

  # create panel for cube configuration based on data in uploaded file
  output$cube_config_ui <- renderUI({
    req(retrieve_file())
    
    # get columns from uploaded file
    cols <- names(retrieve_file())
    
    fluidRow(
      column(6, 
             tagList(
               selectInput(
                 "aggregate_cols",
                 "Columns to aggregate on",
                 choices = cols,
                 multiple = TRUE
               ),
               selectInput(
                 "coordinate_uncertainty_col",
                 "Coordinate uncertainty",
                 choices =c("None" = "None", cols),
                 multiple = FALSE,
                 selected = {
                   hit <- grep("coordinateUncertainty", cols, value = TRUE)[1]
                   if (is.na(hit)) NULL else hit
                 }
               ),
               numericInput(
                 "seed",
                 "Establish seed for random grid allocation",
                 value = 42,
                 min = 0
               ),
               textInput(
                 "output_name",
                 "Output File Name (will be stored in same directory as input)"
               )
               
             )
      ),
      column(6,
             tagList(
               selectInput(
                 "y_col",
                 "Y-coordinate/Latitude",
                 choices =c( cols),
                 multiple = FALSE,
                 selected = {
                   hit <- grep("Latitude", cols, value = TRUE, ignore.case=TRUE)[1]
                   if (is.na(hit)) NULL else hit
                 }
               ),
               selectInput(
                 "x_col",
                 "X-coordinate/Longitude",
                 choices =c(cols),
                 multiple = FALSE,
                 selected = {
                   hit <- grep("Longitude", cols, value = TRUE, ignore.case=TRUE)[1]
                   if (is.na(hit)) NULL else hit
                 }
               ),
               numericInput(
                 "coordinate_uncertainty_na",
                 "Replacement value for missing coordinate uncertainty (meters)",
                 value = 1000,
                 min = 0
               ),
               checkboxInput(
                 "use_custom_uncertainty",
                 "Specify different uncertainty values for time periods",
                 value = FALSE
               ),
               
               disabled(textInput(
                 "custom_uncertainty",
                 "Custom uncertainty (m): e.g. 2000-2010, 500; 2011-2020, 200; 2021-2026, 50"
               ))
               
               
             )
      ),
      actionButton(
        "apply_cube_config",
        "Create cube"
      )
    )
  })
  
  observe({
    
    if (isTRUE(input$use_custom_uncertainty)) {
      
      shinyjs::enable("custom_uncertainty")
      
    } else {
      
      shinyjs::disable("custom_uncertainty")
    }
  })
  
  processing_done <<- reactiveVal(FALSE)
  
  # check if necessary options have been set
  observeEvent(input$apply_cube_config, {
    req(input$aggregate_cols)
    
    processing_done(FALSE)
    
    #if the user chooses a predefined grid
    
    if (input$grid_source == 'preset'){
      
        process_big_file(file_path=file_selected()$datapath, output_file= input$output_name, 
                         input_sep=input$sep,  input_grid_source = 'preset', 
                         input_coordinate_uncertainty_col=input$coordinate_uncertainty_col,
                         input_preset_choice=input$preset_choice, input_aggregate_cols=input$aggregate_cols )
    
    #if the user uploads its own grid
        #should implement something here to check the grid is ok for further processing
    } else if(input$grid_source == 'custom'){
      
        process_big_file(file_path=file_selected()$datapath, output_file= input$output_name, 
                         input_coordinate_uncertainty_col=input$coordinate_uncertainty_col,
                         input_sep=input$sep, input_grid_source = 'custom', input_grid_crs=input$grid_crs,
                         input_file_grid_datapath = input$file_grid$datapath,  input_aggregate_cols=input$aggregate_cols )
      
    }
    
    processing_done(TRUE)
    
  })
  
  output$processed <- renderText({
      req(processing_done())
      #gotta get rid of this message when file is changed
      if (isTRUE(processing_done())){
        "Cube created!"
      }
  })
  
  
  
  ###Merging cubes###
  
  shinyFileChoose(input, 'cubeA', roots=volumes, filetypes=c('', 'txt', 'csv', 'tsv'))
  
  shinyFileChoose(input, 'cubeB', roots=volumes, filetypes=c('', 'txt', 'csv', 'tsv'))
  
  cubeA_selected <- eventReactive(input$cubeA, {
    req(input$cubeA)
    
    if(mapping_counter()>0){
      remove_mapping()
    }
    
    parseFilePaths(volumes, input$cubeA)
    
    
  })
  
  cubeB_selected <- eventReactive(input$cubeB, {
    req(input$cubeB)
    
    if(mapping_counter()>0){
      remove_mapping()
    }
    
    parseFilePaths(volumes, input$cubeB)
    
    
    
  })
  
  retrieve_cubeA <- reactive({
    req(input$cubeA)
    
    #check if file for cube A has been selected
    req(!is.integer(input$cubeA))
    
    
    model <- detect_dm_csv(cubeA_selected()$datapath, sep=',', header=T, fill=T)
    df.laf <- laf_open(model)
    
    #read first 10 rows
    goto(df.laf, 1)
    df <- next_block(df.laf,nrows=10)
    
    
    return(df)
    
  })
  
  output$cubeA_filepath <- renderPrint({
    if (is.integer(input$cubeA)) {
      cat("No files have been selected, please choose one!")
    } else {
      
      cubeA_selected()$datapath[[1]]
      
    }
  })
  
  #small panel showing the first rows of cube A
  output$contents_cubeA <- renderTable({
    
    req(retrieve_cubeA())
    
    return(head(retrieve_cubeA()))
    
  })
  
  retrieve_cubeB <- reactive({
    req(input$cubeB)
    
    req(!is.integer(input$cubeB))
    
    model <- detect_dm_csv(cubeB_selected()$datapath, sep=',', header=T, fill=T)
    df.laf <- laf_open(model)
    
    #read first 10 rows
    goto(df.laf, 1)
    df <- next_block(df.laf,nrows=10)
    
    return(df)
    
  })
  
  output$cubeB_filepath <- renderPrint({
    if (is.integer(input$cubeB)) {
      cat("No files have been selected, please choose one!")
    } else {
      
      cubeB_selected()$datapath[[1]]
      
    }
  })
  
  #small panel showing the first rows of cube B
  output$contents_cubeB <- renderTable({
    req(retrieve_cubeB())
    
    return(head(retrieve_cubeB()))
    
  })
  
  #this will create the column mapping ui for the cubes
  #corresponding to coordinate uncertainty and occurrence counts 
  #cause they must be processed differently
  #it will try to get the columns by itself based on names
  output$premadecube_config_ui <- renderUI({
    req(retrieve_cubeA())
    
    # get columns from uploaded file
    cols <- names(retrieve_cubeA())
    cols2 <- names(retrieve_cubeB())
    
    fluidRow(
      column(6, 
             tagList(
            
               selectInput(
                 "cubeA_uncertainty_col",
                 "Coordinate uncertainty",
                 choices =c(cols),
                 multiple = FALSE,
                 selected = {
                   hit <- grep("coordinateUncertainty", cols, value = TRUE)[1]
                   if (is.na(hit)) NULL else hit
                 }
               ),
             
               selectInput(
                 "cubeA_count_col",
                 "Number of occurrences",
                 choices =c( cols),
                 multiple = FALSE,
                 selected = {
                   hit <- grep("count$", cols, value = TRUE)[1]
                   if (is.na(hit)) NULL else hit
                 }
               ),
               textInput(
                 "output_name_merged_cube",
                 "Output File Name (will be stored in same directory as input)"
               )
              
             )
      ),
      column(6,
             selectInput(
               "cubeB_uncertainty_col",
               "Coordinate uncertainty",
               choices =c( cols2),
               multiple = FALSE,
               selected = {
                 hit <- grep("coordinateUncertainty", cols2, value = TRUE)[1]
                 if (is.na(hit)) NULL else hit
               }
             ),
             
             selectInput(
               "cubeB_count_col",
               "Number of occurrences",
               choices =c( cols2),
               multiple = FALSE,
               selected = {
                 hit <- grep("count$", cols2, value = TRUE)[1]
                 if (is.na(hit)) NULL else hit
               }
             )
             
             
            
               
             )
      )
      
    
  })
  
  
  mapping_counter <- reactiveVal(0)
  
  #this configures the mapping ui for the rest of the columns
  observeEvent(input$add_mapping, {
    
    i <- mapping_counter() + 1
    mapping_counter(i)
    
    cols_a <- names(retrieve_cubeA())
    cols_b <- names(retrieve_cubeB())
    
    insertUI(
      selector = "#mapping_container",
      where = "beforeEnd",
      ui = div(
        id = paste0("map_row_", i),
        
        fluidRow(
          
          #row with input fields that will appear after pressing "add mapping" button       
          column(5,
                 selectInput(
                   paste0("map_a_", i),
                   label = NULL,
                   choices = c("-- skip --" = "", cols_a)
                 )
          ),
          
          column(5,
                 selectInput(
                   paste0("map_b_", i),
                   label = NULL,
                   choices = c("-- skip --" = "", cols_b)
                 )
          ),
          
          column(2,
                 actionButton(paste0("remove_", i), "✖")
          )
        )
      )
    )
  })
  
  observe({
    
    lapply(seq_len(mapping_counter()), function(i) {
      
      observeEvent(input[[paste0("remove_", i)]], {
        
        removeUI(selector = paste0("#map_row_", i))
      }, ignoreInit = TRUE)
    })
  })
  
  #remove all mapping when a new cube file is selected
  remove_mapping <- function(){
    
    lapply(seq_len(mapping_counter()), function(i){
      removeUI(selector = paste0("#map_row_", i))
    })
    
    mapping_counter(0)
  }

  merge_mapping <- reactive({
    
    n <- mapping_counter()
    
    maps <- lapply(seq_len(n), function(i) {
      
      a <- input[[paste0("map_a_", i)]]
      b <- input[[paste0("map_b_", i)]]
      
      if (is.null(a) || a == "" || is.null(b) || b == "") {
        return(NULL)
      }
      
      data.frame(a = a, b = b)
    })
    
    do.call(rbind, maps)
  })
  
  
  #merge both cubes on clicking the Merge Cubes button
  merged_data <- eventReactive(input$run_merge, {
    
    req(retrieve_cubeA(), retrieve_cubeB())
    
    map <- merge_mapping()
    
    validate(
      need(nrow(map) > 0, "Please define at least one mapping")
    )
    
    cubeA <- read.csv(cubeA_selected()$datapath[[1]], header=T)
    cubeB <- read.csv(cubeB_selected()$datapath[[1]], header=T)
    
    proj_dir <- dirname(cubeA_selected()$datapath)
    output_path = paste(proj_dir, input$output_name_merged_cube, sep='/')
    
   
    #make sure coordinate uncertainty column is called like this to feed into dplyr in merge function
    cubeA2merge <- cubeA %>% 
      rename("coordinateUncertainty" = input$cubeA_uncertainty_col, "count"=input$cubeA_count_col)
      
    cubeB2merge <- cubeB %>% 
      rename("coordinateUncertainty" = input$cubeB_uncertainty_col, "count" = input$cubeB_count_col)
    
    map_df <<- map
    
    merged_cube <- merge_cubes(
      cubeA2merge,
      cubeB2merge,
      map
    )
    
    print('Finished merging cubes!')
    
    merged_cube <- merged_cube %>% 
     rename(!!input$cubeA_uncertainty_col := "coordinateUncertainty", !!input$cubeA_count_col := "count")
      
    
    write.csv(merged_cube, file=output_path, quote=F, row.names=F)
    
    
    
    return(merged_cube)
    
  })
  
  output$merged <- renderTable({
    
    req(merged_data())
    
    head(merged_data())
        
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)


