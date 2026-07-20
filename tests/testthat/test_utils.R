library(testthat)
library(sf)
library(dplyr)
library(withr)
library(maps)
library(mapproj)
library(shiny)
library(bslib)
library(terra)


root_dir <- normalizePath(test_path("..", ".."))
local_dir(root_dir)

helpers_env <- new.env()
sys.source(file.path(root_dir, "utils.R"), envir = helpers_env)

# helper references
filter_missing_coords <- helpers_env$filter_missing_coords
check_req_fields <- helpers_env$check_req_fields
assess_uncertainty <- helpers_env$assess_uncertainty
merge_cubes <- helpers_env$merge_cubes
floppydisk2cube <- helpers_env$floppydisk2cube


test_that("filter_missing_coords excludes rows with missing coordinates", {
  input_data <- data.frame(
    decimalLatitude = c(10, NA, 20),
    decimalLongitude = c(5, 15, NA),
    stringsAsFactors = FALSE
  )

  result <- filter_missing_coords(input_data)

  expect_equal(nrow(result), 1)
  expect_equal(result$decimalLatitude, 10)
  expect_equal(result$decimalLongitude, 5)
})


test_that("assess_uncertainty adds default uncertainty when column is missing", {
  data <- data.frame(
    decimalLatitude = 10,
    decimalLongitude = 5,
    stringsAsFactors = FALSE
  )

  result <- assess_uncertainty(data)

  expect_true("coordinateUncertaintyInMeters" %in% names(result))
  expect_equal(result$coordinateUncertaintyInMeters, 1000)
})


test_that("assess_uncertainty replaces NA values in existing uncertainty column", {
  data <- data.frame(
    decimalLatitude = 10,
    decimalLongitude = 5,
    coordinateUncertaintyInMeters = c(NA, 50),
    stringsAsFactors = FALSE
  )
  
  data2 <- data.frame(
    decimalLatitude = 10,
    decimalLongitude = 5,
    coordinateUncertaintyInMeters = c(NA, 50, NA, NA, NA),
    year=c(2015, 2019, 2020, 2023, 2025),
    stringsAsFactors = FALSE
    
  )
  

  result <- assess_uncertainty(data, coord_uncertainty_col = "coordinateUncertaintyInMeters", default_na = 500)
  result2 <- assess_uncertainty(data2, coord_uncertainty_col = "coordinateUncertaintyInMeters", special_rule = '2017-2020, 200; 2021-2026, 10')

  expect_equal(result$coordinateUncertaintyInMeters, c(500, 50))
  expect_equal(result2$coordinateUncertaintyInMeters, c(1000, 50, 200, 10, 10 ))
})


test_that("merge_cubes sums counts for overlapping cube rows", {
  processed_cube <- data.frame(
    eeacellcode = c("A", "B"),
    speciesKey = c(1, 2),
    countryCode = c("US", "FR"),
    year = c(2020, 2021),
    count = c(2L, 3L),
    coordinateUncertaintyInMeters = c(4,15),
    stringsAsFactors = FALSE
  )

  new_cube <- data.frame(
    eeacellcode = c("A", "B"),
    speciesKey = c(1, 2),
    countryCode = c("US", "FR"),
    year = c(2020, 2021),
    count = c(5L, 7L),
    coordinateUncertaintyInMeters = c(30, 5),
    stringsAsFactors = FALSE
  )
  
  map_df <- data.frame(
    a = c("eeacellcode", "speciesKey", "countryCode", "year", 'coordinateUncertaintyInMeters' ),
    b = c("eeacellcode", "speciesKey", "countryCode", "year", 'coordinateUncertaintyInMeters'),
    stringsAsFactors = FALSE
  )
  

  merged <- merge_cubes(new_cube, processed_cube, map_df, col_min='coordinateUncertaintyInMeters')

  expect_equal(nrow(merged), 2)
  expect_equal(merged$count, c(7L, 10L))
})

test_that("get_uncertainty_time_period correctly provides output integer according to defined rule",{
  
  time_periods <- strsplit("2000-2010, 400; 2011-2020, 200; 2021-2026, 50", ';')
  
  result1 <- get_uncertainty_time_period(time_periods, 2026, 1000)
  result2 <- get_uncertainty_time_period(time_periods, 2002, 1000)
  result3 <- get_uncertainty_time_period(time_periods, 2015, 1000)
  result4 <- get_uncertainty_time_period(time_periods, 1998, 1000)
  
  expect_true(class(result1)== 'integer')
  expect_equal(result1, 50)
  expect_equal(result2, 400)
  expect_equal(result3, 200)
  expect_equal(result4, 1000)
})


test_that("floppydisk2cube aggregates occurrences into target grid cells", {
  
  # ---- Create mock occurrence data ----
  occ_data <- data.frame(
    species = c("sp1", "sp1", "sp2"),
    decimalLongitude = c(0.5, 0.6, 1.5),
    decimalLatitude = c(0.5, 0.6, 1.5),
    coordinateUncertaintyInMeters = c(100, 50, 200)
  )
  
  # ---- Create simple target grid ----
  bbox <- st_bbox(c(xmin = 0, ymin = 0, xmax = 2, ymax = 2), crs = 4326)
  
  grid <- st_make_grid(
    st_as_sfc(bbox),
    n = c(2, 2)
  )
  
  target_grid <- st_sf(
    CellCode = c("A1", "A2", "B1", "B2"),
    geometry = grid
  )
  
  # ---- Run function ----
  result <- floppydisk2cube(
    data_in = occ_data,
    aggregate_columns = c("species", "eeacellcode"),
    target_grid = target_grid,
    grid_crs = st_crs(4326), 
    seed=42
  )
  
  # ---- Expectations ----
  
  # Output should be a data.frame
  expect_s3_class(result, "data.frame")
  
  # Expected columns
  expect_true(all(c(
    "species",
    "eeacellcode",
    "coordinateUncertaintyInMeters",
    "count"
  ) %in% names(result)))
  
  # sp1 should aggregate into one cell with min uncertainty = 50
  sp1_row <- result %>%
    filter(species == "sp1")
  
  expect_equal(nrow(sp1_row), 1)
  expect_equal(sp1_row$coordinateUncertaintyInMeters, 50)
  expect_equal(sp1_row$count, 2)
  
  # sp2 should remain single occurrence
  sp2_row <- result %>%
    filter(species == "sp2")
  
  expect_equal(nrow(sp2_row), 1)
  expect_equal(sp2_row$coordinateUncertaintyInMeters, 200)
  expect_equal(sp2_row$count, 1)
})
