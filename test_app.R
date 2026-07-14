library(testthat)
library(shiny)
library(withr)

root_dir <- normalizePath(test_path("..", ".."))
local_dir(root_dir)

app_env <- new.env()
sys.source(file.path(root_dir, "app.R"), envir = app_env)


test_that("app.R defines ui and server", {
  expect_true(is.function(app_env$server))
  expect_true(inherits(app_env$ui, c("shiny.tag", "shiny.tag.list")))
})


test_that("server retrieve_file reads CSV and filters missing coordinates", {
  sample_data <- data.frame(
    countryCode = "US",
    scientificName = "Test species",
    decimalLatitude = c(1, NA),
    decimalLongitude = c(1, 2),
    year = c(2020, 2021),
    specieskey = c(101, 102),
    coordinateUncertaintyInMeters = c(50, 50),
    stringsAsFactors = FALSE
  )

  tmp_csv <- tempfile(fileext = ".csv")
  write.csv(sample_data, tmp_csv, row.names = FALSE)

  testServer(app_env$server, {
    session$setInputs(
      file1 = list(datapath = tmp_csv),
      header = TRUE,
      sep = ",",
      quote = '"'
    )

    expect_equal(nrow(retrieve_file()), 1)
    expect_equal(retrieve_file()$countryCode, "US")
    expect_equal(retrieve_file()$decimalLatitude, 1)
    expect_equal(retrieve_file()$decimalLongitude, 1)
  })
})
