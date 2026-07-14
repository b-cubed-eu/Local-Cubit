library(testthat)
# script_dir <- dirname(sys.frame(1)$file)
# if (is.na(script_dir) || script_dir == "") {
#   script_dir <- getwd()
# }

# Source - https://stackoverflow.com/a/35842176
# Posted by Richie Cotton, modified by community. See post 'Timeline' for change history
# Retrieved 2026-05-13, License - CC BY-SA 3.0

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#root_dir <- normalizePath(file.path(script_dir, ".."))
setwd('..')

test_dir("tests/testthat")

