if (!testthat::is_testing()) {
  suppressPackageStartupMessages(library(testthat))
  testthat::source_test_helpers(env = globalenv())
}

suppressPackageStartupMessages({
  library(SpaDES.core)
  library(data.table)
})

# Source module functions in R/
moduleRPath <- testthat::test_path("..", "..", "R")
lapply(list.files(moduleRPath, pattern = "[.]R$", full.names = TRUE), source)
