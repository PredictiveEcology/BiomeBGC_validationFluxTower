# Unit tests for .inputObjects() in BiomeBGC_validationFluxTower.R: input validation and
# construction of studyArea/rasterToMatch from user-supplied towerCoordinates.
#
# Drafted with assistance from Claude (Posit Assistant).

library(SpaDES.core)

modulePath <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE) |>
  dirname() |>
  normalizePath(mustWork = TRUE)
moduleName <- "BiomeBGC_validationFluxTower"

minimalFluxData <- function() {
  data.frame(TIMESTAMP = 20010101:20010102, NEE_VUT_REF = c(0.5, 0.6), NEE_VUT_REF_QC = c(1, 1))
}

test_that(".inputObjects() errors when towerCoordinates is not supplied", {
  expect_error(
    simInit(
      times = list(start = 0, end = 1),
      params = list(.globals = list(verbose = FALSE)),
      modules = list(moduleName),
      objects = list(
        towerDailyFlux = minimalFluxData(),
        towerMonthlyFlux = minimalFluxData(),
        towerAnnualFlux = minimalFluxData()
      ),
      paths = list(modulePath = modulePath, outputPath = file.path(tempdir(), "outputs"))
    ),
    regexp = "EC site"
  )
})

test_that(".inputObjects() errors when the flux data.frames are not supplied", {
  expect_error(
    simInit(
      times = list(start = 0, end = 1),
      params = list(.globals = list(verbose = FALSE)),
      modules = list(moduleName),
      objects = list(towerCoordinates = c(lon = -98.48, lat = 55.88)),
      paths = list(modulePath = modulePath, outputPath = file.path(tempdir(), "outputs"))
    ),
    regexp = "flux data"
  )
})

test_that(".inputObjects() builds a studyArea SpatVector from towerCoordinates", {
  sim <- simInit(
    times = list(start = 0, end = 1),
    params = list(.globals = list(verbose = FALSE)),
    modules = list(moduleName),
    objects = list(
      towerCoordinates = c(lon = -98.48, lat = 55.88),
      towerDailyFlux = minimalFluxData(),
      towerMonthlyFlux = minimalFluxData(),
      towerAnnualFlux = minimalFluxData()
    ),
    paths = list(modulePath = modulePath, outputPath = file.path(tempdir(), "outputs"))
  )

  expect_s4_class(sim$studyArea, "SpatVector")
  expect_equal(terra::crs(sim$studyArea), terra::crs(P(sim, "targetCRS", "BiomeBGC_validationFluxTower")))
  expect_equal(terra::geomtype(sim$studyArea), "points")
})
