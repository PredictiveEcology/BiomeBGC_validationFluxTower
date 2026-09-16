# Unit tests for the "compareNEE"/"compareRECO"/"compareGPP" doEvent branches of
# BiomeBGC_validationFluxTower.R, run through a full simInit()/spades() simulation.
#
# Test fixtures are trimmed real AmeriFlux tower data (site CA-NS1, 2001 and 2004; see
# tests/testthat/testdata/README.md for provenance), including NEE, GPP, and RECO columns.
# CA-NS1s 2001 data has no computed GPP/RECO partitioning (coded -9999/missing in the raw
# file), so only 2004 contributes rows to the RECO/GPP comparisons below; the BiomeBGC-side
# outputs are synthetic (no BiomeBGC model run is available in this test environment),
# constructed to align on year/day/month with the real tower years and to sit at a small,
# checkable offset from the tower observations.
#
# Drafted with assistance from Claude (Posit Assistant).

library(SpaDES.core)

modulePath <- normalizePath(
  file.path(testthat::test_path(), "..", ".."),
  mustWork = TRUE
) |>
  dirname() |>
  normalizePath(mustWork = TRUE)
moduleName <- "BiomeBGC_validationFluxTower"

towerDaily <- data.table::fread(testthat::test_path(
  "testdata",
  "sample_towerDailyFlux.csv"
)) |>
  as.data.frame()
towerMonthly <- data.table::fread(testthat::test_path(
  "testdata",
  "sample_towerMonthlyFlux.csv"
)) |>
  as.data.frame()
towerAnnual <- data.table::fread(testthat::test_path(
  "testdata",
  "sample_towerAnnualFlux.csv"
)) |>
  as.data.frame()

# Synthetic BiomeBGC output including NEE, RECO, and GPP components, at a small, deliberate,
# checkable offset from the real tower NEE/RECO/GPP.
buildDailyBBGC <- function() {
  makeYear <- function(year, timestepStart) {
    nDays <- if (year %% 4 == 0) 366L else 365L
    data.table::data.table(
      year = year,
      timestep = seq(timestepStart, length.out = nDays),
      day = seq_len(nDays),
      daily_nep = -0.0005 + 0.0002 * sin(seq_len(nDays) / 20),
      daily_mr = 0.0006 + 0.0001 * sin(seq_len(nDays) / 20),
      daily_gr = 0.0003,
      daily_hr = 0.0004,
      daily_gpp = 0.0016 + 0.0002 * cos(seq_len(nDays) / 20)
    )
  }
  rbind(makeYear(2001, 1), makeYear(2004, 1001))
}

buildMonthlyBBGC <- function() {
  data.table::data.table(
    year = rep(c(2001, 2004), each = 12),
    month = rep(1:12, 2),
    daily_nep = -0.0005 + 0.0002 * sin(rep(1:12, 2) / 3),
    daily_mr = 0.0006 + 0.0001 * sin(rep(1:12, 2) / 3),
    daily_gr = 0.0003,
    daily_hr = 0.0004,
    daily_gpp = 0.0016 + 0.0002 * cos(rep(1:12, 2) / 3)
  )
}

buildAnnualBBGC <- function() {
  data.table::data.table(
    year = c(2001, 2004),
    daily_nep = c(-0.0004, -0.0006),
    daily_mr = c(0.0006, 0.0007),
    daily_gr = 0.0003,
    daily_hr = 0.0004,
    daily_gpp = c(0.0016, 0.0018)
  )
}

# A dailyOutput lacking RECO/GPP components, used to test that compareRECO/compareGPP skip
# gracefully (with a message) rather than erroring when those columns are absent.
buildDailyBBGCNoPartitioning <- function() {
  out <- buildDailyBBGC()
  out$daily_mr <- NULL
  out$daily_gr <- NULL
  out$daily_hr <- NULL
  out$daily_gpp <- NULL
  out
}

# A monthlyAverages lacking RECO/GPP components, even though dailyOutput has them. This
# checks that compareRECO/compareGPP skip gracefully when *any* of the three BiomeBGC-side
# inputs (daily/monthly/annual) is missing the required columns, not just dailyOutput.
buildMonthlyBBGCNoPartitioning <- function() {
  out <- buildMonthlyBBGC()
  out$daily_mr <- NULL
  out$daily_gr <- NULL
  out$daily_hr <- NULL
  out$daily_gpp <- NULL
  out
}

runValidationSim <- function(
  dailyOutput,
  monthlyAverages,
  annualAverages,
  outputPath
) {
  dir.create(
    file.path(outputPath, moduleName),
    recursive = TRUE,
    showWarnings = FALSE
  )
  simInit(
    times = list(start = 0, end = 1),
    params = list(
      .globals = list(verbose = FALSE),
      BiomeBGC_validationFluxTower = list(.plots = "none")
    ),
    modules = list(moduleName),
    objects = list(
      towerCoordinates = c(lon = -98.48, lat = 55.88),
      towerDailyFlux = towerDaily,
      towerMonthlyFlux = towerMonthly,
      towerAnnualFlux = towerAnnual,
      dailyOutput = dailyOutput,
      monthlyAverages = monthlyAverages,
      annualAverages = annualAverages
    ),
    paths = list(modulePath = modulePath, outputPath = outputPath)
  ) |>
    spades(debug = FALSE)
}

test_that("the module computes NEE validation metrics at daily/monthly/yearly scales", {
  outputPath <- file.path(
    tempdir(),
    paste0("validationFluxTower_nee_", sample.int(1e6, 1))
  )
  sim <- runValidationSim(
    buildDailyBBGC(),
    buildMonthlyBBGC(),
    buildAnnualBBGC(),
    outputPath
  )

  expect_s4_class(sim, "simList")
  vs <- sim$validationSummary
  expect_true(is.data.frame(vs))
  expect_setequal(
    vs$timescale[vs$estimate == "NEE"],
    c("daily", "month", "year")
  )

  # Independently recompute the daily NEE metrics from the same inputs and compare
  dayComparison <- mergeData(
    towerDaily,
    buildDailyBBGC(),
    timescale = "day",
    outputVar = "NEE"
  ) |>
    na.omit()
  resid <- dayComparison$BBGC - dayComparison$fluxTower
  dailyRow <- vs[vs$estimate == "NEE" & vs$timescale == "daily", ]

  expect_equal(dailyRow$MAE, mean(abs(resid)), tolerance = 1e-9)
  expect_equal(dailyRow$RMSE, sqrt(mean(resid^2)), tolerance = 1e-9)
  expect_equal(dailyRow$Bias, mean(resid), tolerance = 1e-9)
  expect_equal(
    dailyRow$R2,
    cor(dayComparison$BBGC, dayComparison$fluxTower)^2,
    tolerance = 1e-9
  )
})

test_that("the module computes RECO and GPP validation metrics from real 2004 tower data", {
  outputPath <- file.path(
    tempdir(),
    paste0("validationFluxTower_recogpp_", sample.int(1e6, 1))
  )
  sim <- runValidationSim(
    buildDailyBBGC(),
    buildMonthlyBBGC(),
    buildAnnualBBGC(),
    outputPath
  )

  vs <- sim$validationSummary
  # Real GPP/RECO tower data only exists for 2004, so only the daily/monthly/yearly rows
  # derived from 2004 observations should be present
  expect_setequal(
    vs$timescale[vs$estimate == "RECO"],
    c("daily", "month", "year")
  )
  expect_setequal(
    vs$timescale[vs$estimate == "GPP"],
    c("daily", "month", "year")
  )

  # Independently recompute the daily RECO metrics from the same inputs and compare
  recoDay <- mergeData(
    towerDaily,
    buildDailyBBGC(),
    timescale = "day",
    outputVar = "RECO"
  ) |>
    na.omit()
  expect_true(all(recoDay$year == 2004))
  resid <- recoDay$BBGC - recoDay$fluxTower
  recoRow <- vs[vs$estimate == "RECO" & vs$timescale == "daily", ]
  expect_equal(recoRow$MAE, mean(abs(resid)), tolerance = 1e-9)
  expect_equal(recoRow$RMSE, sqrt(mean(resid^2)), tolerance = 1e-9)

  # Independently recompute the daily GPP metrics from the same inputs and compare
  gppDay <- mergeData(
    towerDaily,
    buildDailyBBGC(),
    timescale = "day",
    outputVar = "GPP"
  ) |>
    na.omit()
  expect_true(all(gppDay$year == 2004))
  resid <- gppDay$BBGC - gppDay$fluxTower
  gppRow <- vs[vs$estimate == "GPP" & vs$timescale == "daily", ]
  expect_equal(gppRow$MAE, mean(abs(resid)), tolerance = 1e-9)
  expect_equal(gppRow$RMSE, sqrt(mean(resid^2)), tolerance = 1e-9)
})

test_that("the module writes validationSummary.csv on the save event", {
  outputPath <- file.path(
    tempdir(),
    paste0("validationFluxTower_save_", sample.int(1e6, 1))
  )
  runValidationSim(
    buildDailyBBGC(),
    buildMonthlyBBGC(),
    buildAnnualBBGC(),
    outputPath
  )

  savedFile <- file.path(outputPath, moduleName, "validationSummary.csv")
  expect_true(file.exists(savedFile))
  saved <- data.table::fread(savedFile)
  expect_true(nrow(saved) > 0)
})

test_that("compareRECO and compareGPP are skipped gracefully when their BiomeBGC outputs are absent", {
  outputPath <- file.path(
    tempdir(),
    paste0("validationFluxTower_nogpp_", sample.int(1e6, 1))
  )

  expect_message(
    sim <- runValidationSim(
      buildDailyBBGCNoPartitioning(),
      buildMonthlyBBGC(),
      buildAnnualBBGC(),
      outputPath
    ),
    "skipped"
  )

  vs <- sim$validationSummary
  # NEE should still be computed; RECO and GPP should be absent since dailyOutput has no
  # respiration/GPP components
  expect_true("NEE" %in% vs$estimate)
  expect_false("GPP" %in% vs$estimate)
  expect_false("RECO" %in% vs$estimate)
})

test_that("compareRECO and compareGPP are also skipped when only monthlyAverages is missing RECO/GPP columns", {
  # dailyOutput has full RECO/GPP partitioning, but monthlyAverages does not: the event
  # should still skip gracefully (rather than erroring partway through, once it reaches the
  # monthly comparison) because hasBiomeBGCColumns() checks daily/monthly/annual together.
  outputPath <- file.path(
    tempdir(),
    paste0("validationFluxTower_nogpp_monthly_", sample.int(1e6, 1))
  )

  expect_message(
    sim <- runValidationSim(
      buildDailyBBGC(),
      buildMonthlyBBGCNoPartitioning(),
      buildAnnualBBGC(),
      outputPath
    ),
    "skipped"
  )

  vs <- sim$validationSummary
  expect_true("NEE" %in% vs$estimate)
  expect_false("GPP" %in% vs$estimate)
  expect_false("RECO" %in% vs$estimate)
})
