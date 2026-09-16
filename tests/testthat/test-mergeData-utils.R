# Unit tests for the data-merging helper functions in R/utils.R:
# determineColumns(), mergeDailyData(), mergeMonthlyData(), mergeAnnualData(), mergeData().
#
# Test fixtures are trimmed real AmeriFlux tower data (site CA-NS1, 2001 and 2004; see
# tests/testthat/testdata/README.md for provenance), including NEE, GPP (GPP_DT_*), and
# RECO (RECO_DT_*) columns. Note that CA-NS1 2001 data has no computed GPP/RECO
# partitioning (flagged -9999 in the raw data, i.e. missing after cleaning); only 2004
# has real GPP/RECO values, so the GPP/RECO tests below rely on the 2004 subset.
#
# Drafted with assistance from Claude (Posit Assistant).

## ---- determineColumns() ----

test_that("determineColumns() builds NEE column names without confInt", {
  cols <- determineColumns(
    outputVar = "NEE", NEEpartitioningMethod = "DT",
    ustarThresMethod = "VUT", centralValue = "REF", confInt = NA
  )
  expect_equal(cols, c("TIMESTAMP", "NEE_VUT_REF"))
})

test_that("determineColumns() builds GPP/RECO column names using the partitioning method", {
  gppCols <- determineColumns(
    outputVar = "GPP", NEEpartitioningMethod = "DT",
    ustarThresMethod = "VUT", centralValue = "REF", confInt = NA
  )
  expect_equal(gppCols, c("TIMESTAMP", "GPP_DT_VUT_REF"))

  recoCols <- determineColumns(
    outputVar = "RECO", NEEpartitioningMethod = "DT",
    ustarThresMethod = "CUT", centralValue = "REF", confInt = NA
  )
  expect_equal(recoCols, c("TIMESTAMP", "RECO_DT_CUT_REF"))
})

test_that("determineColumns() adds confidence-interval columns when confInt is supplied", {
  cols <- determineColumns(
    outputVar = "NEE", NEEpartitioningMethod = "DT",
    ustarThresMethod = "VUT", centralValue = "REF", confInt = 95
  )
  expect_equal(cols, c("TIMESTAMP", "NEE_VUT_REF", "NEE_VUT_05", "NEE_VUT_95"))
})

## ---- mergeDailyData(): real CA-NS1 tower data + synthetic BiomeBGC output ----

towerDaily <- data.table::fread(testthat::test_path("testdata", "sample_towerDailyFlux.csv")) |> as.data.frame()

# Build a synthetic BiomeBGC daily output covering the same two years (2001, 2004),
# including Feb 29 in the leap year so the Feb-29-removal/Julian-day-shift logic can be
# exercised against real tower dates.
buildDailyBBGC <- function() {
  makeYear <- function(year, timestepStart) {
    nDays <- if (year %% 4 == 0) 366L else 365L
    data.table::data.table(
      year = year,
      timestep = seq(timestepStart, length.out = nDays),
      day = seq_len(nDays),
      daily_nep = 0.001 * sin(seq_len(nDays) / 30) + 0.0005,
      daily_mr = 0.0008,
      daily_gr = 0.0004,
      daily_hr = 0.0006,
      daily_gpp = 0.002 + 0.0005 * cos(seq_len(nDays) / 30)
    )
  }
  rbind(makeYear(2001, 1), makeYear(2004, 1001))
}
bbgcDaily <- buildDailyBBGC()

test_that("mergeDailyData() merges real tower NEE data with BiomeBGC output and converts units", {
  colToKeep <- determineColumns("NEE", "DT", "VUT", "REF", NA)
  out <- mergeDailyData(towerDaily, bbgcDaily, outputVar = "NEE", colToKeep = colToKeep)

  expect_s3_class(out, "data.table")
  expect_named(out, c("timestep", "year", "day", "fluxTower", "BBGC"))
  # 2004 is a leap year: Feb 29 must be dropped, so 731 - 1 = 730 rows remain
  expect_equal(nrow(out), 730)

  # NEE = -daily_nep, converted to gC/m2/day (x1000); verify by an independent merge
  expected <- merge(
    data.table::data.table(year = bbgcDaily$year, day = bbgcDaily$day, expected = -bbgcDaily$daily_nep * 1000),
    out, by = c("year", "day")
  )
  expect_equal(expected$BBGC, expected$expected, tolerance = 1e-9)
})

test_that("mergeDailyData() drops February 29th and shifts later Julian days by one", {
  colToKeep <- determineColumns("NEE", "DT", "VUT", "REF", NA)
  out <- mergeDailyData(towerDaily, bbgcDaily, outputVar = "NEE", colToKeep = colToKeep)

  out2004 <- out[year == 2004]
  # December 31 2004 (a leap year) is Julian day 366 on the calendar,
  # but must merge against BiomeBGC output day-of-year count (365, since Feb 29 removed).
  expect_equal(max(out2004$day), 365)
  # No day 366 should remain (leap day dropped, later days shifted back by one)
  expect_false(366 %in% out2004$day)
})

# NOTE: mergeDailyData() itself does not mask -9999 values -- that cleaning happens one
# level up, in mergeData(), before it dispatches to mergeDailyData()/mergeMonthlyData()/
# mergeAnnualData(). So the RECO/GPP tests below go through mergeData() (as the module does)
# rather than calling mergeDailyData() directly with a RECO/GPP colToKeep.
test_that("mergeData() computes RECO as the sum of maintenance, growth, and heterotrophic respiration", {
  out <- mergeData(towerDaily, bbgcDaily, timescale = "day", outputVar = "RECO") |> na.omit()
  # Only 2004 has real RECO data (2001 is coded -9999/missing in the raw tower file)
  expect_true(all(out$year == 2004))
  expectedBBGC <- (bbgcDaily$daily_mr + bbgcDaily$daily_gr + bbgcDaily$daily_hr) * 1000
  merged <- merge(
    data.table::data.table(year = bbgcDaily$year, day = bbgcDaily$day, expected = expectedBBGC),
    out, by = c("year", "day")
  )
  expect_true(all(abs(merged$expected - merged$BBGC) < 1e-6))
})

test_that("mergeData() computes GPP directly from daily_gpp", {
  out <- mergeData(towerDaily, bbgcDaily, timescale = "day", outputVar = "GPP") |> na.omit()
  # Only 2004 has real GPP data (2001 is coded -9999/missing in the raw tower file)
  expect_true(all(out$year == 2004))
  merged <- merge(
    data.table::data.table(year = bbgcDaily$year, day = bbgcDaily$day, expected = bbgcDaily$daily_gpp * 1000),
    out, by = c("year", "day")
  )
  expect_true(all(abs(merged$expected - merged$BBGC) < 1e-6))
})

## ---- mergeMonthlyData(): real CA-NS1 tower data + synthetic BiomeBGC output ----

towerMonthly <- data.table::fread(testthat::test_path("testdata", "sample_towerMonthlyFlux.csv")) |> as.data.frame()

buildMonthlyBBGC <- function() {
  months <- rep(1:12, 2)
  years <- rep(c(2001, 2004), each = 12)
  data.table::data.table(
    year = years,
    month = months,
    daily_nep = 0.0006,
    daily_mr = 0.0008, daily_gr = 0.0004, daily_hr = 0.0006,
    daily_gpp = 0.002
  )
}
bbgcMonthly <- buildMonthlyBBGC()

test_that("mergeMonthlyData() merges real tower NEE data by year/month and converts units", {
  colToKeep <- determineColumns("NEE", "DT", "VUT", "REF", NA)
  out <- mergeMonthlyData(towerMonthly, bbgcMonthly, outputVar = "NEE", colToKeep = colToKeep)

  expect_named(out, c("year", "month", "fluxTower", "BBGC"))
  expect_equal(nrow(out), 24) # 12 months x 2 years
  expect_true(all(abs(out$BBGC - (-0.0006 * 1000)) < 1e-9))
})

test_that("mergeData() computes GPP from real 2004 monthly tower data", {
  out <- mergeData(towerMonthly, bbgcMonthly, timescale = "month", outputVar = "GPP") |> na.omit()
  # Only 2004 has real monthly GPP data
  expect_true(all(out$year == 2004))
  expect_true(all(abs(out$BBGC - 0.002 * 1000) < 1e-9))
})

## ---- mergeAnnualData(): real CA-NS1 tower data + synthetic BiomeBGC output ----

towerAnnual <- data.table::fread(testthat::test_path("testdata", "sample_towerAnnualFlux.csv")) |> as.data.frame()

buildAnnualBBGC <- function() {
  data.table::data.table(
    year = c(2001, 2004),
    daily_nep = c(0.0005, -0.0002),
    daily_mr = 0.0008, daily_gr = 0.0004, daily_hr = 0.0006,
    daily_gpp = 0.002
  )
}
bbgcAnnual <- buildAnnualBBGC()

test_that("mergeAnnualData() without a confidence interval converts units and keeps only tower/BBGC means", {
  colToKeep <- determineColumns("NEE", "DT", "VUT", "REF", NA)
  out <- mergeAnnualData(towerAnnual, bbgcAnnual, outputVar = "NEE", colToKeep = colToKeep, confInt = NA)

  expect_named(out, c("year", "fluxTower", "BBGC"))
  expect_equal(nrow(out), 2)
  expect_equal(out$BBGC, -bbgcAnnual$daily_nep * 1000 * 365, tolerance = 1e-9)
})

test_that("mergeAnnualData() with a confidence interval keeps the min/max tower bounds", {
  colToKeep <- determineColumns("NEE", "DT", "VUT", "REF", 95)
  out <- mergeAnnualData(towerAnnual, bbgcAnnual, outputVar = "NEE", colToKeep = colToKeep, confInt = 95)

  expect_named(out, c("year", "fluxTower", "fluxTower_min", "fluxTower_max", "BBGC"))
  expect_true(all(out$fluxTower_min <= out$fluxTower))
  expect_true(all(out$fluxTower_max >= out$fluxTower))
})

test_that("mergeData() computes RECO from real 2004 annual tower data", {
  out <- mergeData(towerAnnual, bbgcAnnual, timescale = "year", outputVar = "RECO") |> na.omit()
  # Only 2004 has real annual RECO data
  expect_equal(out$year, 2004)
  expectedBBGC <- (bbgcAnnual$daily_mr + bbgcAnnual$daily_gr + bbgcAnnual$daily_hr) * 1000 * 365
  expect_equal(out$BBGC, expectedBBGC[bbgcAnnual$year == 2004], tolerance = 1e-9)
})

## ---- mergeData(): end-to-end wrapper, including the VUT -> CUT fallback ----

test_that("mergeData() dispatches to the correct helper based on timescale", {
  outDay <- mergeData(towerDaily, bbgcDaily, timescale = "day", outputVar = "NEE")
  outMonth <- mergeData(towerMonthly, bbgcMonthly, timescale = "month", outputVar = "NEE")
  outYear <- mergeData(towerAnnual, bbgcAnnual, timescale = "year", outputVar = "NEE")

  expect_named(outDay, c("timestep", "year", "day", "fluxTower", "BBGC"))
  expect_named(outMonth, c("year", "month", "fluxTower", "BBGC"))
  expect_named(outYear, c("year", "fluxTower", "BBGC"))
})

test_that("mergeData() falls back from VUT to CUT when the VUT QC column is absent", {
  towerNoVUT <- towerAnnual
  towerNoVUT$NEE_VUT_REF_QC <- NULL

  out <- mergeData(towerNoVUT, bbgcAnnual, timescale = "year", outputVar = "NEE", ustarThresMethod = "VUT")
  # The 2001 CUT value is heavily gap-filled (QC = 0) and gets masked to NA, then dropped
  # by na.omit() inside mergeAnnualData(); only the 2004 CUT-based row survives.
  expect_equal(nrow(out), 1)
  expect_equal(out$year, 2004)
  # Result should now be based on the CUT reference NEE, not VUT
  expect_equal(out$fluxTower, towerAnnual$NEE_CUT_REF[towerAnnual$TIMESTAMP == 2004])
})

test_that("mergeData() masks poorly gap-filled tower observations (QC < 0.5) as missing", {
  out <- mergeData(towerDaily, bbgcDaily, timescale = "day", outputVar = "NEE") |> na.omit()
  keptTimestamps <- towerDaily$TIMESTAMP[towerDaily$NEE_VUT_REF_QC >= 0.5]
  mergedTimestamps <- as.integer(format(
    as.Date(paste0(out$year, out$day), format = "%Y%j"), "%Y%m%d"
  ))
  expect_true(all(mergedTimestamps %in% keptTimestamps))
})
