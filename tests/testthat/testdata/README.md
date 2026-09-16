# Test fixtures: trimmed AmeriFlux tower data

`sample_towerDailyFlux.csv`, `sample_towerMonthlyFlux.csv`, and `sample_towerAnnualFlux.csv`
are trimmed excerpts of real eddy-covariance flux tower data used to test the data-merging
functions and events of the `BiomeBGC_validationFluxTower` module.

## Source

Site: **CA-NS1** (Old Black Spruce, Saskatchewan), AmeriFlux FLUXNET FULLSET product.

Original files (not included here):
- `AMF_CA-NS1_FLUXNET_FULLSET_DD_2001-2005_3-5.csv` (daily)
- `AMF_CA-NS1_FLUXNET_FULLSET_MM_2001-2005_3-5.csv` (monthly)
- `AMF_CA-NS1_FLUXNET_FULLSET_YY_2001-2005_3-5.csv` (yearly)

## Trimming

- Restricted to two years: **2001** (non-leap) and **2004** (leap), to exercise the
  Feb-29-removal / Julian-day-shift logic in `mergeDailyData()`.
- Restricted to columns needed for the NEE, GPP, and RECO comparison paths: `TIMESTAMP`,
  `NEE_VUT_REF`, `NEE_VUT_REF_QC`, `NEE_CUT_REF`, `NEE_CUT_REF_QC`, `GPP_DT_VUT_REF`,
  `GPP_DT_CUT_REF`, `RECO_DT_VUT_REF`, `RECO_DT_CUT_REF` (plus `NEE_VUT_05`/`NEE_VUT_95`/
  `NEE_CUT_05`/`NEE_CUT_95` in the yearly file, for the confidence-interval path in
  `mergeAnnualData()`).

## Known limitation

CA-NS1's **2001** data has no computed GPP/RECO flux partitioning (coded `-9999`/missing in
the raw FLUXNET file at all three timescales); only **2004** has real GPP/RECO values. Tests
exercising the RECO/GPP code paths therefore only see 2004 rows surviving the module's
missing-value masking, and this is called out explicitly in the relevant test files
(`test-mergeData-utils.R`, `test-events.R`).

---

Drafted with assistance from Claude (Posit Assistant).
