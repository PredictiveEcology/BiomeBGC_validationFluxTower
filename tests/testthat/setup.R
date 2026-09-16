# Source the module's helper functions (R/utils.R) so function-level tests
# (test-mergeData-utils.R) can call them directly without a full simInit().
#
# Drafted with assistance from Claude (Posit Assistant).

library(data.table)

moduleRDir <- testthat::test_path("..", "..", "R")
for (f in list.files(moduleRDir, pattern = "[.]R$", full.names = TRUE)) {
  source(f)
}
