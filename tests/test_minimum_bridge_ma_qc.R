#!/usr/bin/env Rscript

# Regression test for the relaxed, post-PEcAn bridge QC policy.
# Run from the repository root with:
#   Rscript tests/test_minimum_bridge_ma_qc.R

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("This test requires data.table.", call. = FALSE)
}
if (!requireNamespace("coda", quietly = TRUE)) {
  stop("This test requires coda.", call. = FALSE)
}

source("run_ma_parallel.R")

make_mcmc <- function(center) {
  chain_1 <- coda::mcmc(matrix(
    center + seq(-0.1, 0.1, length.out = 300),
    ncol = 1L,
    dimnames = list(NULL, "beta.o")
  ))
  chain_2 <- coda::mcmc(matrix(
    center + seq(0.1, -0.1, length.out = 300),
    ncol = 1L,
    dimnames = list(NULL, "beta.o")
  ))
  coda::mcmc.list(chain_1, chain_2)
}

traits <- c(
  "diagnostic_only",
  "physical_fail",
  "missing_mcmc"
)

trait.data <- list(
  diagnostic_only = data.frame(mean = c(5, 5), stat = NA, n = c(1, 0)),
  physical_fail = data.frame(mean = c(-1, 2), stat = NA, n = 1),
  missing_mcmc = data.frame(mean = c(1, 2), stat = NA, n = 1)
)

ma_result <- list(
  trait.mcmc = list(
    diagnostic_only = make_mcmc(5),
    physical_fail = make_mcmc(1)
  ),
  post.distns = data.frame(
    distn = rep("norm", length(traits)),
    parama = rep(1, length(traits)),
    paramb = rep(1, length(traits)),
    n = rep(2, length(traits)),
    row.names = traits
  ),
  jagged.data = list(
    diagnostic_only = data.frame(Y = c(5, 5), site = c(1, 1)),
    physical_fail = data.frame(Y = c(-1, 2), site = c(1, 2)),
    missing_mcmc = data.frame(Y = c(1, 2), site = c(1, 2))
  )
)

range_rules <- data.frame(
  trait = traits,
  lower = 0,
  upper = Inf,
  action = "FAIL"
)

minimum_qc <- qc_pecan_ma_result(
  ma_result = ma_result,
  trait.data = trait.data,
  range_rules = range_rules,
  classification_policy = "minimum_bridge",
  save_filtered = FALSE
)

stopifnot(
  minimum_qc$summary[trait == "diagnostic_only", status] == "PASS",
  grepl(
    "IGNORED_FAIL:too_few_unique_values",
    minimum_qc$summary[trait == "diagnostic_only", flags],
    fixed = TRUE
  ),
  grepl(
    "IGNORED_REVIEW:invalid_sample_size_n",
    minimum_qc$summary[trait == "diagnostic_only", flags],
    fixed = TRUE
  ),
  minimum_qc$summary[trait == "physical_fail", status] == "FAIL",
  grepl(
    "FAIL:outside_physical_range",
    minimum_qc$summary[trait == "physical_fail", flags],
    fixed = TRUE
  ),
  minimum_qc$summary[trait == "missing_mcmc", status] == "FAIL",
  grepl(
    "FAIL:mcmc_not_retained_or_beta_o_missing",
    minimum_qc$summary[trait == "missing_mcmc", flags],
    fixed = TRUE
  ),
  identical(
    sort(minimum_qc$passed_traits),
    "diagnostic_only"
  )
)

strict_qc <- qc_pecan_ma_result(
  ma_result = ma_result,
  trait.data = trait.data,
  range_rules = range_rules,
  classification_policy = "strict",
  save_filtered = FALSE
)

stopifnot(
  strict_qc$summary[trait == "diagnostic_only", status] == "FAIL"
)

message("Minimum-bridge MA QC regression test passed.")
