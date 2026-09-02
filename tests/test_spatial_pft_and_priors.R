#!/usr/bin/env Rscript

# Minimal regression tests for the two fixes introduced in this change.
# Run from the repository root with:
#   Rscript tests/test_spatial_pft_and_priors.R

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("This test requires data.table.", call. = FALSE)
}
suppressPackageStartupMessages(library(data.table))

source("functions_for_try_data.R")
source("generate_prior_distns.R")


# ---------------------------------------------------------------------------
# Observation-level PFT assignment
# ---------------------------------------------------------------------------

try_test <- data.table::data.table(
  ObsDataID = 1:4,
  AccSpeciesID = c("unique_a", "ambiguous", "ambiguous", "ambiguous"),
  Latitude = c(45, 45.05, 45.05, NA),
  Longitude = c(-100, -100.05, -80.05, NA)
)

species_map_test <- data.table::data.table(
  try_species_id = c("unique_a", "ambiguous", "ambiguous"),
  final_pft = c("PFT_A", "PFT_A", "PFT_B"),
  include = TRUE
)

coordinate_map_test <- data.table::data.table(
  final_pft = c("PFT_A", "PFT_B"),
  lat = c(45, 45),
  lon = c(-100, -80)
)

assigned_test <- assign_try_observations_to_pft(
  try_data = try_test,
  pft_species_map = species_map_test,
  pft_coordinate_map = coordinate_map_test,
  max_distance_km = 250,
  unassigned_action = "drop"
)

stopifnot(
  assigned_test[ObsDataID == 1, assigned_final_pft] == "PFT_A",
  assigned_test[ObsDataID == 2, assigned_final_pft] == "PFT_A",
  assigned_test[ObsDataID == 3, assigned_final_pft] == "PFT_B",
  assigned_test[ObsDataID == 4, pft_assignment_status] == "UNASSIGNED",
  assigned_test[ObsDataID == 4, pft_assignment_method] ==
    "ambiguous_species_missing_coordinates"
)


# ---------------------------------------------------------------------------
# Domain-aware prior distributions
# ---------------------------------------------------------------------------

unit_map_test <- data.table::data.table(
  pecan_vname = c(
    "fraction_trait",
    "positive_trait",
    "percent_trait",
    "signed_trait"
  ),
  canonical_unit = c(
    "fraction",
    "year-1",
    "percent leaf dry mass",
    "MPa"
  ),
  conversion_id = c(
    "fraction",
    "rate_year",
    "percent_mass",
    "pressure_mpa"
  )
)

trait_data_test <- list(
  fraction_trait = data.frame(mean = c(0.2, 0.4, 0.6)),
  positive_trait = data.frame(mean = c(1, 2, 3)),
  percent_trait = data.frame(mean = c(30, 40, 50)),
  signed_trait = data.frame(mean = c(-3, -2, -1))
)

prior_test <- make_prior_distns_from_trait_data(
  trait.data = trait_data_test,
  sample_fraction = 1,
  min_sample_n = 2,
  max_sample_n = 2,
  width_multiplier = 5,
  seed = 1,
  unit_map = unit_map_test,
  positive_distribution = "lnorm",
  domain_action = "stop"
)

stopifnot(
  prior_test["fraction_trait", "distn"] == "beta",
  prior_test["positive_trait", "distn"] == "lnorm",
  prior_test["percent_trait", "distn"] == "unif",
  prior_test["signed_trait", "distn"] == "norm",
  prior_test["percent_trait", "parama"] == 0,
  prior_test["percent_trait", "paramb"] == 100,
  all(prior_test$parama > 0 | rownames(prior_test) == "signed_trait"),
  all(prior_test$paramb > 0)
)

message("All spatial-PFT and domain-aware-prior tests passed.")
