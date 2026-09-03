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
  ObservationID = 101:104,
  AccSpeciesID = c("unique_a", "ambiguous", "ambiguous", "ambiguous")
)

# Reproduce the real input contract: coordinates are held separately and the
# lookup uses a differently formatted observation-ID column name.
observation_coordinate_test <- data.table::data.table(
  observation_id = 101:104,
  latitude = c(45, 45.05, 45.05, NA),
  longitude = c(-100, -100.05, -80.05, NA)
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

try_test_with_coordinates <- attach_try_observation_coordinates(
  try_data = try_test,
  observation_coordinate_data = observation_coordinate_test
)
coordinate_join_audit_test <- attr(
  try_test_with_coordinates,
  "observation_coordinate_join_audit"
)

stopifnot(
  coordinate_join_audit_test$try_join_key == "ObservationID",
  coordinate_join_audit_test$lookup_join_key == "observation_id",
  coordinate_join_audit_test$n_coordinates_filled_from_lookup == 3L,
  coordinate_join_audit_test$n_rows_still_without_valid_coordinates == 1L
)

assigned_test <- assign_try_observations_to_pft(
  try_data = try_test_with_coordinates,
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

# Integration regression: prepare_single_pft_try_data_for_ma() must accept a
# TRY trait table with no Latitude/Longitude columns and use the separate
# observation-level lookup before spatial PFT assignment.
prepare_try_test <- data.table::data.table(
  ObservationID = c(201L, 202L),
  AccSpeciesID = c("ambiguous", "ambiguous"),
  TraitName = c("Fraction trait", "Fraction trait"),
  StdValue = c(0.25, 0.75),
  UnitName = c("fraction", "fraction")
)
prepare_coordinate_test <- data.table::data.table(
  observation_id = c(201L, 202L),
  lat = c(45.05, 45.05),
  lon = c(-100.05, -80.05)
)
prepare_trait_map_test <- c("Fraction trait" = "fraction_trait")
prepare_unit_map_test <- data.table::data.table(
  TraitName = "Fraction trait",
  pecan_vname = "fraction_trait",
  canonical_unit = "fraction",
  conversion_id = "fraction"
)

prepared_pft_a_test <- prepare_single_pft_try_data_for_ma(
  trydat_use_species = prepare_try_test,
  pftname = "PFT_A",
  trait_map = prepare_trait_map_test,
  unit_map = prepare_unit_map_test,
  pft_species_map = species_map_test,
  pft_coordinate_map = coordinate_map_test,
  observation_coordinate_data = prepare_coordinate_test,
  max_pft_distance_km = 250,
  unsupported_action = "stop",
  ambiguous_species_action = "warn"
)

stopifnot(
  nrow(prepared_pft_a_test) == 1L,
  prepared_pft_a_test$ObservationID == 201L,
  prepared_pft_a_test$final_pft == "PFT_A",
  prepared_pft_a_test$StdValue == 0.25,
  attr(
    prepared_pft_a_test,
    "observation_coordinate_join_audit"
  )$n_coordinates_filled_from_lookup == 2L
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
  prior_test["fraction_trait", "parama"] > 0,
  prior_test["fraction_trait", "paramb"] > 0,
  is.finite(prior_test["positive_trait", "parama"]),
  all(prior_test$paramb > 0)
)

message("All spatial-PFT and domain-aware-prior tests passed.")
