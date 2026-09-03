#!/usr/bin/env Rscript

# Regression tests for the pre-MA PEcAn-target workflow changes.
# Run from the repository root with:
#   Rscript tests/test_prema_spatial_and_multiroute.R

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("This test requires data.table.", call. = FALSE)
}
suppressPackageStartupMessages(library(data.table))

source("functions_for_try_data.R")
source("pecan_to_sipnet.R")
source("sipnet_writer_unit_contract.R")
source("prema_pecan_trait_ma_functions.R")


# ---------------------------------------------------------------------------
# The pre-MA selector must spatially split ambiguous-species observations.
# ---------------------------------------------------------------------------

try_test <- data.table(
  ObsDataID = 1:4,
  ObservationID = 101:104,
  AccSpeciesID = c("unique_a", "ambiguous", "ambiguous", "ambiguous"),
  TraitName = "test_trait",
  StdValue = 1:4,
  UnitName = "unit"
)

species_map_test <- data.table(
  try_species_id = c("unique_a", "ambiguous", "ambiguous"),
  final_pft = c("PFT_A", "PFT_A", "PFT_B"),
  include = TRUE
)

pft_coordinate_test <- data.table(
  final_pft = c("PFT_A", "PFT_B"),
  lat = c(45, 45),
  lon = c(-100, -80)
)

observation_coordinate_test <- data.table(
  observation_id = 101:104,
  latitude = c(45, 45.05, 45.05, NA),
  longitude = c(-100, -100.05, -80.05, NA)
)

selected_test <- select_prema_pft_observations(
  trydat_use_species = try_test,
  pftspecies = species_map_test,
  pft_name = "PFT_A",
  pft_coordinate_map = pft_coordinate_test,
  observation_coordinate_data = observation_coordinate_test,
  max_distance_km = 250
)

selection_audit_test <- attr(selected_test, "pft_selection_audit")
unassigned_test <- attr(selected_test, "excluded_ambiguous_rows")

stopifnot(
  identical(sort(selected_test$ObservationID), c(101L, 102L)),
  selected_test[ObservationID == 102L, pft_assignment_method] ==
    "nearest_candidate_pft_coordinate",
  selection_audit_test$selected_unique_species_rows == 1L,
  selection_audit_test$selected_spatial_rows == 1L,
  selection_audit_test$excluded_unassigned_candidate_rows == 1L,
  unassigned_test$ObservationID == 104L
)


# ---------------------------------------------------------------------------
# Direct values must not globally block independent derived observations.
# ---------------------------------------------------------------------------

canonical_test <- data.table(
  ObservationID = c(1L, 1L, 2L, 3L),
  ObsDataID = c(11L, 12L, 13L, 14L),
  AccSpeciesID = "species_a",
  SpeciesName = "Species a",
  DatasetID = "dataset_a",
  Reference = "reference_a",
  Latitude = c(45, 45, 46, 47),
  Longitude = c(-100, -100, -101, -102),
  Replicates = 1L,
  Date = NA_character_,
  Time = NA_character_,
  pecan_vname = c(
    "leaf_turnover_rate",
    "leaf_lifespan_md",
    "leaf_lifespan_md",
    "leaf_lifespan_md"
  ),
  StdValue = c(0.6, 2, 4, 5)
)

target_test <- build_prema_pecan_target_observations(
  canonical_try = canonical_test,
  pft_name = "PFT_A",
  seed = 1L
)

turnover_test <- target_test$observations[
  pecan_trait == "leaf_turnover_rate"
]
turnover_audit_test <- target_test$target_audit[
  pecan_trait == "leaf_turnover_rate"
]

stopifnot(
  nrow(turnover_test) == 3L,
  turnover_test[source_observation_id == "1", source_class] ==
    "DIRECT_OBSERVED",
  isTRUE(all.equal(
    sort(turnover_test$target_value),
    sort(c(0.6, 0.25, 0.2))
  )),
  turnover_audit_test$target_status ==
    "READY_FOR_MA_DIRECT_AND_DERIVED",
  turnover_audit_test$n_deduplicated_same_observation == 1L,
  grepl("leaf_lifespan_md", turnover_audit_test$route_sources_used)
)

message("All pre-MA spatial and multi-route tests passed.")
