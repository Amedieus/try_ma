#!/usr/bin/env Rscript

# Regression tests for:
#   1. target-PFT point rectangles expanded by +/- one degree at species level;
#   2. target-specific prior construction by unique species.
# Run from the repository root with:
#   Rscript tests/test_pft_range_species_and_prior_split.R

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("This test requires data.table.", call. = FALSE)
}
suppressPackageStartupMessages(library(data.table))

source("functions_for_try_data.R")
source("generate_prior_distns.R")
source("prema_pecan_trait_ma_functions.R")


# ---------------------------------------------------------------------------
# PFT point-range species selection
# ---------------------------------------------------------------------------

try_test <- data.table(
  ObsDataID = 1:11,
  ObservationID = 1:11,
  AccSpeciesID = c(
    "inside", "inside", "boundary", "outside", "outside",
    "no_coordinate", "no_coordinate", "antimeridian", "just_outside",
    "lookup_inside", "not_a_candidate"
  ),
  TraitName = "test_trait",
  StdValue = seq_len(11),
  UnitName = "unit",
  Latitude = c(
    0.5, 5, 1, 5, NA, NA, 999, 20, 1.0001, NA, 0
  ),
  Longitude = c(
    0.5, 5, 1, 5, NA, NA, 0, -179.5, 0, NA, 0
  )
)

pft_species_test <- data.table(
  try_species_id = c(
    "inside", "boundary", "outside", "no_coordinate",
    "antimeridian", "just_outside", "lookup_inside",
    "not_a_candidate"
  ),
  final_pft = c(rep("PFT_A", 7), "PFT_B"),
  include = TRUE
)

pft_points_test <- data.table(
  final_pft = c("PFT_A", "PFT_A", "PFT_A", "PFT_B"),
  latitude = c(0, 10, 20, 0),
  longitude = c(0, 10, 179.5, 0)
)

coordinate_lookup_test <- data.table(
  observation_id = 10L,
  latitude = 10.5,
  longitude = 10.5
)

selected_test <- select_prema_pft_observations(
  trydat_use_species = try_test,
  pftspecies = pft_species_test,
  pft_name = "PFT_A",
  pft_coordinate_map = pft_points_test,
  assignment_mode = "pft_range_species",
  observation_coordinate_data = coordinate_lookup_test,
  pft_range_buffer_degrees = 1
)

range_audit_test <- attr(selected_test, "pft_species_range_audit")
selection_audit_test <- attr(selected_test, "pft_selection_audit")
excluded_test <- attr(selected_test, "excluded_ambiguous_rows")
coordinate_audit_test <- attr(
  selected_test,
  "observation_coordinate_join_audit"
)

stopifnot(
  identical(
    sort(selected_test$ObservationID),
    c(1L, 2L, 3L, 6L, 7L, 8L, 10L)
  ),
  # Species-level rule: after one in-range point, even its out-of-range row
  # remains available to the target PFT analysis.
  2L %in% selected_test$ObservationID,
  range_audit_test[
    species_id == "boundary", eligibility_reason
  ] == "IN_PFT_POINT_BUFFER",
  range_audit_test[
    species_id == "antimeridian", eligibility_reason
  ] == "IN_PFT_POINT_BUFFER",
  range_audit_test[
    species_id == "no_coordinate", eligibility_reason
  ] == "NO_VALID_COORDINATES_INCLUDED",
  range_audit_test[
    species_id == "outside", eligibility_reason
  ] == "KNOWN_COORDINATES_OUTSIDE_PFT_RANGE",
  range_audit_test[
    species_id == "just_outside", eligible_species
  ] == FALSE,
  !("not_a_candidate" %in% range_audit_test$species_id),
  identical(sort(excluded_test$ObservationID), c(4L, 5L, 9L)),
  selection_audit_test$target_reference_points == 3L,
  selection_audit_test$pft_range_buffer_degrees == 1,
  coordinate_audit_test$n_coordinates_filled_from_lookup == 1L
)

missing_points_error <- tryCatch(
  {
    select_prema_pft_observations(
      trydat_use_species = try_test,
      pftspecies = pft_species_test,
      pft_name = "PFT_A",
      pft_coordinate_map = pft_points_test[final_pft == "PFT_B"],
      assignment_mode = "pft_range_species"
    )
    ""
  },
  error = function(e) conditionMessage(e)
)
stopifnot(grepl("no usable points", missing_points_error, fixed = TRUE))


# ---------------------------------------------------------------------------
# Per-target prior species split
# ---------------------------------------------------------------------------

make_target_rows <- function(trait, n_species) {
  species <- sprintf("%s_species_%03d", trait, seq_len(n_species))
  data.table(
    target_observation_id = seq_len(2L * n_species),
    pecan_trait = trait,
    species_key = rep(species, each = 2L),
    target_value = rep(seq_len(n_species), each = 2L) +
      rep(c(0, 2), times = n_species),
    site_key = rep(paste0("site_", seq_len(n_species)), each = 2L)
  )
}

species_counts <- c(
  trait_n1 = 1L,
  trait_n2 = 2L,
  trait_n3 = 3L,
  trait_n19 = 19L,
  trait_n20 = 20L,
  trait_n21 = 21L,
  trait_n100 = 100L
)
target_rows_test <- rbindlist(
  Map(make_target_rows, names(species_counts), species_counts)
)

prior_split_test <- split_prema_prior_species(
  target_rows_test,
  prior_species_fraction = 0.10,
  fraction_rule_min_species = 20L,
  no_holdout_max_species = 2L,
  seed = 41L
)
prior_split_repeat <- split_prema_prior_species(
  target_rows_test,
  prior_species_fraction = 0.10,
  fraction_rule_min_species = 20L,
  no_holdout_max_species = 2L,
  seed = 41L
)

expected_held_out <- data.table(
  pecan_trait = names(species_counts),
  expected = c(0L, 0L, 1L, 1L, 2L, 3L, 10L)
)
heldout_check <- prior_split_test$trait_audit[
  expected_held_out,
  on = "pecan_trait"
]

stopifnot(
  all(heldout_check$n_prior_species_held_out == heldout_check$expected),
  prior_split_test$trait_audit[
    pecan_trait == "trait_n1", n_prior_species_used
  ] == 1L,
  prior_split_test$trait_audit[
    pecan_trait == "trait_n2", n_prior_species_used
  ] == 2L,
  all(prior_split_test$trait_audit[
    pecan_trait %in% c("trait_n1", "trait_n2"),
    prior_likelihood_species_overlap
  ]),
  identical(
    prior_split_test$species_assignments,
    prior_split_repeat$species_assignments
  ),
  identical(
    prior_split_test$likelihood_observations$target_observation_id,
    seq_len(nrow(prior_split_test$likelihood_observations))
  )
)

for (trait_i in names(species_counts)[species_counts > 2L]) {
  held_out_i <- prior_split_test$species_assignments[
    pecan_trait == trait_i & species_role == "PRIOR_HELD_OUT",
    species_key
  ]
  likelihood_i <- unique(
    prior_split_test$likelihood_observations[
      pecan_trait == trait_i,
      species_key
    ]
  )
  stopifnot(length(intersect(held_out_i, likelihood_i)) == 0L)
}

# Every selected prior species contributes all of its source rows, but only
# one species median contributes to the prior parameter calculation.
source_medians <- prior_split_test$prior_source_observations[
  ,
  .(expected_prior_value = median(target_value)),
  by = .(pecan_trait, species_key)
]
prior_value_check <- prior_split_test$prior_species_values[
  source_medians,
  on = c("pecan_trait", "species_key")
]
stopifnot(
  all(prior_value_check$prior_value == prior_value_check$expected_prior_value),
  all(prior_value_check$n_source_rows == 2L),
  all(prior_split_test$likelihood_observations[
    ,
    identical(sort(unique(site_id)), seq_len(uniqueN(site_id))),
    by = pecan_trait
  ]$V1)
)

prior_targets <- names(species_counts)
writer_contract_test <- data.table(
  pecan_trait = prior_targets,
  pecan_input_unit = "test positive unit"
)
prior_bundle_test <- make_prema_species_prior_distns(
  prior_species_values = prior_split_test$prior_species_values,
  targets = prior_targets,
  writer_contract = writer_contract_test,
  unit_map = data.table(),
  seed = 42L
)
prior_n_test <- data.table(
  pecan_trait = rownames(prior_bundle_test$prior.distns),
  prior_n = as.integer(prior_bundle_test$prior.distns$n)
)
prior_n_test <- prior_n_test[
  prior_split_test$trait_audit,
  on = "pecan_trait"
]
stopifnot(
  all(prior_n_test$prior_n == prior_n_test$n_prior_species_used),
  all(prior_bundle_test$prior.distns$distn == "lnorm")
)

message("PFT-range species and species-prior split tests passed.")
