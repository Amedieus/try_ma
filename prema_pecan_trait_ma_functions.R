options(stringsAsFactors = FALSE)

PREMA_PECAN_PIPELINE_VERSION <- "2026-09-04.2"


# =============================================================================
# TRY canonical observations -> 21 PEcAn targets -> random-effect MA ->
# PEcAn/SIPNET-readable samples
# =============================================================================
#
# This file intentionally contains functions only. Source it from the RStudio
# caller `run_prema_pecan_trait_ma_rstudio.R` after the repository's existing
# mapping, unit, bridge, MA, QC, and writer-contract files have been sourced.
#
# Scientific interpretation
# -------------------------
# - DIRECT_OBSERVED: an already compatible PEcAn target observation.
# - DERIVED_ALGEBRAIC: a deterministic target derived from TRY values.
# - PROXY_MODEL: an empirical/mechanistic scenario model with residual noise.
# - DEFAULT_ONLY: no PFT-specific TRY evidence; this bypasses MA and is used
#   only to complete the downstream 21-column scenario sample contract.
#
# Values entering MA are observations or pseudo-observations, not priors.
# Their MA posteriors become PFT priors for the downstream PEcAn workflow.
# =============================================================================


.prema_require_packages <- function(packages) {
  missing <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing) > 0L) {
    stop(
      "Missing required R packages: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}


.prema_require_functions <- function(function_names) {
  missing <- function_names[
    !vapply(
      function_names,
      exists,
      logical(1),
      mode = "function",
      inherits = TRUE
    )
  ]
  if (length(missing) > 0L) {
    stop(
      "Required repository functions have not been sourced: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}


prema_pecan_targets_21 <- function() {
  c(
    "SLA",
    "leafC",
    "c2n_leaf",
    "c2n_fineroot",
    "c2n_wood",
    "Amax",
    "leaf_respiration_rate_m2",
    "growth_resp_factor",
    "half_saturation_PAR",
    "dVPDSlope",
    "leaf_turnover_rate",
    "root_turnover_rate",
    "wood_turnover_rate",
    "turn_over_time",
    "wueConst",
    "veg_respiration_Q10",
    "fine_root_respiration_Q10",
    "coarse_root_respiration_Q10",
    "root_respiration_rate",
    "stem_respiration_rate",
    "waterRemoveFrac"
  )
}


# Ordered source lists. Every available source can anchor independent target
# observations; the order is used only to resolve duplicate routes for the
# same source observation. Sources that merely triggered a default in the old
# all-enabled bridge but were not used numerically are deliberately excluded.
prema_target_source_registry_21 <- function() {
  list(
    SLA = c(
      "leaf_thickness_md", "leaf_tissue_density_md", "LDMC_md"
    ),
    leafC = c(
      "leafC_area", "SLA", "leaf_thickness_md",
      "leaf_tissue_density_md", "LDMC_md"
    ),
    c2n_leaf = c(
      "leafC_area", "leaf_N_area_md", "leafC", "leaf_N_mass_md",
      "SLA", "leaf_thickness_md", "leaf_tissue_density_md", "LDMC_md"
    ),
    c2n_fineroot = c(
      "fine_root_C_md", "fine_root_N_mass_md", "c2n_root_generic_md",
      "root_C_md", "root_N_mass_md"
    ),
    c2n_wood = c(
      "stem_CN_md", "stem_N_mass_md", "wood_N_mass_md", "stem_C_md"
    ),
    Amax = c(
      "photosynthesis_rate_area_observed_md",
      "photosynthesis_rate_mass_observed_md",
      "PNUE_md", "leaf_N_area_md", "leaf_N_mass_md",
      "Vcmax_area_Tref_md", "Vcmax_mass_Tref_md",
      "Jmax_area_Tref_md", "Jmax_mass_Tref_md",
      "ci_md", "ci_ca", "Rd_Vcmax_fraction_md",
      "leaf_dark_respiration_area_Tmeas_md",
      "leaf_dark_respiration_mass_Tmeas_md",
      "SLA", "leaf_thickness_md", "leaf_tissue_density_md", "LDMC_md"
    ),
    leaf_respiration_rate_m2 = c(
      "leaf_dark_respiration_area_Tmeas_md",
      "leaf_dark_respiration_mass_Tmeas_md",
      "leaf_respiration_Q10_md", "Rd_Vcmax_fraction_md",
      "Vcmax_area_Tref_md", "Vcmax_mass_Tref_md",
      "SLA", "leaf_thickness_md", "leaf_tissue_density_md", "LDMC_md"
    ),
    growth_resp_factor = c(
      "leaf_construction_cost_mass_md",
      "leaf_construction_cost_area_md", "leafC", "SLA",
      "leaf_thickness_md", "leaf_tissue_density_md", "LDMC_md"
    ),
    half_saturation_PAR = c(
      "quantum_yield_md", "LUE_md",
      "photosynthesis_rate_area_observed_md"
    ),
    dVPDSlope = c(
      "leaf_turgor_loss_point_md", "osmotic_potential_tlp_md",
      "osmotic_potential_full_turgor_md", "midday_leaf_water_potential_md"
    ),
    leaf_turnover_rate = c(
      "leaf_lifespan_md", "SLA", "LDMC_md", "leaf_lignin_md",
      "leaf_thickness_md", "leaf_tissue_density_md"
    ),
    root_turnover_rate = c(
      "root_turnover_rate_generic_md",
      "fine_root_tissue_density_md", "root_tissue_density_md",
      "fine_root_diameter_md", "root_diameter_md",
      "fine_root_N_mass_md", "root_N_mass_md",
      "fine_root_RDMC_md", "root_RDMC_md"
    ),
    wood_turnover_rate = c(
      "wood_density_md", "sapwood_density_md", "branch_density_md",
      "stem_DMC_md"
    ),
    turn_over_time = c(
      "litter_decomposition_k_observed_md",
      "litter_decomposition_rate_observed_md",
      "litter_lignin_md", "litter_N_mass_md", "litter_CN_md",
      "cwd_decomposition_k_md", "cwd_decomposition_rate_md"
    ),
    wueConst = c(
      "WUE_md", "ci_ca", "ci_md",
      "photosynthesis_rate_area_observed_md", "leaf_transpiration_area_md"
    ),
    veg_respiration_Q10 = "leaf_respiration_Q10_md",
    fine_root_respiration_Q10 = "leaf_respiration_Q10_md",
    coarse_root_respiration_Q10 = "leaf_respiration_Q10_md",
    root_respiration_rate = c(
      "fine_root_respiration_mass_Tmeas_md",
      "root_respiration_mass_Tmeas_generic_md",
      "fine_root_N_mass_md", "root_N_mass_md",
      "fine_root_tissue_density_md", "root_tissue_density_md",
      "fine_root_diameter_md", "root_diameter_md"
    ),
    stem_respiration_rate = c(
      "stem_N_mass_md", "wood_N_mass_md",
      "wood_density_md", "sapwood_density_md"
    ),
    waterRemoveFrac = c(
      "fine_root_rooting_depth_md", "rooting_depth_md",
      "leaf_hydraulic_conductance_md", "leaf_WSC_md"
    )
  )
}


# Correct the half-saturation dependency list without making it unreadably long
# inside the literal above. Amax itself may be generated from any of these
# sources during the first bridge pass and consumed during the second pass.
.prema_expand_source_registry <- function(registry) {
  registry$half_saturation_PAR <- c(
    "quantum_yield_md", "LUE_md",
    "photosynthesis_rate_area_observed_md",
    "photosynthesis_rate_mass_observed_md",
    "PNUE_md", "leaf_N_area_md", "leaf_N_mass_md",
    "Vcmax_area_Tref_md", "Vcmax_mass_Tref_md",
    "Jmax_area_Tref_md", "Jmax_mass_Tref_md",
    "ci_md", "ci_ca", "Rd_Vcmax_fraction_md",
    "leaf_dark_respiration_area_Tmeas_md",
    "leaf_dark_respiration_mass_Tmeas_md",
    "SLA", "leaf_thickness_md", "leaf_tissue_density_md", "LDMC_md"
  )
  registry
}


prema_default_only_specs_21 <- function() {
  data.table::data.table(
    pecan_trait = prema_pecan_targets_21(),
    center = c(
      7.23, 45.14, 20, 40, 100, 7.37, 0.238, 0.218,
      26.49, 0.0311, 1.025, 0.132, 0.014, 0.40, 10.9,
      2, 2, 2, 750, 33, 0.088
    ),
    distribution = c(
      "lnorm", "lnorm", "lnorm", "lnorm", "lnorm", "lnorm",
      "lnorm", "beta", "lnorm", "lnorm", "lnorm", "lnorm",
      "lnorm", "lnorm", "lnorm", "lnorm", "lnorm", "lnorm",
      "lnorm", "lnorm", "beta"
    ),
    uncertainty = c(
      0.35, 0.15, 0.30, 0.35, 0.40, 0.40, 0.40, 20,
      0.40, 0.40, 0.35, 0.40, 0.40, 0.40, 0.35,
      0.20, 0.20, 0.20, 0.50, 0.50, 25
    ),
    basis = c(
      rep("SIPNET_RUSSELL_SMOKE_REVERSE_OR_DIRECT", 15),
      rep("USER_APPROVED_Q10_CONTEXT_DEFAULT_2", 3),
      rep("CURRENT_ALL_ENABLED_BRIDGE_CONTEXT", 3)
    ),
    note = c(
      rep(
        paste(
          "Scenario fallback only; not PFT-specific TRY evidence and does not",
          "enter the meta-analysis. Replace with an external PEcAn/BETY prior",
          "when available."
        ),
        15
      ),
      rep("Context Q10 default requested for missing TRY Q10 evidence.", 3),
      rep(
        paste(
          "Scenario fallback only; not PFT-specific TRY evidence and does not",
          "enter the meta-analysis."
        ),
        3
      )
    )
  )
}


.prema_value_column <- function(x, name, default = NA) {
  if (name %in% names(x)) x[[name]] else rep(default, nrow(x))
}


.prema_nonempty_text <- function(x) {
  out <- trimws(as.character(x))
  out[is.na(out) | !nzchar(out)] <- NA_character_
  out
}


load_prema_rdata_object <- function(path, preferred_names) {
  if (!file.exists(path)) {
    stop("Cannot find RData input: ", path, call. = FALSE)
  }
  input_env <- new.env(parent = emptyenv())
  loaded_names <- load(path, envir = input_env)
  preferred <- intersect(preferred_names, loaded_names)
  if (length(preferred) == 1L) {
    return(get(preferred[[1L]], envir = input_env, inherits = FALSE))
  }
  if (length(loaded_names) == 1L) {
    return(get(loaded_names[[1L]], envir = input_env, inherits = FALSE))
  }
  stop(
    "Cannot identify one object in ", path,
    ". Objects present: ", paste(loaded_names, collapse = ", "),
    call. = FALSE
  )
}


# Read a coordinate/reference table without loading its objects into the global
# environment. CSV/TSV, RDS, and RData inputs are accepted so the spatial PFT
# workflow can reuse the same files as `ma_prep.R`.
load_prema_table <- function(path, preferred_names = character()) {
  if (length(path) != 1L || is.na(path) || !nzchar(trimws(path))) {
    stop("A non-empty table path is required.", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop("Cannot find table input: ", path, call. = FALSE)
  }

  extension <- tolower(tools::file_ext(path))
  if (extension %in% c("csv", "txt", "tsv")) {
    return(data.table::fread(path, showProgress = FALSE))
  }
  if (extension == "rds") {
    return(readRDS(path))
  }
  if (extension %in% c("rdata", "rda")) {
    return(load_prema_rdata_object(path, preferred_names))
  }
  stop(
    "Unsupported table format `", extension,
    "`. Use CSV, TSV, RDS, RData, or RDA.",
    call. = FALSE
  )
}


# Resolve and normalize the point table used by the PFT-range selector.
.prema_resolve_table_column <- function(x, candidates, label) {
  found <- intersect(candidates, names(x))
  if (length(found) == 0L) {
    stop(
      label, " column was not found. Tried: ",
      paste(candidates, collapse = ", "),
      call. = FALSE
    )
  }
  found[[1L]]
}


.prema_target_pft_points <- function(pft_coordinate_map, pft_name) {
  x <- data.table::copy(data.table::as.data.table(pft_coordinate_map))
  pft_col <- .prema_resolve_table_column(
    x,
    c("final_pft", "pftname", "pft_name", "pft", "PFT"),
    "Reference PFT"
  )
  lat_col <- .prema_resolve_table_column(
    x,
    c("lat", "latitude", "Latitude", "LAT"),
    "Reference latitude"
  )
  lon_col <- .prema_resolve_table_column(
    x,
    c("lon", "longitude", "Longitude", "LON", "long"),
    "Reference longitude"
  )

  points <- unique(data.table::data.table(
    pft = trimws(as.character(x[[pft_col]])),
    latitude = suppressWarnings(as.numeric(as.character(x[[lat_col]]))),
    longitude = suppressWarnings(as.numeric(as.character(x[[lon_col]])))
  ))
  points <- points[
    pft == pft_name &
      is.finite(latitude) & latitude >= -90 & latitude <= 90 &
      is.finite(longitude) & longitude >= -180 & longitude <= 180
  ]
  if (nrow(points) == 0L) {
    stop(
      "pft_coordinate_map has no usable points for target PFT ",
      pft_name, ".",
      call. = FALSE
    )
  }
  data.table::setorder(points, latitude, longitude)
  points[]
}


# Test each coordinate against the union of the latitude/longitude rectangles
# centered on all target-PFT points. Longitude distance is circular so a box
# crossing the antimeridian is handled correctly.
.prema_in_pft_degree_boxes <- function(
    latitude,
    longitude,
    reference_points,
    buffer_degrees = 1
) {
  buffer_degrees <- as.numeric(buffer_degrees)
  if (
    length(buffer_degrees) != 1L ||
    !is.finite(buffer_degrees) ||
    buffer_degrees <= 0 ||
    buffer_degrees > 180
  ) {
    stop("buffer_degrees must be in (0, 180].", call. = FALSE)
  }

  latitude <- suppressWarnings(as.numeric(latitude))
  longitude <- suppressWarnings(as.numeric(longitude))
  inside <- rep(FALSE, length(latitude))
  valid <-
    is.finite(latitude) & latitude >= -90 & latitude <= 90 &
    is.finite(longitude) & longitude >= -180 & longitude <= 180
  if (!any(valid)) return(inside)

  reference <- data.table::copy(data.table::as.data.table(reference_points))
  data.table::setorder(reference, latitude, longitude)
  reference_latitude <- reference$latitude
  reference_longitude <- reference$longitude
  valid_rows <- which(valid)

  # left.open=TRUE makes the lower-bound lookup inclusive when the lower
  # boundary exactly equals a reference latitude.
  lower_index <- findInterval(
    latitude[valid_rows] - buffer_degrees,
    reference_latitude,
    left.open = TRUE
  ) + 1L
  upper_index <- findInterval(
    latitude[valid_rows] + buffer_degrees,
    reference_latitude
  )

  for (position in seq_along(valid_rows)) {
    lower_i <- max(1L, lower_index[[position]])
    upper_i <- min(length(reference_latitude), upper_index[[position]])
    if (lower_i > upper_i) next
    candidate_i <- seq.int(lower_i, upper_i)
    longitude_delta <- abs(
      ((reference_longitude[candidate_i] -
          longitude[valid_rows[[position]]] + 180) %% 360) - 180
    )
    inside[valid_rows[[position]]] <- any(
      longitude_delta <= buffer_degrees
    )
  }
  inside
}


# Select TRY observations for one target PFT using one of three explicit rules.
#
# pft_range_species (default): begin with species listed for the target PFT
# in pftspecies. A candidate species is retained when at least one of its valid
# TRY coordinates is inside any target-PFT point's +/- degree rectangle. A
# species with no valid coordinates at all is retained by an explicit fallback.
# Once eligible, all observations of that species are retained, including rows
# without coordinates.
#
# share_species: retain every observation of every listed species.
#
# spatial_observation: preserve the older nearest-point / km rule.
select_prema_pft_observations <- function(
    trydat_use_species,
    pftspecies,
    pft_name,
    pft_coordinate_map = NULL,
    assignment_mode = c(
      "pft_range_species", "share_species", "spatial_observation"
    ),
    observation_coordinate_data = NULL,
    observation_lat_col = "Latitude",
    observation_lon_col = "Longitude",
    pft_range_buffer_degrees = 1,
    max_distance_km = 250
) {
  .prema_require_packages("data.table")
  assignment_mode <- match.arg(assignment_mode)
  pft_name <- trimws(as.character(pft_name))
  if (length(pft_name) != 1L || is.na(pft_name) || !nzchar(pft_name)) {
    stop("pft_name must be one non-empty string.", call. = FALSE)
  }
  coordinate_modes <- c("pft_range_species", "spatial_observation")
  if (assignment_mode %in% coordinate_modes) {
    .prema_require_functions("attach_try_observation_coordinates")
    if (is.null(pft_coordinate_map)) {
      stop(
        "pft_coordinate_map is required for ", assignment_mode, " mode.",
        call. = FALSE
      )
    }
  }
  if (identical(assignment_mode, "spatial_observation")) {
    .prema_require_functions("assign_try_observations_to_pft")
  }
  try_dt <- data.table::copy(data.table::as.data.table(trydat_use_species))
  pft_dt <- data.table::copy(data.table::as.data.table(pftspecies))

  required_try <- c("AccSpeciesID", "TraitName", "StdValue", "UnitName")
  missing_try <- setdiff(required_try, names(try_dt))
  if (length(missing_try) > 0L) {
    stop(
      "trydat_use_species is missing: ",
      paste(missing_try, collapse = ", "),
      call. = FALSE
    )
  }

  if (!"try_species_id" %in% names(pft_dt)) {
    candidate <- intersect(
      c("AccSpeciesID", "species_id", "TRY_AccSpeciesID"),
      names(pft_dt)
    )
    if (length(candidate) == 0L) {
      stop("pftspecies has no try_species_id column.", call. = FALSE)
    }
    data.table::setnames(pft_dt, candidate[[1L]], "try_species_id")
  }
  if (!"final_pft" %in% names(pft_dt)) {
    candidate <- intersect(
      c("pftname", "pft_name", "pft", "PFT"),
      names(pft_dt)
    )
    if (length(candidate) == 0L) {
      stop("pftspecies has no final_pft column.", call. = FALSE)
    }
    data.table::setnames(pft_dt, candidate[[1L]], "final_pft")
  }

  include_flag <- if ("include" %in% names(pft_dt)) {
    if (is.logical(pft_dt$include)) {
      !is.na(pft_dt$include) & pft_dt$include
    } else {
      tolower(trimws(as.character(pft_dt$include))) %in%
        c("true", "t", "1", "yes", "y")
    }
  } else {
    rep(TRUE, nrow(pft_dt))
  }

  pft_dt <- pft_dt[
    include_flag &
      !is.na(try_species_id) &
      nzchar(trimws(as.character(try_species_id))) &
      !is.na(final_pft) &
      nzchar(trimws(as.character(final_pft)))
  ]
  pft_dt[, species_key__ := trimws(as.character(try_species_id))]
  pft_dt[, pft_key__ := trimws(as.character(final_pft))]
  target_species <- unique(
    pft_dt[pft_key__ == pft_name, species_key__]
  )
  if (length(target_species) == 0L) {
    stop(
      "pftspecies has no included species for target PFT `",
      pft_name, "`.",
      call. = FALSE
    )
  }

  input_try_rows <- nrow(try_dt)
  try_dt[, species_key_prema__ := trimws(as.character(AccSpeciesID))]
  candidates <- data.table::copy(
    try_dt[species_key_prema__ %in% target_species]
  )
  if (nrow(candidates) == 0L) {
    stop(
      "No TRY rows belong to species that are candidates for PFT `",
      pft_name, "`.",
      call. = FALSE
    )
  }

  species_membership <- pft_dt[
    ,
    .(
      pft_candidate_count = data.table::uniqueN(pft_key__),
      pft_candidate_names = paste(
        sort(unique(pft_key__)),
        collapse = " | "
      )
    ),
    by = species_key__
  ]
  candidates[
    species_membership,
    on = .(species_key_prema__ = species_key__),
    `:=`(
      pft_candidate_count = i.pft_candidate_count,
      pft_candidate_names = i.pft_candidate_names
    )
  ]
  candidates[, shared_across_pfts := pft_candidate_count > 1L]

  if (identical(assignment_mode, "share_species")) {
    candidates[, species_key_prema__ := NULL]
    candidates[, `:=`(
      assigned_final_pft = pft_name,
      pft_assignment_method = data.table::fifelse(
        shared_across_pfts,
        "shared_species_membership",
        "unique_species_pft"
      ),
      pft_assignment_distance_km = NA_real_,
      pft_assignment_status = "ASSIGNED"
    )]
    candidates[, final_pft := pft_name]
    candidates[, pft_assignment_method_prema := pft_assignment_method]

    assignment_audit <- candidates[
      ,
      .(
        n_observations = .N,
        n_species = data.table::uniqueN(
          trimws(as.character(AccSpeciesID))
        )
      ),
      by = .(
        pft_assignment_status,
        pft_assignment_method,
        assigned_final_pft
      )
    ]
    unassigned <- candidates[0]
    coordinate_join_audit <- data.table::data.table(
      coordinate_usage = "NOT_USED_SHARE_SPECIES",
      n_try_rows = nrow(candidates),
      n_valid_coordinates_before_join = NA_integer_,
      n_coordinates_filled_from_lookup = 0L,
      n_valid_coordinates_after_join = NA_integer_,
      n_rows_still_without_valid_coordinates = NA_integer_
    )
    assignment_configuration <- list(
      assignment_mode = assignment_mode,
      coordinates_used = FALSE,
      max_distance_km = NA_real_
    )
    audit <- data.table::data.table(
      pft = pft_name,
      assignment_mode = assignment_mode,
      input_try_rows = input_try_rows,
      target_candidate_species = length(target_species),
      target_candidate_rows = nrow(candidates),
      selected_rows = nrow(candidates),
      selected_unique_species_rows = candidates[
        shared_across_pfts == FALSE, .N
      ],
      selected_shared_species_rows = candidates[
        shared_across_pfts == TRUE, .N
      ],
      selected_spatial_rows = 0L,
      excluded_ambiguous_rows = 0L,
      excluded_unassigned_candidate_rows = 0L,
      max_distance_km = NA_real_
    )

    attr(candidates, "pft_selection_audit") <- audit
    attr(candidates, "excluded_ambiguous_rows") <- unassigned
    attr(candidates, "observation_pft_assignment_audit") <-
      assignment_audit
    attr(candidates, "observation_pft_assignment_configuration") <-
      assignment_configuration
    attr(candidates, "observation_coordinate_join_audit") <-
      coordinate_join_audit
    return(candidates[])
  }

  coordinate_columns_present <- all(
    c(observation_lat_col, observation_lon_col) %in% names(candidates)
  )
  if (coordinate_columns_present) {
    latitude <- suppressWarnings(as.numeric(
      as.character(candidates[[observation_lat_col]])
    ))
    longitude <- suppressWarnings(as.numeric(
      as.character(candidates[[observation_lon_col]])
    ))
    valid_before <-
      is.finite(latitude) & latitude >= -90 & latitude <= 90 &
      is.finite(longitude) & longitude >= -180 & longitude <= 180
  } else {
    valid_before <- rep(FALSE, nrow(candidates))
  }

  coordinate_join_audit <- data.table::data.table(
    n_try_rows = nrow(candidates),
    n_valid_coordinates_before_join = sum(valid_before),
    n_coordinates_filled_from_lookup = 0L,
    n_valid_coordinates_after_join = sum(valid_before),
    n_rows_still_without_valid_coordinates = sum(!valid_before)
  )
  if (!is.null(observation_coordinate_data) && any(!valid_before)) {
    candidates <- attach_try_observation_coordinates(
      try_data = candidates,
      observation_coordinate_data = observation_coordinate_data,
      observation_lat_col = observation_lat_col,
      observation_lon_col = observation_lon_col
    )
    coordinate_join_audit <- attr(
      candidates,
      "observation_coordinate_join_audit"
    )
  }

  # `assign_try_observations_to_pft()` accepts missing coordinates for
  # single-PFT species, but it requires the two columns to exist.
  if (!observation_lat_col %in% names(candidates)) {
    candidates[, (observation_lat_col) := NA_real_]
  }
  if (!observation_lon_col %in% names(candidates)) {
    candidates[, (observation_lon_col) := NA_real_]
  }

  if (identical(assignment_mode, "pft_range_species")) {
    buffer_degrees <- as.numeric(pft_range_buffer_degrees)
    if (
      length(buffer_degrees) != 1L ||
      !is.finite(buffer_degrees) ||
      buffer_degrees <= 0 ||
      buffer_degrees > 180
    ) {
      stop(
        "pft_range_buffer_degrees must be in (0, 180].",
        call. = FALSE
      )
    }

    reference_points <- .prema_target_pft_points(
      pft_coordinate_map,
      pft_name
    )
    candidates[, (observation_lat_col) := suppressWarnings(
      as.numeric(as.character(get(observation_lat_col)))
    )]
    candidates[, (observation_lon_col) := suppressWarnings(
      as.numeric(as.character(get(observation_lon_col)))
    )]
    candidates[, coordinate_valid_prema__ :=
      is.finite(get(observation_lat_col)) &
      get(observation_lat_col) >= -90 &
      get(observation_lat_col) <= 90 &
      is.finite(get(observation_lon_col)) &
      get(observation_lon_col) >= -180 &
      get(observation_lon_col) <= 180
    ]
    candidates[, coordinate_in_range_prema__ :=
      .prema_in_pft_degree_boxes(
        latitude = get(observation_lat_col),
        longitude = get(observation_lon_col),
        reference_points = reference_points,
        buffer_degrees = buffer_degrees
      )
    ]

    species_range_audit <- candidates[
      ,
      {
        any_valid <- any(coordinate_valid_prema__)
        any_inside <- any(coordinate_in_range_prema__)
        list(
          n_try_rows = .N,
          n_valid_coordinate_rows = sum(coordinate_valid_prema__),
          n_missing_or_invalid_coordinate_rows =
            sum(!coordinate_valid_prema__),
          n_in_range_coordinate_rows = sum(coordinate_in_range_prema__),
          eligible_species = any_inside || !any_valid,
          eligibility_reason = if (any_inside) {
            "IN_PFT_POINT_BUFFER"
          } else if (!any_valid) {
            "NO_VALID_COORDINATES_INCLUDED"
          } else {
            "KNOWN_COORDINATES_OUTSIDE_PFT_RANGE"
          }
        )
      },
      by = .(species_id = species_key_prema__)
    ]
    species_range_audit[, `:=`(
      pft = pft_name,
      pft_reference_point_count = nrow(reference_points),
      pft_range_buffer_degrees = buffer_degrees
    )]
    data.table::setcolorder(
      species_range_audit,
      c(
        "pft", "species_id", "eligible_species", "eligibility_reason",
        "n_try_rows", "n_valid_coordinate_rows",
        "n_missing_or_invalid_coordinate_rows",
        "n_in_range_coordinate_rows", "pft_reference_point_count",
        "pft_range_buffer_degrees"
      )
    )

    candidates[
      species_range_audit,
      on = .(species_key_prema__ = species_id),
      `:=`(
        pft_range_eligible = i.eligible_species,
        pft_species_eligibility_reason = i.eligibility_reason
      )
    ]
    selected <- data.table::copy(candidates[pft_range_eligible == TRUE])
    unassigned <- data.table::copy(candidates[pft_range_eligible == FALSE])
    if (nrow(selected) == 0L) {
      stop(
        "No target-PFT candidate species passed the +/-",
        format(buffer_degrees, trim = TRUE),
        " degree range rule for `", pft_name, "`.",
        call. = FALSE
      )
    }

    selected[, `:=`(
      assigned_final_pft = pft_name,
      pft_assignment_method = data.table::fifelse(
        pft_species_eligibility_reason == "IN_PFT_POINT_BUFFER",
        "pft_range_species_in_range",
        "pft_range_species_no_valid_coordinates"
      ),
      pft_assignment_distance_km = NA_real_,
      pft_assignment_status = "ASSIGNED",
      final_pft = pft_name
    )]
    selected[, pft_assignment_method_prema := pft_assignment_method]
    unassigned[, `:=`(
      assigned_final_pft = NA_character_,
      pft_assignment_method = "species_outside_pft_degree_buffer",
      pft_assignment_distance_km = NA_real_,
      pft_assignment_status = "UNASSIGNED"
    )]

    assignment_records <- data.table::rbindlist(
      list(selected, unassigned),
      use.names = TRUE,
      fill = TRUE
    )
    assignment_audit <- assignment_records[
      ,
      .(
        n_observations = .N,
        n_species = data.table::uniqueN(
          trimws(as.character(AccSpeciesID))
        )
      ),
      by = .(
        pft_assignment_status,
        pft_assignment_method,
        assigned_final_pft
      )
    ]
    coordinate_join_audit[, coordinate_usage := "PFT_RANGE_SPECIES"]
    audit <- data.table::data.table(
      pft = pft_name,
      assignment_mode = assignment_mode,
      input_try_rows = input_try_rows,
      target_candidate_species = length(target_species),
      target_candidate_rows = nrow(candidates),
      target_reference_points = nrow(reference_points),
      pft_range_buffer_degrees = buffer_degrees,
      selected_rows = nrow(selected),
      selected_species = species_range_audit[
        eligible_species == TRUE, .N
      ],
      selected_in_range_species = species_range_audit[
        eligibility_reason == "IN_PFT_POINT_BUFFER", .N
      ],
      selected_no_coordinate_species = species_range_audit[
        eligibility_reason == "NO_VALID_COORDINATES_INCLUDED", .N
      ],
      excluded_outside_range_species = species_range_audit[
        eligibility_reason == "KNOWN_COORDINATES_OUTSIDE_PFT_RANGE", .N
      ],
      selected_unique_species_rows = selected[
        shared_across_pfts == FALSE, .N
      ],
      selected_shared_species_rows = selected[
        shared_across_pfts == TRUE, .N
      ],
      selected_spatial_rows = as.integer(
        species_range_audit[
          eligible_species == TRUE,
          sum(n_in_range_coordinate_rows)
        ]
      ),
      selected_rows_from_in_range_species = selected[
        pft_species_eligibility_reason == "IN_PFT_POINT_BUFFER", .N
      ],
      excluded_ambiguous_rows = nrow(unassigned),
      excluded_unassigned_candidate_rows = nrow(unassigned),
      max_distance_km = NA_real_
    )

    helper_columns <- c(
      "species_key_prema__", "coordinate_valid_prema__",
      "coordinate_in_range_prema__", "pft_range_eligible"
    )
    selected[, (helper_columns) := NULL]
    unassigned[, (helper_columns) := NULL]
    attr(selected, "pft_selection_audit") <- audit
    attr(selected, "excluded_ambiguous_rows") <- unassigned
    attr(selected, "observation_pft_assignment_audit") <-
      assignment_audit
    attr(selected, "observation_pft_assignment_configuration") <- list(
      assignment_mode = assignment_mode,
      coordinates_used = TRUE,
      pft_range_buffer_degrees = buffer_degrees,
      range_definition = "union_of_latitude_longitude_boxes",
      missing_coordinate_species_policy = "include_only_if_species_has_no_valid_coordinates"
    )
    attr(selected, "observation_coordinate_join_audit") <-
      coordinate_join_audit
    attr(selected, "pft_species_range_audit") <- species_range_audit[]
    attr(selected, "pft_reference_points_used") <- reference_points[]
    return(selected[])
  }

  candidates[, species_key_prema__ := NULL]

  assigned <- assign_try_observations_to_pft(
    try_data = candidates,
    pft_species_map = pft_dt,
    pft_coordinate_map = pft_coordinate_map,
    species_col = "AccSpeciesID",
    observation_lat_col = observation_lat_col,
    observation_lon_col = observation_lon_col,
    max_distance_km = max_distance_km,
    unassigned_action = "drop"
  )
  assignment_audit <- attr(assigned, "observation_pft_assignment_audit")
  unassigned <- attr(assigned, "observation_pft_unassigned")
  assignment_configuration <- attr(
    assigned,
    "observation_pft_assignment_configuration"
  )
  if (nrow(unassigned) > 0L) {
    warning(
      nrow(unassigned),
      " candidate TRY observations could not be assigned to one PFT and ",
      "were excluded. Inspect excluded_ambiguous_rows.csv.",
      call. = FALSE
    )
  }

  selected <- data.table::copy(
    assigned[
      pft_assignment_status == "ASSIGNED" &
        assigned_final_pft == pft_name
    ]
  )

  if (nrow(selected) == 0L) {
    stop(
      "No TRY observations were spatially assigned to PFT `", pft_name,
      "`. Inspect the assignment audit and coordinate inputs.",
      call. = FALSE
    )
  }

  selected[, final_pft := pft_name]
  selected[, pft_assignment_method_prema := pft_assignment_method]

  audit <- data.table::data.table(
    pft = pft_name,
    assignment_mode = assignment_mode,
    input_try_rows = input_try_rows,
    target_candidate_species = length(target_species),
    target_candidate_rows = nrow(candidates),
    selected_rows = nrow(selected),
    selected_unique_species_rows = selected[
      pft_assignment_method == "unique_species_pft", .N
    ],
    selected_shared_species_rows = 0L,
    selected_spatial_rows = selected[
      pft_assignment_method == "nearest_candidate_pft_coordinate", .N
    ],
    excluded_ambiguous_rows = nrow(unassigned),
    excluded_unassigned_candidate_rows = nrow(unassigned),
    max_distance_km = as.numeric(max_distance_km)
  )

  attr(selected, "pft_selection_audit") <- audit
  attr(selected, "excluded_ambiguous_rows") <- unassigned
  attr(selected, "observation_pft_assignment_audit") <- assignment_audit
  attr(selected, "observation_pft_assignment_configuration") <-
    assignment_configuration
  attr(selected, "observation_coordinate_join_audit") <-
    coordinate_join_audit
  selected[]
}


.prema_source_metadata <- function(source_dt) {
  n <- nrow(source_dt)
  value_or <- function(name, default = NA) {
    .prema_value_column(source_dt, name, default)
  }
  observation_id <- .prema_nonempty_text(value_or("ObservationID"))
  obs_data_id <- .prema_nonempty_text(value_or("ObsDataID"))
  source_row <- .prema_nonempty_text(value_or("unit_conversion_row_id__"))
  source_row[is.na(source_row)] <- as.character(seq_len(n))[is.na(source_row)]
  source_observation_id <- observation_id
  source_observation_id[is.na(source_observation_id)] <- obs_data_id[
    is.na(source_observation_id)
  ]
  source_observation_id[is.na(source_observation_id)] <- paste0(
    "row_", source_row[is.na(source_observation_id)]
  )

  data.table::data.table(
    source_observation_id = source_observation_id,
    observation_key = observation_id,
    species_key = .prema_nonempty_text(value_or("AccSpeciesID")),
    species_name = .prema_nonempty_text(value_or("SpeciesName")),
    dataset_key = .prema_nonempty_text(value_or("DatasetID")),
    reference_key = .prema_nonempty_text(value_or("Reference")),
    latitude = suppressWarnings(as.numeric(value_or("Latitude"))),
    longitude = suppressWarnings(as.numeric(value_or("Longitude"))),
    replicates = suppressWarnings(as.numeric(value_or("Replicates"))),
    date = value_or("Date"),
    time = value_or("Time")
  )
}


.prema_align_secondary_source <- function(base_meta, source_dt) {
  pool_meta <- .prema_source_metadata(source_dt)
  pool_value <- suppressWarnings(as.numeric(source_dt$StdValue))
  n <- nrow(base_meta)
  chosen <- rep(NA_integer_, n)
  mode <- rep("UNPAIRED_PFT_SAMPLE", n)

  valid_obs_base <- !is.na(base_meta$observation_key)
  valid_obs_pool <- !is.na(pool_meta$observation_key)
  if (any(valid_obs_base) && any(valid_obs_pool)) {
    idx <- match(
      base_meta$observation_key,
      pool_meta$observation_key
    )
    use <- is.na(chosen) & !is.na(idx)
    chosen[use] <- idx[use]
    mode[use] <- "SAME_OBSERVATION_ID"
  }

  base_study_species <- paste(
    base_meta$dataset_key,
    base_meta$species_key,
    sep = "\u001f"
  )
  pool_study_species <- paste(
    pool_meta$dataset_key,
    pool_meta$species_key,
    sep = "\u001f"
  )
  valid_base_key <- !is.na(base_meta$dataset_key) & !is.na(base_meta$species_key)
  valid_pool_key <- !is.na(pool_meta$dataset_key) & !is.na(pool_meta$species_key)
  if (any(valid_base_key) && any(valid_pool_key)) {
    idx <- match(base_study_species, pool_study_species)
    use <- is.na(chosen) & valid_base_key & !is.na(idx)
    chosen[use] <- idx[use]
    mode[use] <- "SAME_DATASET_AND_SPECIES"
  }

  if (any(!is.na(base_meta$species_key)) && any(!is.na(pool_meta$species_key))) {
    idx <- match(base_meta$species_key, pool_meta$species_key)
    use <- is.na(chosen) & !is.na(base_meta$species_key) & !is.na(idx)
    chosen[use] <- idx[use]
    mode[use] <- "SAME_SPECIES"
  }

  remaining <- which(is.na(chosen))
  if (length(remaining) > 0L) {
    finite_pool <- which(is.finite(pool_value))
    if (length(finite_pool) > 0L) {
      chosen[remaining] <- sample(
        finite_pool,
        size = length(remaining),
        replace = length(finite_pool) < length(remaining)
      )
    }
  }

  value <- rep(NA_real_, n)
  use <- !is.na(chosen)
  value[use] <- pool_value[chosen[use]]
  list(value = value, pairing_mode = mode)
}


.prema_make_site_key <- function(meta, digits = 5L) {
  valid_coordinate <-
    is.finite(meta$latitude) & meta$latitude >= -90 & meta$latitude <= 90 &
    is.finite(meta$longitude) & meta$longitude >= -180 & meta$longitude <= 180

  key <- rep(NA_character_, nrow(meta))
  key[valid_coordinate] <- paste0(
    "coord:",
    format(round(meta$latitude[valid_coordinate], digits), nsmall = digits),
    ":",
    format(round(meta$longitude[valid_coordinate], digits), nsmall = digits)
  )
  use_dataset <- is.na(key) & !is.na(meta$dataset_key)
  key[use_dataset] <- paste0("dataset:", meta$dataset_key[use_dataset])
  use_reference <- is.na(key) & !is.na(meta$reference_key)
  key[use_reference] <- paste0("reference:", meta$reference_key[use_reference])
  key[is.na(key)] <- paste0(
    "unlocated:", meta$source_observation_id[is.na(key)]
  )
  key
}


.prema_target_domain_valid <- function(trait, value) {
  value <- suppressWarnings(as.numeric(value))
  finite <- is.finite(value)
  if (trait == "leafC") return(finite & value >= 0 & value <= 100)
  if (trait %in% c("growth_resp_factor", "waterRemoveFrac")) {
    return(finite & value >= 0 & value <= 1)
  }
  if (trait %in% c(
    "leaf_respiration_rate_m2", "dVPDSlope", "leaf_turnover_rate",
    "root_turnover_rate", "wood_turnover_rate", "turn_over_time",
    "root_respiration_rate", "stem_respiration_rate"
  )) {
    return(finite & value >= 0)
  }
  finite & value > 0
}


.prema_classify_conversion <- function(conversion_classes, pairing_modes) {
  if (any(conversion_classes == "DIRECT_WRITER_TRAIT")) {
    return("DIRECT_OBSERVED")
  }
  if (any(conversion_classes == "USER_SUPPLIED_MODEL")) {
    return("PROXY_MODEL")
  }
  if (any(grepl("PROXY|MODEL", conversion_classes))) {
    return("PROXY_MODEL")
  }
  if (any(pairing_modes == "UNPAIRED_PFT_SAMPLE")) {
    return("UNPAIRED_SYNTHETIC")
  }
  "DERIVED_ALGEBRAIC"
}


.prema_combine_pairing_modes <- function(current, candidate) {
  levels <- c(
    "DIRECT_OBSERVATION" = 0L,
    "PRIMARY_SOURCE" = 0L,
    "SAME_OBSERVATION_ID" = 1L,
    "SAME_DATASET_AND_SPECIES" = 2L,
    "SAME_SPECIES" = 3L,
    "UNPAIRED_PFT_SAMPLE" = 4L
  )
  current_rank <- unname(levels[current])
  candidate_rank <- unname(levels[candidate])
  current_rank[is.na(current_rank)] <- 0L
  candidate_rank[is.na(candidate_rank)] <- 0L
  use_candidate <- candidate_rank > current_rank
  current[use_candidate] <- candidate[use_candidate]
  current
}


.prema_split_source_traits <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0L) return(character())
  unique(trimws(unlist(strsplit(x, "|", fixed = TRUE))))
}


.prema_observation_dedup_key <- function(x) {
  id <- .prema_nonempty_text(x$source_observation_id)
  dataset <- .prema_nonempty_text(x$dataset_key)
  species <- .prema_nonempty_text(x$species_key)
  dataset[is.na(dataset)] <- "unknown_dataset"
  species[is.na(species)] <- "unknown_species"
  id[is.na(id)] <- paste0(
    "generated_", x$anchor_source[is.na(id)], "_", which(is.na(id))
  )
  paste(dataset, species, id, sep = "\u001f")
}


.prema_run_two_pass_bridge <- function(draws, context, models, target) {
  model_dependencies <- unique(c("SLA", "Amax", target))
  models_use <- models[
    intersect(model_dependencies, names(models))
  ]
  result_1 <- bridge_md_posteriors_to_pecan(
    ma_draws = draws,
    context = context,
    models = models_use,
    registry = md_to_pecan_bridge_registry(),
    strict = FALSE
  )

  draws_2 <- data.table::copy(draws)
  produced <- setdiff(names(result_1$candidate_trait_draws), "draw_id")
  for (name_i in produced) {
    if (!name_i %in% names(draws_2)) {
      data.table::set(
        draws_2,
        j = name_i,
        value = result_1$candidate_trait_draws[[name_i]]
      )
    }
  }

  result_2 <- bridge_md_posteriors_to_pecan(
    ma_draws = draws_2,
    context = context,
    models = models_use,
    registry = md_to_pecan_bridge_registry(),
    strict = FALSE
  )

  attempts_1 <- data.table::copy(result_1$rule_attempts)
  attempts_1[, bridge_pass := 1L]
  attempts_2 <- data.table::copy(result_2$rule_attempts)
  attempts_2[, bridge_pass := 2L]

  value_1 <- if (target %in% names(result_1$candidate_trait_draws)) {
    result_1$candidate_trait_draws[[target]]
  } else {
    rep(NA_real_, nrow(draws))
  }
  value_2 <- if (target %in% names(result_2$candidate_trait_draws)) {
    result_2$candidate_trait_draws[[target]]
  } else {
    rep(NA_real_, nrow(draws))
  }
  value <- suppressWarnings(as.numeric(value_1))
  use_2 <- !is.finite(value) & is.finite(value_2)
  value[use_2] <- suppressWarnings(as.numeric(value_2[use_2]))

  # Pass 2 contains pass-1 targets as input columns. Its automatic direct-trait
  # audit entry is therefore an implementation detail, not evidence that the
  # original TRY observation was a direct writer trait. Keep only the attempts
  # that actually generated the chosen pass-1/pass-2 values.
  winning_attempts <- list()
  if (any(is.finite(value_1))) {
    winning_attempts[[length(winning_attempts) + 1L]] <- attempts_1[
      target_pecan_trait == target & n_filled > 0L
    ]
  }
  if (any(use_2)) {
    winning_attempts[[length(winning_attempts) + 1L]] <- attempts_2[
      target_pecan_trait == target &
        n_filled > 0L &
        !(
          conversion_class == "DIRECT_WRITER_TRAIT" &
          source_traits == target
        )
    ]
  }
  winning_attempts <- if (length(winning_attempts) > 0L) {
    data.table::rbindlist(
      winning_attempts,
      use.names = TRUE,
      fill = TRUE
    )
  } else {
    data.table::data.table()
  }

  list(
    value = value,
    successful_attempts = winning_attempts,
    attempts = data.table::rbindlist(
      list(attempts_1, attempts_2),
      use.names = TRUE,
      fill = TRUE
    )
  )
}


build_prema_pecan_target_observations <- function(
    canonical_try,
    pft_name,
    seed = 20260903L,
    context_overrides = list(),
    unpaired_sdlog = 0.15,
    site_coordinate_digits = 5L
) {
  .prema_require_packages("data.table")
  .prema_require_functions(c(
    "all_enabled_md_bridge_context",
    "all_enabled_md_bridge_models",
    "bridge_md_posteriors_to_pecan",
    "md_to_pecan_bridge_registry",
    "sipnet_writer_unit_contract_v2"
  ))

  x <- data.table::copy(data.table::as.data.table(canonical_try))
  required <- c("pecan_vname", "StdValue")
  missing <- setdiff(required, names(x))
  if (length(missing) > 0L) {
    stop(
      "canonical_try is missing: ", paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  x[, pecan_vname := trimws(as.character(pecan_vname))]
  x[, StdValue := suppressWarnings(as.numeric(StdValue))]
  x <- x[!is.na(pecan_vname) & nzchar(pecan_vname) & is.finite(StdValue)]
  if (nrow(x) == 0L) stop("canonical_try has no finite values.", call. = FALSE)

  targets <- prema_pecan_targets_21()
  registry <- .prema_expand_source_registry(
    prema_target_source_registry_21()
  )
  contract <- data.table::as.data.table(sipnet_writer_unit_contract_v2())[
    pecan_trait %in% targets
  ]
  contract <- contract[match(targets, pecan_trait)]
  if (nrow(contract) != length(targets) || anyNA(contract$pecan_trait)) {
    stop("The SIPNET writer contract does not contain all 21 targets.", call. = FALSE)
  }

  context <- all_enabled_md_bridge_context(
    pftname = pft_name,
    context_overrides = utils::modifyList(
      list(allow_root_depth_soilwhc_approx = FALSE),
      context_overrides,
      keep.null = TRUE
    )
  )
  models <- all_enabled_md_bridge_models()
  set.seed(as.integer(seed))

  observation_results <- vector("list", length(targets))
  target_audit <- vector("list", length(targets))
  rule_audit <- vector("list", length(targets))

  for (target_index in seq_along(targets)) {
    target <- targets[[target_index]]
    target_unit <- contract[pecan_trait == target, pecan_input_unit][[1L]]
    target_candidates <- list()
    target_rules <- list()
    direct <- data.table::copy(x[pecan_vname == target])

    if (nrow(direct) > 0L) {
      meta <- .prema_source_metadata(direct)
      value <- direct$StdValue
      valid <- .prema_target_domain_valid(target, value)
      out <- data.table::data.table(
        pecan_trait = target,
        target_value = value,
        target_unit = target_unit,
        source_class = "DIRECT_OBSERVED",
        source_traits = target,
        conversion_formula = "identity after canonical unit conversion",
        pairing_mode = "DIRECT_OBSERVATION",
        anchor_source = target,
        route_priority = 0L,
        uses_default_context = FALSE,
        proxy_sdlog = 0,
        source_observation_id = meta$source_observation_id,
        species_key = meta$species_key,
        species_name = meta$species_name,
        dataset_key = meta$dataset_key,
        reference_key = meta$reference_key,
        latitude = meta$latitude,
        longitude = meta$longitude,
        replicates = meta$replicates,
        date = meta$date,
        time = meta$time,
        domain_valid = valid
      )
      target_candidates[[length(target_candidates) + 1L]] <- out
      target_rules[[length(target_rules) + 1L]] <- data.table::data.table(
        target_pecan_trait = target,
        source_traits = target,
        conversion_class = "DIRECT_WRITER_TRAIT",
        formula = "identity after canonical unit conversion",
        n_candidate = nrow(out),
        n_filled = sum(valid),
        status = if (any(valid)) "FILLED" else "NOT_FILLED",
        note = paste(
          "Direct observations have priority only when another route has the",
          "same source observation; independent lower-priority routes are retained."
        ),
        bridge_pass = 0L,
        anchor_source = target,
        route_priority = 0L,
        anchor_used_by_route = TRUE
      )
    }

    sources <- registry[[target]]
    available <- sources[sources %in% unique(x$pecan_vname)]
    if (length(available) > 0L) {
      # Every available source gets a chance to anchor independent candidate
      # observations. The direct target column is deliberately not aligned
      # here: direct values should only override a derived value when both
      # belong to the same source observation during the final deduplication.
      for (anchor_index in seq_along(available)) {
        anchor_source <- available[[anchor_index]]
        anchor <- data.table::copy(x[pecan_vname == anchor_source])
        base_meta <- .prema_source_metadata(anchor)
        bridge_draws <- data.table::data.table(
          draw_id = seq_len(nrow(anchor))
        )
        data.table::set(
          bridge_draws,
          j = anchor_source,
          value = anchor$StdValue
        )
        pairing_by_source <- list()
        pairing_by_source[[anchor_source]] <- rep(
          "PRIMARY_SOURCE",
          nrow(anchor)
        )

        for (source_i in setdiff(available, anchor_source)) {
          aligned <- .prema_align_secondary_source(
            base_meta,
            x[pecan_vname == source_i]
          )
          data.table::set(
            bridge_draws,
            j = source_i,
            value = aligned$value
          )
          pairing_by_source[[source_i]] <- aligned$pairing_mode
        }

        bridge_run <- .prema_run_two_pass_bridge(
          draws = bridge_draws,
          context = context,
          models = models,
          target = target
        )
        successful_attempts <- bridge_run$successful_attempts
        used_sources <- .prema_split_source_traits(
          successful_attempts$source_traits
        )
        anchor_used <- anchor_source %in% used_sources

        route_rules <- data.table::copy(
          bridge_run$attempts[target_pecan_trait == target]
        )
        if (nrow(route_rules) > 0L) {
          route_rules[, `:=`(
            anchor_source = anchor_source,
            route_priority = as.integer(anchor_index),
            anchor_used_by_route = anchor_used
          )]
          target_rules[[length(target_rules) + 1L]] <- route_rules
        }

        # Do not count a row under an anchor that the successful conversion
        # did not actually use. This prevents unrelated aligned sources from
        # multiplying identical pseudo-observations.
        if (!anchor_used || nrow(successful_attempts) == 0L) next

        pairing_mode <- rep("PRIMARY_SOURCE", nrow(anchor))
        for (source_i in intersect(used_sources, names(pairing_by_source))) {
          pairing_mode <- .prema_combine_pairing_modes(
            pairing_mode,
            pairing_by_source[[source_i]]
          )
        }

        value <- bridge_run$value
        conversion_classes <- unique(successful_attempts$conversion_class)
        conversion_class <- .prema_classify_conversion(
          conversion_classes,
          pairing_mode
        )

        # Keep the existing permissive fallback requested by the user, while
        # preserving its pairing label and adding uncertainty to algebraic
        # PFT-wide combinations.
        add_unpaired_noise <-
          any(pairing_mode == "UNPAIRED_PFT_SAMPLE") &
          !any(conversion_classes == "USER_SUPPLIED_MODEL") &
          is.finite(unpaired_sdlog) & unpaired_sdlog > 0
        if (add_unpaired_noise) {
          positive <- is.finite(value) & value > 0
          value[positive] <- value[positive] * exp(stats::rnorm(
            sum(positive),
            mean = -0.5 * unpaired_sdlog^2,
            sd = unpaired_sdlog
          ))
        }

        valid <- .prema_target_domain_valid(target, value)
        source_text <- paste(
          unique(successful_attempts$source_traits),
          collapse = "|"
        )
        formula_text <- paste(
          unique(successful_attempts$formula),
          collapse = " | "
        )
        out <- data.table::data.table(
          pecan_trait = target,
          target_value = value,
          target_unit = target_unit,
          source_class = conversion_class,
          source_traits = source_text,
          conversion_formula = formula_text,
          pairing_mode = pairing_mode,
          anchor_source = anchor_source,
          route_priority = as.integer(anchor_index),
          uses_default_context = TRUE,
          proxy_sdlog = if (add_unpaired_noise) {
            unpaired_sdlog
          } else {
            NA_real_
          },
          source_observation_id = base_meta$source_observation_id,
          species_key = base_meta$species_key,
          species_name = base_meta$species_name,
          dataset_key = base_meta$dataset_key,
          reference_key = base_meta$reference_key,
          latitude = base_meta$latitude,
          longitude = base_meta$longitude,
          replicates = ifelse(
            pairing_mode %in% c("PRIMARY_SOURCE", "SAME_OBSERVATION_ID"),
            base_meta$replicates,
            1
          ),
          date = base_meta$date,
          time = base_meta$time,
          domain_valid = valid
        )
        target_candidates[[length(target_candidates) + 1L]] <- out
      }
    }

    candidate_table <- if (length(target_candidates) > 0L) {
      data.table::rbindlist(
        target_candidates,
        use.names = TRUE,
        fill = TRUE
      )
    } else {
      data.table::data.table()
    }
    if (nrow(candidate_table) > 0L) {
      n_candidate <- nrow(candidate_table)
      n_invalid_domain <- sum(!candidate_table$domain_valid)
      candidate_table <- candidate_table[domain_valid == TRUE]
      n_valid_before_dedup <- nrow(candidate_table)
      candidate_table[, observation_dedup_key__ :=
        .prema_observation_dedup_key(.SD)]
      data.table::setorder(
        candidate_table,
        route_priority,
        anchor_source,
        source_observation_id
      )
      candidate_table <- candidate_table[
        !duplicated(observation_dedup_key__)
      ]
      candidate_table[, observation_dedup_key__ := NULL]
    } else {
      n_candidate <- 0L
      n_invalid_domain <- 0L
      n_valid_before_dedup <- 0L
    }

    n_valid <- nrow(candidate_table)
    has_direct <- n_valid > 0L && any(
      candidate_table$source_class == "DIRECT_OBSERVED"
    )
    has_derived <- n_valid > 0L && any(
      candidate_table$source_class != "DIRECT_OBSERVED"
    )
    target_status <- if (has_direct && has_derived) {
      "READY_FOR_MA_DIRECT_AND_DERIVED"
    } else if (has_direct) {
      "DIRECT_OBSERVED"
    } else if (has_derived) {
      "READY_FOR_MA_MULTI_ROUTE"
    } else if (nrow(direct) == 0L && length(available) == 0L) {
      "DEFAULT_ONLY_NO_TRY_SOURCE"
    } else {
      "DEFAULT_ONLY_NO_VALID_CONVERSION"
    }

    observation_results[[target_index]] <- candidate_table
    target_audit[[target_index]] <- data.table::data.table(
      pecan_trait = target,
      target_unit = target_unit,
      target_status = target_status,
      primary_source = if (n_valid > 0L) {
        sources_used <- unique(candidate_table$anchor_source)
        if (length(sources_used) == 1L) sources_used[[1L]] else "MULTI_ROUTE"
      } else {
        ""
      },
      available_sources = paste(
        unique(c(if (nrow(direct) > 0L) target else character(), available)),
        collapse = "|"
      ),
      route_sources_used = if (n_valid > 0L) {
        paste(unique(candidate_table$anchor_source), collapse = "|")
      } else {
        ""
      },
      n_candidate = n_candidate,
      n_valid_before_dedup = n_valid_before_dedup,
      n_deduplicated_same_observation = n_valid_before_dedup - n_valid,
      n_valid = n_valid,
      n_invalid_domain = n_invalid_domain,
      source_class = if (n_valid > 0L) {
        paste(sort(unique(candidate_table$source_class)), collapse = "|")
      } else {
        "DEFAULT_ONLY"
      },
      pairing_modes = if (n_valid > 0L) {
        paste(sort(unique(candidate_table$pairing_mode)), collapse = "|")
      } else {
        ""
      }
    )
    rule_audit[[target_index]] <- if (length(target_rules) > 0L) {
      data.table::rbindlist(
        target_rules,
        use.names = TRUE,
        fill = TRUE
      )
    } else {
      data.table::data.table()
    }
  }

  observations <- data.table::rbindlist(
    observation_results,
    use.names = TRUE,
    fill = TRUE
  )
  if (nrow(observations) == 0L) {
    stop("No PEcAn-target observations were generated.", call. = FALSE)
  }
  observations[, site_key := .prema_make_site_key(
    .SD,
    digits = as.integer(site_coordinate_digits)
  )]
  # IDs must be contiguous within each independently fitted trait. Otherwise
  # an integer gap can create empty site levels in the JAGS data.
  observations[
    ,
    site_id := as.integer(factor(site_key)),
    by = pecan_trait
  ]
  observations[, target_observation_id := seq_len(.N)]
  observations[, pft := pft_name]

  list(
    observations = observations[],
    target_audit = data.table::rbindlist(
      target_audit,
      use.names = TRUE,
      fill = TRUE
    ),
    rule_audit = data.table::rbindlist(
      rule_audit,
      use.names = TRUE,
      fill = TRUE
    ),
    context = context,
    writer_contract = contract[]
  )
}


# Select species for each target-specific empirical prior before MA.
#
# For 3-19 usable species, one species is sampled. For 20 or more, ceiling of
# prior_species_fraction times n_species are sampled. For one or two species,
# no random sample is taken and all available species medians are used.
# All species remain in the likelihood, preserving the explicitly accepted
# empirical-Bayes overlap while making it completely visible in the audit.
split_prema_prior_species <- function(
    target_observations,
    prior_species_fraction = 0.10,
    fraction_rule_min_species = 20L,
    no_holdout_max_species = 2L,
    seed = 20260904L
) {
  .prema_require_packages("data.table")
  x <- data.table::copy(data.table::as.data.table(target_observations))
  required <- c(
    "pecan_trait", "species_key", "target_value", "site_key"
  )
  missing <- setdiff(required, names(x))
  if (length(missing) > 0L) {
    stop(
      "target_observations is missing prior-split columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (
    length(prior_species_fraction) != 1L ||
    !is.finite(prior_species_fraction) ||
    prior_species_fraction <= 0 ||
    prior_species_fraction > 1
  ) {
    stop("prior_species_fraction must be in (0, 1].", call. = FALSE)
  }
  fraction_rule_min_species <- as.integer(fraction_rule_min_species)
  no_holdout_max_species <- as.integer(no_holdout_max_species)
  if (
    is.na(fraction_rule_min_species) ||
    is.na(no_holdout_max_species) ||
    no_holdout_max_species < 1L ||
    fraction_rule_min_species <= no_holdout_max_species
  ) {
    stop(
      "fraction_rule_min_species must exceed no_holdout_max_species >= 1.",
      call. = FALSE
    )
  }
  if (length(seed) != 1L || !is.finite(seed)) {
    stop("seed must be one finite integer.", call. = FALSE)
  }

  x[, pecan_trait := trimws(as.character(pecan_trait))]
  x[, prior_species_key__ := .prema_nonempty_text(species_key)]
  x[, target_value := suppressWarnings(as.numeric(target_value))]
  if (any(!is.finite(x$target_value))) {
    stop("target_observations contains non-finite target_value.", call. = FALSE)
  }

  set.seed(as.integer(seed))
  traits <- sort(unique(x$pecan_trait))
  likelihood_rows <- vector("list", length(traits))
  prior_source_rows <- vector("list", length(traits))
  prior_value_rows <- vector("list", length(traits))
  assignment_rows <- vector("list", length(traits))
  audit_rows <- vector("list", length(traits))

  for (trait_index in seq_along(traits)) {
    trait_i <- traits[[trait_index]]
    one <- data.table::copy(x[pecan_trait == trait_i])
    usable_species <- sort(unique(
      one[!is.na(prior_species_key__), prior_species_key__]
    ))
    n_species <- length(usable_species)
    if (n_species == 0L) {
      stop(
        "Target ", trait_i,
        " has no usable species identifier for prior construction.",
        call. = FALSE
      )
    }

    if (n_species <= no_holdout_max_species) {
      prior_species <- usable_species
      n_randomly_selected <- 0L
      selection_rule <- "ALL_SPECIES_PRIOR_NO_RANDOM_SAMPLE_LE2"
    } else {
      n_select <- if (n_species < fraction_rule_min_species) {
        1L
      } else {
        as.integer(ceiling(prior_species_fraction * n_species))
      }
      n_select <- min(n_species, max(1L, n_select))
      prior_species <- sort(sample(
        usable_species,
        size = n_select,
        replace = FALSE
      ))
      n_randomly_selected <- length(prior_species)
      selection_rule <- if (n_species < fraction_rule_min_species) {
        "SAMPLE_ONE_SPECIES_LT20"
      } else {
        "SAMPLE_CEILING_10_PERCENT_SPECIES"
      }
    }

    # The user explicitly chose to keep the current empirical-Bayes overlap:
    # selected prior species are NOT deleted from the MA likelihood.
    likelihood_one <- data.table::copy(one)
    prior_source_one <- data.table::copy(
      one[prior_species_key__ %in% prior_species]
    )
    if (nrow(prior_source_one) == 0L) {
      stop(
        "Target ", trait_i,
        " has no rows for the selected prior species.",
        call. = FALSE
      )
    }

    prior_values_one <- prior_source_one[
      ,
      .(
        prior_value = stats::median(target_value, na.rm = TRUE),
        n_source_rows = .N
      ),
      by = .(
        pecan_trait,
        species_key = prior_species_key__
      )
    ]
    species_assignments_one <- one[
      !is.na(prior_species_key__),
      .(n_target_rows = .N),
      by = .(
        pecan_trait,
        species_key = prior_species_key__
      )
    ]
    species_assignments_one[
      ,
      species_role := data.table::fifelse(
        species_key %in% prior_species,
        "PRIOR_AND_LIKELIHOOD_SELECTED",
        "LIKELIHOOD_NOT_SELECTED_FOR_PRIOR"
      )
    ]
    species_assignments_one[, selection_rule := selection_rule]

    likelihood_rows[[trait_index]] <- likelihood_one
    prior_source_rows[[trait_index]] <- prior_source_one
    prior_value_rows[[trait_index]] <- prior_values_one
    assignment_rows[[trait_index]] <- species_assignments_one
    audit_rows[[trait_index]] <- data.table::data.table(
      pecan_trait = trait_i,
      n_available_species = n_species,
      n_prior_species_used = length(prior_species),
      n_prior_species_selected_randomly = n_randomly_selected,
      n_prior_species_removed_from_likelihood = 0L,
      # Compatibility column for older result readers.
      n_prior_species_held_out = 0L,
      n_likelihood_species = data.table::uniqueN(
        likelihood_one$prior_species_key__,
        na.rm = TRUE
      ),
      n_all_target_rows = nrow(one),
      n_prior_source_rows = nrow(prior_source_one),
      n_likelihood_rows = nrow(likelihood_one),
      n_rows_without_species_id = sum(is.na(one$prior_species_key__)),
      prior_species_fraction = as.numeric(prior_species_fraction),
      fraction_rule_min_species = fraction_rule_min_species,
      no_holdout_max_species = no_holdout_max_species,
      selection_rule = selection_rule,
      prior_likelihood_species_overlap = TRUE
    )
  }

  likelihood <- data.table::rbindlist(
    likelihood_rows,
    use.names = TRUE,
    fill = TRUE
  )
  prior_source <- data.table::rbindlist(
    prior_source_rows,
    use.names = TRUE,
    fill = TRUE
  )
  prior_values <- data.table::rbindlist(
    prior_value_rows,
    use.names = TRUE,
    fill = TRUE
  )
  assignments <- data.table::rbindlist(
    assignment_rows,
    use.names = TRUE,
    fill = TRUE
  )
  audit <- data.table::rbindlist(
    audit_rows,
    use.names = TRUE,
    fill = TRUE
  )

  # Reindex within each independently fitted target so JAGS never sees gaps.
  likelihood[
    ,
    site_id := as.integer(factor(site_key)),
    by = pecan_trait
  ]
  likelihood[, target_observation_id := seq_len(.N)]
  likelihood[, prior_species_key__ := NULL]
  prior_source[, prior_species_key__ := NULL]

  list(
    likelihood_observations = likelihood[],
    prior_source_observations = prior_source[],
    prior_species_values = prior_values[],
    species_assignments = assignments[],
    trait_audit = audit[],
    configuration = list(
      prior_species_fraction = as.numeric(prior_species_fraction),
      fraction_rule_min_species = fraction_rule_min_species,
      no_holdout_max_species = no_holdout_max_species,
      seed = as.integer(seed),
      species_summary = "median",
      selected_prior_species_removed_from_likelihood = FALSE,
      empirical_prior_likelihood_overlap = TRUE,
      sparse_trait_policy =
        "all_species_prior_and_likelihood_no_random_sample_when_le2",
      split_scope =
        "anchor_species_of_prebuilt_target_observations"
    )
  )
}


.prema_prior_cdf <- function(point, distn, parama, paramb) {
  point <- suppressWarnings(as.numeric(point))
  parama <- suppressWarnings(as.numeric(parama))
  paramb <- suppressWarnings(as.numeric(paramb))
  if (
    length(point) != 1L || length(parama) != 1L ||
    length(paramb) != 1L || !is.finite(point) ||
    !is.finite(parama) || !is.finite(paramb)
  ) {
    return(NA_real_)
  }

  switch(
    as.character(distn),
    norm = stats::pnorm(point, mean = parama, sd = paramb),
    lnorm = stats::plnorm(point, meanlog = parama, sdlog = paramb),
    beta = stats::pbeta(point, shape1 = parama, shape2 = paramb),
    unif = stats::punif(point, min = parama, max = paramb),
    stop(
      "Unsupported PEcAn prior distribution in preflight: ",
      as.character(distn),
      call. = FALSE
    )
  )
}


# Build a prior from one median per selected prior species, then reproduce
# PEcAn.MA's likelihood-median consistency check before starting JAGS.
#
# Prior parameters are anchored in the selected prior species. Width,
# concentration, or bounded support is relaxed only when needed so PEcAn.MA
# will not abort with "inconsistent with priors". Physical support is retained.
make_prema_species_prior_distns <- function(
    prior_species_values,
    likelihood_observations,
    likelihood_jagged_summary,
    targets,
    writer_contract,
    unit_map,
    seed = 20260905L,
    width_multiplier = 10,
    relative_sd_floor = 0.10,
    pecan_consistency_tail = 0.025
) {
  .prema_require_packages("data.table")
  .prema_require_functions("make_prior_distns_from_trait_data")
  values <- data.table::copy(
    data.table::as.data.table(prior_species_values)
  )
  likelihood <- data.table::copy(
    data.table::as.data.table(likelihood_observations)
  )

  required_prior <- c("pecan_trait", "species_key", "prior_value")
  missing_prior <- setdiff(required_prior, names(values))
  if (length(missing_prior) > 0L) {
    stop(
      "prior_species_values is missing: ",
      paste(missing_prior, collapse = ", "),
      call. = FALSE
    )
  }
  required_likelihood <- c("pecan_trait", "species_key", "target_value")
  missing_likelihood <- setdiff(required_likelihood, names(likelihood))
  if (length(missing_likelihood) > 0L) {
    stop(
      "likelihood_observations is missing: ",
      paste(missing_likelihood, collapse = ", "),
      call. = FALSE
    )
  }
  targets <- trimws(as.character(targets))
  if (
    length(targets) == 0L ||
    anyNA(targets) ||
    any(!nzchar(targets))
  ) {
    stop("targets must contain non-empty trait names.", call. = FALSE)
  }
  targets <- sort(unique(targets))

  jagged_summary <- data.table::copy(
    data.table::as.data.table(likelihood_jagged_summary)
  )
  required_jagged <- c(
    "pecan_trait", "n_jagged_rows",
    "jagged_min", "jagged_median", "jagged_max"
  )
  missing_jagged <- setdiff(required_jagged, names(jagged_summary))
  if (length(missing_jagged) > 0L) {
    stop(
      "likelihood_jagged_summary is missing: ",
      paste(missing_jagged, collapse = ", "),
      call. = FALSE
    )
  }
  jagged_summary[, pecan_trait := trimws(as.character(pecan_trait))]
  if (
    anyNA(jagged_summary$pecan_trait) ||
    any(!nzchar(jagged_summary$pecan_trait)) ||
    anyDuplicated(jagged_summary$pecan_trait)
  ) {
    stop(
      "likelihood_jagged_summary must have one uniquely named row per target.",
      call. = FALSE
    )
  }
  for (numeric_column in required_jagged[-1L]) {
    jagged_summary[
      ,
      (numeric_column) := suppressWarnings(
        as.numeric(get(numeric_column))
      )
    ]
  }
  missing_jagged_targets <- setdiff(
    targets,
    jagged_summary$pecan_trait
  )
  if (length(missing_jagged_targets) > 0L) {
    stop(
      "No exact PEcAn jagged likelihood summary for: ",
      paste(missing_jagged_targets, collapse = ", "),
      call. = FALSE
    )
  }
  target_jagged_summary <- jagged_summary[
    pecan_trait %in% targets
  ]
  jagged_numeric_matrix <- as.matrix(
    target_jagged_summary[
      ,
      required_jagged[-1L],
      with = FALSE
    ]
  )
  if (
    any(!is.finite(jagged_numeric_matrix)) ||
    any(target_jagged_summary$n_jagged_rows < 1)
  ) {
    stop(
      "Exact PEcAn jagged likelihood summaries must be finite and non-empty.",
      call. = FALSE
    )
  }
  if (
    length(pecan_consistency_tail) != 1L ||
    !is.finite(pecan_consistency_tail) ||
    pecan_consistency_tail < 0.025 ||
    pecan_consistency_tail >= 0.5
  ) {
    stop(
      "pecan_consistency_tail must be in [0.025, 0.5).",
      call. = FALSE
    )
  }

  values[, pecan_trait := trimws(as.character(pecan_trait))]
  values[, species_key := trimws(as.character(species_key))]
  values[, prior_value := suppressWarnings(as.numeric(prior_value))]
  likelihood[, pecan_trait := trimws(as.character(pecan_trait))]
  likelihood[, species_key := trimws(as.character(species_key))]
  likelihood[, target_value := suppressWarnings(as.numeric(target_value))]

  missing_prior_targets <- setdiff(targets, unique(values$pecan_trait))
  if (length(missing_prior_targets) > 0L) {
    stop(
      "No species-level prior values for: ",
      paste(missing_prior_targets, collapse = ", "),
      call. = FALSE
    )
  }
  missing_likelihood_targets <- setdiff(
    targets,
    unique(likelihood$pecan_trait)
  )
  if (length(missing_likelihood_targets) > 0L) {
    stop(
      "No likelihood observations for prior preflight: ",
      paste(missing_likelihood_targets, collapse = ", "),
      call. = FALSE
    )
  }

  prior_trait.data <- lapply(targets, function(trait_i) {
    data.frame(
      mean = values[
        pecan_trait == trait_i & is.finite(prior_value),
        as.numeric(prior_value)
      ],
      stringsAsFactors = FALSE
    )
  })
  names(prior_trait.data) <- targets
  empty_prior_targets <- targets[
    vapply(prior_trait.data, nrow, integer(1)) == 0L
  ]
  if (length(empty_prior_targets) > 0L) {
    stop(
      "No finite species-level prior values for: ",
      paste(empty_prior_targets, collapse = ", "),
      call. = FALSE
    )
  }

  max_prior_species <- max(
    vapply(prior_trait.data, nrow, integer(1))
  )
  prior_registry <- prema_target_prior_registry(
    targets,
    writer_contract
  )
  prior.distns <- make_prior_distns_from_trait_data(
    trait.data = prior_trait.data,
    sample_fraction = 1,
    min_sample_n = 1L,
    max_sample_n = as.integer(max_prior_species),
    width_multiplier = width_multiplier,
    relative_sd_floor = relative_sd_floor,
    seed = as.integer(seed),
    unit_map = unit_map,
    prior_registry = prior_registry,
    positive_distribution = "lnorm",
    domain_action = "stop"
  )

  base_parameter_audit <- data.table::copy(data.table::as.data.table(
    attr(prior.distns, "prior_parameter_audit")
  ))
  target_tail <- min(
    0.499,
    as.numeric(pecan_consistency_tail) + 1e-4
  )
  z_limit <- stats::qnorm(1 - target_tail)
  preflight_rows <- vector("list", length(targets))

  for (trait_index in seq_along(targets)) {
    trait_i <- targets[[trait_index]]
    prior_values_i <- values[
      pecan_trait == trait_i & is.finite(prior_value),
      prior_value
    ]
    likelihood_i <- likelihood[
      pecan_trait == trait_i & is.finite(target_value),
      target_value
    ]
    if (length(likelihood_i) == 0L) {
      stop(
        "No finite likelihood observations for ", trait_i, ".",
        call. = FALSE
      )
    }

    rule <- prior_registry[trait == trait_i][1L]
    jagged_i <- jagged_summary[pecan_trait == trait_i][1L]
    raw_likelihood_median <- stats::median(likelihood_i)
    likelihood_median <- as.numeric(jagged_i$jagged_median)
    likelihood_min <- min(likelihood_i)
    likelihood_max <- max(likelihood_i)
    prior_center <- stats::median(prior_values_i)
    prior_species_sd <- if (length(prior_values_i) >= 2L) {
      stats::sd(prior_values_i)
    } else {
      NA_real_
    }

    distn <- as.character(prior.distns[trait_i, "distn"])
    parama_before <- as.numeric(prior.distns[trait_i, "parama"])
    paramb_before <- as.numeric(prior.distns[trait_i, "paramb"])
    parama_after <- parama_before
    paramb_after <- paramb_before
    empirical_parama <- parama_before
    empirical_paramb <- paramb_before
    adjustment_method <- "NONE"
    beta_strength <- NA_real_

    if (identical(distn, "unif")) {
      hard_lower <- as.numeric(rule$lower)
      hard_upper <- as.numeric(rule$upper)
      if (
        !is.finite(hard_lower) || !is.finite(hard_upper) ||
        hard_upper <= hard_lower
      ) {
        stop(
          "Uniform prior needs finite physical bounds for ", trait_i, ".",
          call. = FALSE
        )
      }

      variability_candidates <- c(
        prior_species_sd,
        relative_sd_floor * max(abs(prior_center), 1)
      )
      variability_candidates <- variability_candidates[
        is.finite(variability_candidates) & variability_candidates > 0
      ]
      empirical_half_width <- width_multiplier * if (
        length(variability_candidates) > 0L
      ) {
        max(variability_candidates)
      } else {
        max((hard_upper - hard_lower) * 0.05, 1)
      }
      parama_after <- max(hard_lower, prior_center - empirical_half_width)
      paramb_after <- min(hard_upper, prior_center + empirical_half_width)
      if (
        !is.finite(parama_after) || !is.finite(paramb_after) ||
        paramb_after <= parama_after
      ) {
        stop(
          "Could not construct a non-degenerate bounded prior for ",
          trait_i,
          call. = FALSE
        )
      }
      empirical_parama <- parama_after
      empirical_paramb <- paramb_after
      adjustment_method <- "SPECIES_CENTERED_BOUNDED_UNIFORM"

      cdf_i <- .prema_prior_cdf(
        likelihood_median,
        distn,
        parama_after,
        paramb_after
      )
      if (!is.finite(cdf_i) || cdf_i < target_tail) {
        required_lower <- (
          likelihood_median - target_tail * paramb_after
        ) / (1 - target_tail)
        parama_after <- max(
          hard_lower,
          min(parama_after, required_lower)
        )
      }
      cdf_i <- .prema_prior_cdf(
        likelihood_median,
        distn,
        parama_after,
        paramb_after
      )
      if (!is.finite(cdf_i) || cdf_i > 1 - target_tail) {
        required_upper <- (
          likelihood_median - target_tail * parama_after
        ) / (1 - target_tail)
        paramb_after <- min(
          hard_upper,
          max(paramb_after, required_upper)
        )
      }
      if (
        !isTRUE(all.equal(parama_after, empirical_parama)) ||
        !isTRUE(all.equal(paramb_after, empirical_paramb))
      ) {
        adjustment_method <- paste0(
          adjustment_method,
          "+EXPANDED_FOR_LIKELIHOOD_MEDIAN"
        )
      }
    } else if (identical(distn, "beta")) {
      beta_values <- pmin(
        1 - 1e-6,
        pmax(1e-6, prior_values_i)
      )
      beta_mean <- mean(beta_values)
      beta_variance <- if (length(beta_values) >= 2L) {
        stats::var(beta_values)
      } else {
        NA_real_
      }
      moment_strength <- if (
        is.finite(beta_variance) && beta_variance > 0
      ) {
        beta_mean * (1 - beta_mean) / beta_variance - 1
      } else {
        NA_real_
      }
      beta_strength <- if (
        is.finite(moment_strength) && moment_strength > 0
      ) {
        min(50, max(0.10, moment_strength))
      } else {
        2
      }

      make_beta_parameters <- function(strength) {
        c(
          parama = max(1e-6, beta_mean * strength),
          paramb = max(1e-6, (1 - beta_mean) * strength)
        )
      }
      beta_parameters <- make_beta_parameters(beta_strength)
      parama_after <- beta_parameters[["parama"]]
      paramb_after <- beta_parameters[["paramb"]]
      empirical_parama <- parama_after
      empirical_paramb <- paramb_after
      adjustment_method <- "SPECIES_PARAMETERIZED_BETA"

      cdf_i <- .prema_prior_cdf(
        likelihood_median,
        distn,
        parama_after,
        paramb_after
      )
      while (
        is.finite(cdf_i) &&
        (cdf_i < target_tail || cdf_i > 1 - target_tail) &&
        beta_strength > 1e-4
      ) {
        beta_strength <- beta_strength / 2
        beta_parameters <- make_beta_parameters(beta_strength)
        parama_after <- beta_parameters[["parama"]]
        paramb_after <- beta_parameters[["paramb"]]
        cdf_i <- .prema_prior_cdf(
          likelihood_median,
          distn,
          parama_after,
          paramb_after
        )
      }

      if (
        !is.finite(cdf_i) ||
        cdf_i < target_tail ||
        cdf_i > 1 - target_tail
      ) {
        # A nearly non-informative symmetric beta is the bounded safety
        # fallback. A tiny species-centered component preserves deterministic
        # dependence on the selected prior-species values.
        safety_shapes <- c(1, 0.5, 0.2, 0.1, 0.05, 0.02, 0.01)
        resolved_shape <- FALSE
        for (shape_i in safety_shapes) {
          parama_candidate <- shape_i + beta_mean * 1e-6
          paramb_candidate <- shape_i + (1 - beta_mean) * 1e-6
          cdf_candidate <- .prema_prior_cdf(
            likelihood_median,
            distn,
            parama_candidate,
            paramb_candidate
          )
          if (
            is.finite(cdf_candidate) &&
            cdf_candidate >= target_tail &&
            cdf_candidate <= 1 - target_tail
          ) {
            parama_after <- parama_candidate
            paramb_after <- paramb_candidate
            cdf_i <- cdf_candidate
            beta_strength <- 1e-6
            resolved_shape <- TRUE
            break
          }
        }
        if (resolved_shape) {
          adjustment_method <- paste0(
            adjustment_method,
            "+BOUNDED_WEAK_SAFETY_FALLBACK"
          )
        }
      } else if (
        !isTRUE(all.equal(parama_after, empirical_parama)) ||
        !isTRUE(all.equal(paramb_after, empirical_paramb))
      ) {
        adjustment_method <- paste0(
          adjustment_method,
          "+LOWER_CONCENTRATION_FOR_LIKELIHOOD_MEDIAN"
        )
      }
    } else if (identical(distn, "lnorm")) {
      if (!is.finite(likelihood_median) || likelihood_median <= 0) {
        stop(
          "Positive lognormal target ", trait_i,
          " has a non-positive likelihood median; cannot satisfy ",
          "PEcAn prior consistency without changing its physical domain.",
          call. = FALSE
        )
      }
      required_sdlog <- abs(
        log(likelihood_median) - parama_after
      ) / z_limit
      if (
        is.finite(required_sdlog) &&
        required_sdlog > paramb_after
      ) {
        paramb_after <- required_sdlog * 1.01
        adjustment_method <- "WIDEN_SDLOG_FOR_LIKELIHOOD_MEDIAN"
      }
    } else if (identical(distn, "norm")) {
      required_sd <- abs(
        likelihood_median - parama_after
      ) / z_limit
      if (is.finite(required_sd) && required_sd > paramb_after) {
        paramb_after <- required_sd * 1.01
        adjustment_method <- "WIDEN_SD_FOR_LIKELIHOOD_MEDIAN"
      }
    } else {
      stop(
        "No prior preflight widening rule for distribution ",
        distn, " (", trait_i, ").",
        call. = FALSE
      )
    }

    cdf_before <- .prema_prior_cdf(
      likelihood_median,
      distn,
      parama_before,
      paramb_before
    )
    cdf_after <- .prema_prior_cdf(
      likelihood_median,
      distn,
      parama_after,
      paramb_after
    )
    resolved <- (
      is.finite(cdf_after) &&
      cdf_after >= pecan_consistency_tail &&
      cdf_after <= 1 - pecan_consistency_tail
    )
    if (!resolved) {
      stop(
        "Prior preflight could not make ", trait_i,
        " compatible with its likelihood median while preserving domain. ",
        "Likelihood median=", signif(likelihood_median, 8),
        "; distribution=", distn,
        "; final CDF=", signif(cdf_after, 8),
        ". Check prior_species_values and target domain.",
        call. = FALSE
      )
    }

    prior.distns[trait_i, "parama"] <- as.numeric(parama_after)
    prior.distns[trait_i, "paramb"] <- as.numeric(paramb_after)
    preflight_rows[[trait_index]] <- data.table::data.table(
      trait = trait_i,
      n_prior_species = data.table::uniqueN(
        values[pecan_trait == trait_i, species_key]
      ),
      selected_prior_species = paste(
        sort(unique(values[pecan_trait == trait_i, species_key])),
        collapse = " | "
      ),
      n_likelihood_species = data.table::uniqueN(
        likelihood[pecan_trait == trait_i, species_key]
      ),
      n_prior_likelihood_species_overlap = length(intersect(
        unique(values[pecan_trait == trait_i, species_key]),
        unique(likelihood[pecan_trait == trait_i, species_key])
      )),
      prior_likelihood_species_overlap = all(
        unique(values[pecan_trait == trait_i, species_key]) %in%
          unique(likelihood[pecan_trait == trait_i, species_key])
      ),
      n_raw_likelihood_rows = length(likelihood_i),
      n_pecan_jagged_rows = as.integer(jagged_i$n_jagged_rows),
      prior_species_center = prior_center,
      prior_species_sd = prior_species_sd,
      raw_likelihood_min = likelihood_min,
      raw_likelihood_median = raw_likelihood_median,
      raw_likelihood_max = likelihood_max,
      pecan_jagged_min = as.numeric(jagged_i$jagged_min),
      likelihood_median = likelihood_median,
      likelihood_median_source =
        "median(PEcAn.MA::jagify(trait.data, use_ghs=FALSE)$Y)",
      pecan_jagged_max = as.numeric(jagged_i$jagged_max),
      distn = distn,
      parama_before_preflight = parama_before,
      paramb_before_preflight = paramb_before,
      empirical_parama = empirical_parama,
      empirical_paramb = empirical_paramb,
      parama_after_preflight = as.numeric(parama_after),
      paramb_after_preflight = as.numeric(paramb_after),
      likelihood_median_cdf_before = cdf_before,
      likelihood_median_cdf_after = cdf_after,
      pecan_error_tail = 5e-4,
      pecan_warning_tail = 0.025,
      pecan_consistency_tail = as.numeric(pecan_consistency_tail),
      adjusted_for_likelihood = (
        !isTRUE(all.equal(parama_after, empirical_parama)) ||
        !isTRUE(all.equal(paramb_after, empirical_paramb))
      ),
      adjustment_method = adjustment_method,
      beta_final_strength = beta_strength,
      pecan_preflight_resolved = resolved
    )
  }

  preflight_audit <- data.table::rbindlist(
    preflight_rows,
    use.names = TRUE,
    fill = TRUE
  )
  prior_parameter_audit <- merge(
    base_parameter_audit,
    preflight_audit,
    by = "trait",
    all = TRUE,
    sort = FALSE
  )
  prior_parameter_audit[, target_order__ := match(trait, targets)]
  data.table::setorder(prior_parameter_audit, target_order__)
  prior_parameter_audit[, target_order__ := NULL]

  prior_configuration <- attr(
    prior.distns,
    "prior_sampling_configuration"
  )
  prior_configuration$species_summary <- "one median per selected species"
  prior_configuration$pecan_consistency_tail <-
    as.numeric(pecan_consistency_tail)
  prior_configuration$preflight_point <-
    "exact median of PEcAn.MA::jagify(..., use_ghs=FALSE)$Y"
  prior_configuration$preflight_rule <-
    "same CDF rule used by PEcAn.MA::meta_analysis_standalone"
  prior_configuration$bounded_prior_support_preserved <- TRUE

  attr(prior.distns, "prior_species_values") <- values[]
  attr(prior.distns, "prior_basis") <-
    "one median per selected species; width preflighted on likelihood median"
  attr(prior.distns, "prior_parameter_audit") <-
    prior_parameter_audit[]
  attr(prior.distns, "prior_compatibility_audit") <-
    preflight_audit[]
  attr(prior.distns, "prior_sampling_configuration") <-
    prior_configuration

  list(
    prior.distns = prior.distns,
    prior_registry = prior_registry,
    prior_trait.data = prior_trait.data,
    prior_compatibility_audit = preflight_audit[]
  )
}


prema_observations_to_trait_data <- function(target_observations) {
  .prema_require_packages("data.table")
  x <- data.table::copy(data.table::as.data.table(target_observations))
  required <- c(
    "target_observation_id", "pecan_trait", "target_value", "site_id"
  )
  missing <- setdiff(required, names(x))
  if (length(missing) > 0L) {
    stop(
      "target_observations is missing: ", paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  citation_key <- .prema_nonempty_text(x$dataset_key)
  citation_key[is.na(citation_key)] <- .prema_nonempty_text(
    x$reference_key[is.na(citation_key)]
  )
  citation_key[is.na(citation_key)] <- "unknown"
  species_key <- .prema_nonempty_text(x$species_key)
  species_key[is.na(species_key)] <- "unknown"
  id_keys <- data.table::data.table(
    pecan_trait = as.character(x$pecan_trait),
    citation_key = citation_key,
    species_key = species_key
  )
  id_keys[
    ,
    citation_id := as.integer(factor(citation_key)),
    by = pecan_trait
  ]
  id_keys[
    ,
    specie_id := as.integer(factor(species_key)),
    by = pecan_trait
  ]
  n_value <- suppressWarnings(as.numeric(x$replicates))
  n_value[!is.finite(n_value) | n_value < 1] <- 1
  n_value <- as.integer(pmax(1, round(n_value)))

  try_ma_long <- data.table::data.table(
    id = as.integer(x$target_observation_id),
    citation_id = id_keys$citation_id,
    site_id = as.integer(x$site_id),
    treatment_id = 1L,
    name = "control",
    date = x$date,
    time = x$time,
    cultivar_id = NA_integer_,
    specie_id = id_keys$specie_id,
    species_name = x$species_name,
    mean = as.numeric(x$target_value),
    statname = NA_character_,
    stat = NA_real_,
    n = n_value,
    vname = as.character(x$pecan_trait),
    month = NA_integer_,
    lon = as.numeric(x$longitude),
    lat = as.numeric(x$latitude),
    control = 1L,
    greenhouse = 0L
  )

  list(
    try_ma_long = try_ma_long[],
    trait.data = make_trait_data_from_try_ma_long(try_ma_long)
  )
}


prema_target_prior_registry <- function(targets, writer_contract) {
  contract <- data.table::as.data.table(writer_contract)
  contract <- contract[match(targets, pecan_trait)]
  fractions <- c("growth_resp_factor", "waterRemoveFrac")

  data.table::data.table(
    trait = targets,
    canonical_unit = contract$pecan_input_unit,
    conversion_id = "PECAN_WRITER_TARGET",
    domain = ifelse(
      targets %in% fractions,
      "unit_interval",
      ifelse(targets == "leafC", "percent_interval", "positive")
    ),
    distribution = ifelse(
      targets %in% fractions,
      "beta",
      ifelse(targets == "leafC", "unif", "lnorm")
    ),
    lower = ifelse(
      targets %in% fractions | targets == "leafC",
      0,
      NA_real_
    ),
    upper = ifelse(
      targets %in% fractions,
      1,
      ifelse(targets == "leafC", 100, NA_real_)
    )
  )
}


prema_target_range_rules <- function(targets, writer_contract) {
  contract <- data.table::as.data.table(writer_contract)
  contract <- contract[match(targets, pecan_trait)]
  data.table::data.table(
    trait = targets,
    lower = ifelse(targets == "leafC", 0, 0),
    upper = ifelse(
      targets == "leafC",
      100,
      ifelse(targets %in% c("growth_resp_factor", "waterRemoveFrac"), 1, Inf)
    ),
    action = "FAIL",
    expected_unit = contract$pecan_input_unit
  )
}


.prema_rhat <- function(mcmc_object, variable) {
  if (length(mcmc_object) < 2L) return(NA_real_)
  chains <- lapply(mcmc_object, function(chain) {
    matrix_i <- as.matrix(chain)
    if (!variable %in% colnames(matrix_i)) return(NULL)
    coda::mcmc(matrix_i[, variable, drop = FALSE])
  })
  if (any(vapply(chains, is.null, logical(1)))) return(NA_real_)
  list_i <- do.call(coda::mcmc.list, chains)
  tryCatch(
    as.numeric(coda::gelman.diag(
      list_i,
      autoburnin = FALSE,
      multivariate = FALSE
    )$psrf[1L, "Point est."]),
    error = function(e) NA_real_
  )
}


.prema_ess <- function(mcmc_object, variable) {
  chains <- lapply(mcmc_object, function(chain) {
    matrix_i <- as.matrix(chain)
    if (!variable %in% colnames(matrix_i)) return(NULL)
    coda::mcmc(matrix_i[, variable, drop = FALSE])
  })
  if (any(vapply(chains, is.null, logical(1)))) return(NA_real_)
  list_i <- do.call(coda::mcmc.list, chains)
  tryCatch(
    as.numeric(coda::effectiveSize(list_i)[[1L]]),
    error = function(e) NA_real_
  )
}


summarize_prema_site_variability <- function(
    ma_result,
    trait.data = NULL,
    rhat_threshold = 1.2,
    min_effective_size = 100
) {
  .prema_require_packages(c("data.table", "coda"))
  traits <- names(ma_result$trait.mcmc)
  rows <- lapply(traits, function(trait_i) {
    object_i <- ma_result$trait.mcmc[[trait_i]]
    matrix_i <- as.matrix(object_i)
    site_columns <- grep("^beta\\.site\\[", colnames(matrix_i), value = TRUE)
    n_sites_analyzed <- if (
      !is.null(trait.data) &&
      trait_i %in% names(trait.data) &&
      "site_id" %in% names(trait.data[[trait_i]])
    ) {
      data.table::uniqueN(trait.data[[trait_i]]$site_id, na.rm = TRUE)
    } else if (
      trait_i %in% names(ma_result$jagged.data) &&
      "site" %in% names(ma_result$jagged.data[[trait_i]])
    ) {
      data.table::uniqueN(
        ma_result$jagged.data[[trait_i]]$site,
        na.rm = TRUE
      )
    } else {
      length(site_columns)
    }
    has_sd_site <- "sd.site" %in% colnames(matrix_i)
    sd_values <- if (has_sd_site) {
      suppressWarnings(as.numeric(matrix_i[, "sd.site"]))
    } else {
      numeric()
    }
    sd_values <- sd_values[is.finite(sd_values)]
    rhat <- if (has_sd_site) .prema_rhat(object_i, "sd.site") else NA_real_
    ess <- if (has_sd_site) .prema_ess(object_i, "sd.site") else NA_real_
    status <- if (n_sites_analyzed < 2L) {
      "NOT_ESTIMABLE_LT2_SITES"
    } else if (!has_sd_site) {
      "NOT_ESTIMATED"
    } else if (length(sd_values) == 0L) {
      "FAIL_NONFINITE"
    } else if (!is.finite(rhat)) {
      "REVIEW_RHAT_UNAVAILABLE"
    } else if (is.finite(rhat) && rhat > rhat_threshold) {
      "REVIEW_RHAT"
    } else if (!is.finite(ess)) {
      "REVIEW_ESS_UNAVAILABLE"
    } else if (is.finite(ess) && ess < min_effective_size) {
      "REVIEW_LOW_ESS"
    } else {
      "PASS"
    }
    quantiles <- if (length(sd_values) > 0L) {
      stats::quantile(
        sd_values,
        c(0.025, 0.5, 0.975),
        names = FALSE,
        na.rm = TRUE
      )
    } else {
      rep(NA_real_, 3L)
    }
    data.table::data.table(
      pecan_trait = trait_i,
      n_sites_analyzed = as.integer(n_sites_analyzed),
      n_site_effect_levels = length(site_columns),
      has_sd_site = has_sd_site,
      sd_site_q025 = quantiles[[1L]],
      sd_site_median = quantiles[[2L]],
      sd_site_q975 = quantiles[[3L]],
      sd_site_rhat = rhat,
      sd_site_effective_size = ess,
      site_variability_status = status
    )
  })
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}


.prema_sample_default <- function(spec, n, trait) {
  center <- as.numeric(spec$center[[1L]])
  distribution <- as.character(spec$distribution[[1L]])
  uncertainty <- as.numeric(spec$uncertainty[[1L]])
  if (distribution == "beta") {
    center <- pmin(pmax(center, 1e-4), 1 - 1e-4)
    concentration <- max(2, uncertainty)
    return(stats::rbeta(
      n,
      shape1 = center * concentration,
      shape2 = (1 - center) * concentration
    ))
  }
  if (distribution == "lnorm") {
    value <- center * exp(stats::rnorm(
      n,
      mean = -0.5 * uncertainty^2,
      sd = uncertainty
    ))
  } else {
    value <- stats::rnorm(n, mean = center, sd = uncertainty)
  }

  valid <- .prema_target_domain_valid(trait, value)
  attempts <- 0L
  while (!all(valid) && attempts < 30L) {
    attempts <- attempts + 1L
    n_bad <- sum(!valid)
    replacement <- if (distribution == "lnorm") {
      center * exp(stats::rnorm(
        n_bad,
        mean = -0.5 * uncertainty^2,
        sd = uncertainty
      ))
    } else {
      stats::rnorm(n_bad, mean = center, sd = uncertainty)
    }
    value[!valid] <- replacement
    valid <- .prema_target_domain_valid(trait, value)
  }
  if (!all(valid)) {
    stop(
      "Could not generate domain-valid default draws for ", trait, ".",
      call. = FALSE
    )
  }
  value
}


.prema_sample_ma_target <- function(
    mcmc_object,
    trait,
    n_draws,
    include_site_variability,
    site_variability_status,
    max_attempts = 30L
) {
  matrix_i <- as.matrix(mcmc_object)
  if (!"beta.o" %in% colnames(matrix_i)) {
    stop("MCMC object has no beta.o for ", trait, call. = FALSE)
  }
  beta <- suppressWarnings(as.numeric(matrix_i[, "beta.o"]))
  has_valid_sd <-
    "sd.site" %in% colnames(matrix_i) &&
    identical(site_variability_status, "PASS")
  sd_site <- if (has_valid_sd) {
    suppressWarnings(as.numeric(matrix_i[, "sd.site"]))
  } else {
    rep(NA_real_, nrow(matrix_i))
  }
  valid_row <- is.finite(beta)
  if (has_valid_sd) valid_row <- valid_row & is.finite(sd_site) & sd_site >= 0
  available <- which(valid_row)
  if (length(available) == 0L) {
    stop("No finite posterior rows for ", trait, call. = FALSE)
  }

  global_index <- sample(
    available,
    size = n_draws,
    replace = length(available) < n_draws
  )
  global_value <- beta[global_index]
  global_valid <- .prema_target_domain_valid(trait, global_value)
  if (!all(global_valid)) {
    valid_beta <- beta[available][
      .prema_target_domain_valid(trait, beta[available])
    ]
    if (length(valid_beta) == 0L) {
      stop("No domain-valid beta.o posterior for ", trait, call. = FALSE)
    }
    global_value[!global_valid] <- sample(
      valid_beta,
      sum(!global_valid),
      replace = TRUE
    )
  }

  if (!isTRUE(include_site_variability) || !has_valid_sd) {
    return(list(
      global = global_value,
      site_predictive = global_value,
      site_variability_used = FALSE,
      n_rejected_site_draws = 0L
    ))
  }

  accepted <- numeric()
  rejected <- 0L
  attempts <- 0L
  while (length(accepted) < n_draws && attempts < max_attempts) {
    attempts <- attempts + 1L
    need <- n_draws - length(accepted)
    candidate_index <- sample(
      available,
      size = max(need * 2L, need),
      replace = TRUE
    )
    candidate <- beta[candidate_index] + stats::rnorm(
      length(candidate_index),
      mean = 0,
      sd = sd_site[candidate_index]
    )
    valid <- .prema_target_domain_valid(trait, candidate)
    accepted <- c(accepted, candidate[valid])
    rejected <- rejected + sum(!valid)
  }
  if (length(accepted) < n_draws) {
    warning(
      "Could not obtain enough domain-valid new-site draws for ", trait,
      "; filling the remainder with beta.o draws.",
      call. = FALSE
    )
    accepted <- c(
      accepted,
      sample(global_value, n_draws - length(accepted), replace = TRUE)
    )
  }

  list(
    global = global_value,
    site_predictive = accepted[seq_len(n_draws)],
    site_variability_used = TRUE,
    n_rejected_site_draws = rejected
  )
}


build_prema_posterior_samples_21 <- function(
    ma_result,
    ma_qc,
    site_variability_summary,
    writer_contract,
    pft_name,
    n_draws = 1000L,
    seed = 20260903L,
    include_site_variability = TRUE,
    default_specs = prema_default_only_specs_21()
) {
  .prema_require_packages(c("data.table", "coda"))
  targets <- prema_pecan_targets_21()
  default_specs <- data.table::as.data.table(default_specs)
  missing_defaults <- setdiff(targets, default_specs$pecan_trait)
  if (length(missing_defaults) > 0L) {
    stop(
      "default_specs is missing: ", paste(missing_defaults, collapse = ", "),
      call. = FALSE
    )
  }
  contract <- data.table::as.data.table(writer_contract)[
    match(targets, pecan_trait)
  ]
  qc_summary <- data.table::as.data.table(ma_qc$summary)
  pass_traits <- as.character(ma_qc$passed_traits)
  set.seed(as.integer(seed))

  global_draws <- data.table::data.table(draw_id = seq_len(n_draws))
  site_draws <- data.table::data.table(draw_id = seq_len(n_draws))
  metadata_rows <- vector("list", length(targets))

  for (target_index in seq_along(targets)) {
    target <- targets[[target_index]]
    qc_status <- qc_summary[trait == target, status]
    if (length(qc_status) == 0L) qc_status <- "NO_MA"
    site_row <- site_variability_summary[pecan_trait == target]
    site_status <- if (nrow(site_row) == 1L) {
      site_row$site_variability_status[[1L]]
    } else {
      "NOT_AVAILABLE"
    }

    if (target %in% pass_traits && target %in% names(ma_result$trait.mcmc)) {
      sampled <- .prema_sample_ma_target(
        mcmc_object = ma_result$trait.mcmc[[target]],
        trait = target,
        n_draws = n_draws,
        include_site_variability = include_site_variability,
        site_variability_status = site_status
      )
      global_value <- sampled$global
      site_value <- sampled$site_predictive
      output_source <- if (sampled$site_variability_used) {
        "QC_PASS_MA_NEW_SITE_PREDICTIVE"
      } else {
        "QC_PASS_MA_BETA_O"
      }
      default_used <- FALSE
      default_basis <- ""
      default_center <- NA_real_
      n_rejected <- sampled$n_rejected_site_draws
      site_used <- sampled$site_variability_used
    } else {
      spec <- default_specs[pecan_trait == target]
      global_value <- .prema_sample_default(spec, n_draws, target)
      site_value <- global_value
      output_source <- "DEFAULT_ONLY_NO_QC_PASS_MA"
      default_used <- TRUE
      default_basis <- spec$basis[[1L]]
      default_center <- spec$center[[1L]]
      n_rejected <- 0L
      site_used <- FALSE
    }

    data.table::set(global_draws, j = target, value = global_value)
    data.table::set(site_draws, j = target, value = site_value)
    metadata_rows[[target_index]] <- data.table::data.table(
      target_order = target_index,
      pecan_trait = target,
      pecan_unit = contract[pecan_trait == target, pecan_input_unit][[1L]],
      ma_qc_status = qc_status[[1L]],
      site_variability_status = site_status,
      site_variability_used_in_predictive_draws = site_used,
      sd_site_median = if (nrow(site_row) == 1L) {
        site_row$sd_site_median[[1L]]
      } else {
        NA_real_
      },
      output_source = output_source,
      default_used = default_used,
      default_center = default_center,
      default_basis = default_basis,
      n_rejected_site_predictive_draws = n_rejected,
      writer_ready = all(.prema_target_domain_valid(target, site_value))
    )
  }

  list(
    global_mean_draws_21 = global_draws[],
    new_site_predictive_draws_21 = site_draws[],
    metadata_21 = data.table::rbindlist(metadata_rows),
    pass_ma_traits = pass_traits
  )
}


save_prema_pecan_samples <- function(
    posterior_samples,
    pft_name,
    outdir,
    sample_mode = c("new_site_predictive", "global_mean")
) {
  .prema_require_packages("data.table")
  sample_mode <- match.arg(sample_mode)
  draws <- if (sample_mode == "new_site_predictive") {
    data.table::copy(posterior_samples$new_site_predictive_draws_21)
  } else {
    data.table::copy(posterior_samples$global_mean_draws_21)
  }
  targets <- prema_pecan_targets_21()
  missing <- setdiff(targets, names(draws))
  if (length(missing) > 0L) {
    stop("Posterior draw table is missing: ", paste(missing, collapse = ", "))
  }
  invalid <- targets[
    !vapply(
      targets,
      function(target) all(.prema_target_domain_valid(target, draws[[target]])),
      logical(1)
    )
  ]
  if (length(invalid) > 0L) {
    stop(
      "Cannot export domain-invalid traits: ", paste(invalid, collapse = ", "),
      call. = FALSE
    )
  }

  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  selected_parameters <- as.data.frame(draws[, ..targets])
  pft_trait_samples <- lapply(selected_parameters, as.numeric)
  names(pft_trait_samples) <- targets
  trait.samples <- stats::setNames(list(pft_trait_samples), pft_name)
  ensemble.samples <- stats::setNames(list(selected_parameters), pft_name)
  sa.samples <- list()
  runs.samples <- list()
  env.samples <- list()

  trait.values.by.draw <- lapply(seq_len(nrow(selected_parameters)), function(i) {
    values_i <- as.numeric(unlist(
      selected_parameters[i, , drop = FALSE],
      use.names = FALSE
    ))
    names(values_i) <- targets
    stats::setNames(list(values_i), pft_name)
  })
  names(trait.values.by.draw) <- sprintf(
    "ensemble_%06d",
    seq_along(trait.values.by.draw)
  )
  trait.values <- trait.values.by.draw[[1L]]

  save(
    ensemble.samples,
    trait.samples,
    sa.samples,
    runs.samples,
    env.samples,
    file = file.path(outdir, "samples.Rdata"),
    compress = FALSE
  )
  saveRDS(
    list(
      trait.samples = trait.samples,
      ensemble.samples = ensemble.samples,
      sa.samples = sa.samples,
      runs.samples = runs.samples,
      env.samples = env.samples,
      sample_mode = sample_mode
    ),
    file.path(outdir, "pecan_samples.rds"),
    compress = FALSE
  )
  saveRDS(
    trait.values.by.draw,
    file.path(outdir, "sipnet_trait_values_by_draw.rds"),
    compress = FALSE
  )
  save(
    trait.values,
    file = file.path(outdir, "sipnet_trait.values_first_draw.Rdata"),
    compress = FALSE
  )
  data.table::fwrite(
    draws,
    file.path(outdir, paste0(sample_mode, "_joint_draws.csv"))
  )

  invisible(list(
    trait.samples = trait.samples,
    ensemble.samples = ensemble.samples,
    trait.values.by.draw = trait.values.by.draw,
    draws = draws,
    sample_mode = sample_mode
  ))
}


run_prema_pecan_trait_ma <- function(
    pft_name,
    output_dir,
    pftspecies_rdata,
    trydat_use_species_rdata,
    pft_coordinate_map_file = NULL,
    pft_assignment_mode = c(
      "pft_range_species", "share_species", "spatial_observation"
    ),
    observation_coordinate_file = NULL,
    observation_coordinate_data = NULL,
    pft_range_buffer_degrees = 1,
    prior_species_fraction = 0.10,
    prior_fraction_rule_min_species = 20L,
    prior_no_holdout_max_species = 2L,
    iterations = 3000L,
    workers = 2L,
    n_output_draws = 1000L,
    random = TRUE,
    seed = 20260904L,
    context_overrides = list(),
    include_site_variability_in_samples = TRUE,
    sample_mode = c("new_site_predictive", "global_mean"),
    unsupported_unit_action = c("stop", "drop"),
    max_pft_distance_km = 250,
    resume = TRUE
) {
  .prema_require_packages(c(
    "data.table", "coda", "future", "furrr", "PEcAn.MA"
  ))
  .prema_require_functions(c(
    "build_try_unit_map",
    "convert_try_units_with_unit_map",
    "make_trait_data_from_try_ma_long",
    "make_prior_distns_from_trait_data",
    "run_pecan_ma_parallel",
    "qc_pecan_ma_result",
    "attach_try_observation_coordinates",
    "assign_try_observations_to_pft"
  ))
  sample_mode <- match.arg(sample_mode)
  pft_assignment_mode <- match.arg(pft_assignment_mode)
  unsupported_unit_action <- match.arg(unsupported_unit_action)
  if (!isTRUE(random)) {
    stop(
      "This entry point is specifically for site-to-site random effects; ",
      "set random = TRUE.",
      call. = FALSE
    )
  }
  pft_name <- trimws(as.character(pft_name))
  if (length(pft_name) != 1L || is.na(pft_name) || !nzchar(pft_name)) {
    stop("pft_name must be one non-empty string.", call. = FALSE)
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(output_dir)) {
    stop("Cannot create output_dir: ", output_dir, call. = FALSE)
  }
  output_dir <- normalizePath(output_dir, mustWork = TRUE)
  canonical_dir <- file.path(output_dir, "01_canonical_try")
  target_dir <- file.path(output_dir, "02_prema_pecan_observations")
  ma_dir <- file.path(output_dir, "03_meta_analysis")
  qc_dir <- file.path(output_dir, "04_ma_qc")
  export_dir <- file.path(output_dir, "05_pecan_samples")
  for (dir_i in c(canonical_dir, target_dir, ma_dir, qc_dir, export_dir)) {
    dir.create(dir_i, recursive = TRUE, showWarnings = FALSE)
  }

  pftspecies <- load_prema_rdata_object(
    pftspecies_rdata,
    c("pftspecies", "pft_species", "pft_species_map")
  )
  trydat_use_species <- load_prema_rdata_object(
    trydat_use_species_rdata,
    c("trydat_use_species", "trydat", "try_data")
  )
  pft_coordinate_path_provided <-
    !is.null(pft_coordinate_map_file) &&
    length(pft_coordinate_map_file) == 1L &&
    !is.na(pft_coordinate_map_file) &&
    nzchar(trimws(pft_coordinate_map_file))
  coordinate_aware_pft_mode <- pft_assignment_mode %in%
    c("pft_range_species", "spatial_observation")
  if (coordinate_aware_pft_mode && !pft_coordinate_path_provided) {
    stop(
      "pft_coordinate_map_file is required for ",
      pft_assignment_mode, " mode.",
      call. = FALSE
    )
  }
  pft_coordinate_map <- if (coordinate_aware_pft_mode) {
    load_prema_table(
      pft_coordinate_map_file,
      c("pft_coordinate_map", "final_pft_sites", "pft_sites")
    )
  } else {
    NULL
  }
  observation_coordinate_data_provided <-
    coordinate_aware_pft_mode && !is.null(observation_coordinate_data)
  observation_coordinate_path_provided <-
    coordinate_aware_pft_mode &&
    !observation_coordinate_data_provided &&
    !is.null(observation_coordinate_file) &&
    length(observation_coordinate_file) == 1L &&
    !is.na(observation_coordinate_file) &&
    nzchar(trimws(observation_coordinate_file))
  if (observation_coordinate_data_provided) {
    observation_coordinate_data <- data.table::as.data.table(
      observation_coordinate_data
    )
    if (nrow(observation_coordinate_data) == 0L) {
      stop(
        "observation_coordinate_data was supplied but has zero rows.",
        call. = FALSE
      )
    }
  } else if (observation_coordinate_path_provided) {
    observation_coordinate_data <- load_prema_table(
      observation_coordinate_file,
      c("observation_coordinates", "try_observation_coordinates")
    )
  }

  selected_try <- select_prema_pft_observations(
    trydat_use_species = trydat_use_species,
    pftspecies = pftspecies,
    pft_name = pft_name,
    pft_coordinate_map = pft_coordinate_map,
    assignment_mode = pft_assignment_mode,
    observation_coordinate_data = observation_coordinate_data,
    pft_range_buffer_degrees = pft_range_buffer_degrees,
    max_distance_km = max_pft_distance_km
  )
  pft_selection_audit <- attr(selected_try, "pft_selection_audit")
  excluded_ambiguous_rows <- attr(selected_try, "excluded_ambiguous_rows")
  observation_pft_assignment_audit <- attr(
    selected_try,
    "observation_pft_assignment_audit"
  )
  observation_coordinate_join_audit <- attr(
    selected_try,
    "observation_coordinate_join_audit"
  )
  pft_species_range_audit <- attr(
    selected_try,
    "pft_species_range_audit"
  )
  pft_reference_points_used <- attr(
    selected_try,
    "pft_reference_points_used"
  )
  if ("ErrorRisk" %in% names(selected_try)) {
    selected_try[, ErrorRisk_original_prema := ErrorRisk]
    selected_try[, ErrorRisk := NULL]
  }

  if (!exists("trait_map", inherits = TRUE)) {
    stop("Repository object `trait_map` was not sourced.", call. = FALSE)
  }
  unit_map <- build_try_unit_map(trait_map)
  canonical_try <- convert_try_units_with_unit_map(
    try_data = selected_try,
    unit_map = unit_map,
    unit_col = "UnitName",
    value_col = "StdValue",
    days_per_year = 365,
    unsupported_action = unsupported_unit_action
  )
  unit_conversion_audit <- attr(canonical_try, "unit_conversion_audit")
  saveRDS(canonical_try, file.path(canonical_dir, "canonical_try.rds"))
  saveRDS(unit_map, file.path(canonical_dir, "unit_map.rds"))
  data.table::fwrite(
    pft_selection_audit,
    file.path(canonical_dir, "pft_selection_audit.csv")
  )
  data.table::fwrite(
    excluded_ambiguous_rows,
    file.path(canonical_dir, "excluded_ambiguous_rows.csv")
  )
  if (identical(pft_assignment_mode, "pft_range_species")) {
    data.table::fwrite(
      excluded_ambiguous_rows,
      file.path(canonical_dir, "excluded_outside_range_rows.csv")
    )
  }
  data.table::fwrite(
    observation_pft_assignment_audit,
    file.path(canonical_dir, "observation_pft_assignment_audit.csv")
  )
  data.table::fwrite(
    observation_coordinate_join_audit,
    file.path(canonical_dir, "observation_coordinate_join_audit.csv")
  )
  data.table::fwrite(
    unit_conversion_audit,
    file.path(canonical_dir, "unit_conversion_audit.csv")
  )
  if (!is.null(pft_species_range_audit)) {
    data.table::fwrite(
      pft_species_range_audit,
      file.path(canonical_dir, "pft_species_range_audit.csv")
    )
  }
  if (!is.null(pft_reference_points_used)) {
    data.table::fwrite(
      pft_reference_points_used,
      file.path(canonical_dir, "pft_reference_points_used.csv")
    )
  }

  target_result <- build_prema_pecan_target_observations(
    canonical_try = canonical_try,
    pft_name = pft_name,
    seed = seed,
    context_overrides = context_overrides
  )
  prior_split <- split_prema_prior_species(
    target_observations = target_result$observations,
    prior_species_fraction = prior_species_fraction,
    fraction_rule_min_species = prior_fraction_rule_min_species,
    no_holdout_max_species = prior_no_holdout_max_species,
    seed = seed + 1L
  )
  target_formatted <- prema_observations_to_trait_data(
    prior_split$likelihood_observations
  )
  try_ma_long <- target_formatted$try_ma_long
  trait.data <- target_formatted$trait.data

  # The compatibility filename now contains the observations actually used by
  # the MA likelihood. The unsplit candidates are saved with an explicit
  # "_all" suffix, and prior-source rows are saved separately.
  saveRDS(
    prior_split$likelihood_observations,
    file.path(target_dir, "prema_pecan_target_observations.rds")
  )
  data.table::fwrite(
    prior_split$likelihood_observations,
    file.path(target_dir, "prema_pecan_target_observations.csv")
  )
  saveRDS(
    target_result$observations,
    file.path(target_dir, "prema_pecan_target_observations_all.rds")
  )
  data.table::fwrite(
    target_result$observations,
    file.path(target_dir, "prema_pecan_target_observations_all.csv")
  )
  saveRDS(
    prior_split$prior_source_observations,
    file.path(target_dir, "prior_species_source_observations.rds")
  )
  data.table::fwrite(
    prior_split$prior_source_observations,
    file.path(target_dir, "prior_species_source_observations.csv")
  )
  data.table::fwrite(
    prior_split$prior_species_values,
    file.path(target_dir, "prior_species_values.csv")
  )
  data.table::fwrite(
    prior_split$species_assignments,
    file.path(target_dir, "prior_species_assignments.csv")
  )
  data.table::fwrite(
    prior_split$trait_audit,
    file.path(target_dir, "prior_species_split_audit.csv")
  )
  data.table::fwrite(
    target_result$target_audit,
    file.path(target_dir, "prema_target_audit_21.csv")
  )
  data.table::fwrite(
    target_result$rule_audit,
    file.path(target_dir, "prema_conversion_rule_audit.csv")
  )
  saveRDS(try_ma_long, file.path(target_dir, "try_ma_long_targets.rds"))
  save(
    trait.data,
    file = file.path(target_dir, "trait.data.Rdata"),
    compress = FALSE
  )

  ma_traits <- names(trait.data)
  prior_preflight_jagged <- lapply(
    trait.data,
    PEcAn.MA::jagify,
    use_ghs = FALSE
  )
  empty_preflight_traits <- names(prior_preflight_jagged)[
    vapply(prior_preflight_jagged, nrow, integer(1)) == 0L
  ]
  if (length(empty_preflight_traits) > 0L) {
    stop(
      "No observations remain after PEcAn jagify for: ",
      paste(empty_preflight_traits, collapse = ", "),
      call. = FALSE
    )
  }
  prior_preflight_jagged_summary <- data.table::rbindlist(
    lapply(seq_along(prior_preflight_jagged), function(trait_index) {
      one_trait <- prior_preflight_jagged[[trait_index]]
      y_value <- suppressWarnings(as.numeric(one_trait[, "Y"]))
      data.table::data.table(
        pecan_trait = names(prior_preflight_jagged)[[trait_index]],
        n_jagged_rows = length(y_value),
        jagged_min = min(y_value),
        jagged_median = stats::median(y_value),
        jagged_max = max(y_value)
      )
    }),
    use.names = TRUE,
    fill = TRUE
  )
  prior_bundle <- make_prema_species_prior_distns(
    prior_species_values = prior_split$prior_species_values,
    likelihood_observations = prior_split$likelihood_observations,
    likelihood_jagged_summary = prior_preflight_jagged_summary,
    targets = ma_traits,
    writer_contract = target_result$writer_contract,
    unit_map = unit_map,
    seed = seed + 2L,
    width_multiplier = 10,
    relative_sd_floor = 0.10
  )
  prior.distns <- prior_bundle$prior.distns
  prior_registry <- prior_bundle$prior_registry
  prior_trait.data <- prior_bundle$prior_trait.data
  save(
    prior.distns,
    file = file.path(target_dir, "prior.distns.Rdata"),
    compress = FALSE
  )
  save(
    prior_trait.data,
    file = file.path(target_dir, "prior_species_trait.data.Rdata"),
    compress = FALSE
  )
  data.table::fwrite(
    prior_registry,
    file.path(target_dir, "prior_registry_targets.csv")
  )
  data.table::fwrite(
    attr(prior.distns, "prior_parameter_audit"),
    file.path(target_dir, "prior_parameter_audit.csv")
  )
  data.table::fwrite(
    prior_bundle$prior_compatibility_audit,
    file.path(target_dir, "prior_compatibility_audit.csv")
  )
  data.table::fwrite(
    prior_preflight_jagged_summary,
    file.path(target_dir, "prior_preflight_jagged_summary.csv")
  )
  saveRDS(
    attr(prior.distns, "prior_sampling_configuration"),
    file.path(target_dir, "prior_sampling_configuration.rds")
  )

  ma_result <- run_pecan_ma_parallel(
    trait.data = trait.data,
    prior.distns = prior.distns,
    pft_name = pft_name,
    outdir = ma_dir,
    iterations = as.integer(iterations),
    random = TRUE,
    threshold = 1.2,
    use_ghs = FALSE,
    gamma_tau = 0.01,
    workers = as.integer(workers),
    resume = isTRUE(resume),
    save_combined = TRUE
  )
  saveRDS(
    ma_result,
    file.path(ma_dir, "ma_result.rds"),
    compress = FALSE
  )

  range_rules <- prema_target_range_rules(
    ma_traits,
    target_result$writer_contract
  )
  ma_qc <- qc_pecan_ma_result(
    ma_result = ma_result,
    trait.data = trait.data,
    outdir = qc_dir,
    range_rules = range_rules,
    classification_policy = "minimum_bridge"
  )
  saveRDS(
    ma_qc,
    file.path(qc_dir, "ma_qc_result.rds"),
    compress = FALSE
  )

  site_variability_summary <- summarize_prema_site_variability(
    ma_result,
    trait.data = trait.data
  )
  data.table::fwrite(
    site_variability_summary,
    file.path(qc_dir, "site_variability_summary.csv")
  )
  saveRDS(
    site_variability_summary,
    file.path(qc_dir, "site_variability_summary.rds")
  )

  posterior_samples <- build_prema_posterior_samples_21(
    ma_result = ma_result,
    ma_qc = ma_qc,
    site_variability_summary = site_variability_summary,
    writer_contract = target_result$writer_contract,
    pft_name = pft_name,
    n_draws = as.integer(n_output_draws),
    seed = seed + 3L,
    include_site_variability = include_site_variability_in_samples
  )
  saveRDS(
    posterior_samples,
    file.path(export_dir, "pecan_posterior_samples_21.rds"),
    compress = FALSE
  )
  data.table::fwrite(
    posterior_samples$global_mean_draws_21,
    file.path(export_dir, "global_mean_draws_21.csv")
  )
  data.table::fwrite(
    posterior_samples$new_site_predictive_draws_21,
    file.path(export_dir, "new_site_predictive_draws_21.csv")
  )
  data.table::fwrite(
    posterior_samples$metadata_21,
    file.path(export_dir, "pecan_trait_metadata_21.csv")
  )

  pecan_export <- save_prema_pecan_samples(
    posterior_samples = posterior_samples,
    pft_name = pft_name,
    outdir = export_dir,
    sample_mode = sample_mode
  )

  audit_scalar <- function(name, default = NA) {
    if (name %in% names(pft_selection_audit)) {
      pft_selection_audit[[name]][[1L]]
    } else {
      default
    }
  }
  pipeline_summary <- data.table::data.table(
    pipeline_version = PREMA_PECAN_PIPELINE_VERSION,
    pft = pft_name,
    output_dir = output_dir,
    random_effects = TRUE,
    sample_mode = sample_mode,
    ma_qc_policy = ma_qc$classification_policy,
    pft_assignment_mode = pft_assignment_mode,
    pft_range_buffer_degrees = audit_scalar(
      "pft_range_buffer_degrees",
      NA_real_
    ),
    n_pft_reference_points = audit_scalar(
      "target_reference_points",
      NA_integer_
    ),
    n_candidate_pft_species = audit_scalar(
      "target_candidate_species",
      NA_integer_
    ),
    n_selected_pft_species = audit_scalar(
      "selected_species",
      data.table::uniqueN(selected_try$AccSpeciesID)
    ),
    n_selected_in_range_species = audit_scalar(
      "selected_in_range_species",
      NA_integer_
    ),
    n_selected_no_coordinate_species = audit_scalar(
      "selected_no_coordinate_species",
      NA_integer_
    ),
    n_excluded_outside_range_species = audit_scalar(
      "excluded_outside_range_species",
      NA_integer_
    ),
    n_selected_unique_species_rows =
      pft_selection_audit$selected_unique_species_rows,
    n_selected_shared_species_rows =
      pft_selection_audit$selected_shared_species_rows,
    n_selected_spatial_rows = pft_selection_audit$selected_spatial_rows,
    n_unassigned_pft_candidate_rows =
      pft_selection_audit$excluded_unassigned_candidate_rows,
    n_canonical_try_rows = nrow(canonical_try),
    n_prema_target_rows = nrow(target_result$observations),
    n_likelihood_target_rows =
      nrow(prior_split$likelihood_observations),
    n_prior_source_rows =
      nrow(prior_split$prior_source_observations),
    n_prior_species_used_across_traits = sum(
      prior_split$trait_audit$n_prior_species_used
    ),
    n_prior_species_randomly_selected_across_traits = sum(
      prior_split$trait_audit$n_prior_species_selected_randomly
    ),
    n_prior_species_removed_from_likelihood = sum(
      prior_split$trait_audit$n_prior_species_removed_from_likelihood
    ),
    n_traits_with_random_prior_species_sample = prior_split$trait_audit[
      n_prior_species_selected_randomly > 0L, .N
    ],
    n_traits_no_random_prior_sample_le2 = prior_split$trait_audit[
      selection_rule == "ALL_SPECIES_PRIOR_NO_RANDOM_SAMPLE_LE2", .N
    ],
    n_priors_adjusted_for_likelihood_compatibility =
      prior_bundle$prior_compatibility_audit[
        adjusted_for_likelihood == TRUE, .N
      ],
    n_targets_using_multiple_routes = target_result$target_audit[
      primary_source == "MULTI_ROUTE", .N
    ],
    n_target_rows_deduplicated = sum(
      target_result$target_audit$n_deduplicated_same_observation,
      na.rm = TRUE
    ),
    n_targets_entering_ma = length(ma_traits),
    n_ma_qc_pass = length(ma_qc$passed_traits),
    n_ma_qc_review = length(ma_qc$review_traits),
    n_ma_qc_fail = length(ma_qc$failed_traits),
    n_targets_using_site_variability = posterior_samples$metadata_21[
      site_variability_used_in_predictive_draws == TRUE,
      .N
    ],
    n_default_only_targets = posterior_samples$metadata_21[
      default_used == TRUE,
      .N
    ],
    n_output_draws = as.integer(n_output_draws)
  )
  data.table::fwrite(
    pipeline_summary,
    file.path(output_dir, "pipeline_summary.csv")
  )

  pipeline_bundle <- list(
    version = PREMA_PECAN_PIPELINE_VERSION,
    pft = pft_name,
    configuration = list(
      output_dir = output_dir,
      pftspecies_rdata = normalizePath(pftspecies_rdata, mustWork = TRUE),
      trydat_use_species_rdata = normalizePath(
        trydat_use_species_rdata,
        mustWork = TRUE
      ),
      pft_assignment_mode = pft_assignment_mode,
      pft_coordinate_map_file = if (coordinate_aware_pft_mode) {
        normalizePath(pft_coordinate_map_file, mustWork = TRUE)
      } else {
        NA_character_
      },
      observation_coordinate_file = if (
        !observation_coordinate_path_provided
      ) {
        NA_character_
      } else {
        normalizePath(observation_coordinate_file, mustWork = TRUE)
      },
      observation_coordinate_source = if (
        identical(pft_assignment_mode, "share_species")
      ) {
        "NOT_USED_SHARE_SPECIES"
      } else if (observation_coordinate_data_provided) {
        "IN_MEMORY_R_OBJECT"
      } else if (observation_coordinate_path_provided) {
        "FILE"
      } else {
        "TRY_INPUT_COLUMNS_ONLY"
      },
      pft_range_buffer_degrees = if (
        identical(pft_assignment_mode, "pft_range_species")
      ) {
        as.numeric(pft_range_buffer_degrees)
      } else {
        NA_real_
      },
      max_pft_distance_km = if (
        identical(pft_assignment_mode, "spatial_observation")
      ) {
        as.numeric(max_pft_distance_km)
      } else {
        NA_real_
      },
      prior_species_fraction = as.numeric(prior_species_fraction),
      prior_fraction_rule_min_species =
        as.integer(prior_fraction_rule_min_species),
      prior_no_holdout_max_species =
        as.integer(prior_no_holdout_max_species),
      prior_species_summary = "median",
      iterations = as.integer(iterations),
      workers = as.integer(workers),
      random = TRUE,
      ma_qc_policy = ma_qc$classification_policy,
      include_site_variability_in_samples =
        include_site_variability_in_samples,
      sample_mode = sample_mode,
      seed = as.integer(seed)
    ),
    pft_selection_audit = pft_selection_audit,
    pft_species_range_audit = pft_species_range_audit,
    pft_reference_points_used = pft_reference_points_used,
    observation_pft_assignment_audit = observation_pft_assignment_audit,
    observation_coordinate_join_audit = observation_coordinate_join_audit,
    target_audit = target_result$target_audit,
    prior_species_audit = prior_split$trait_audit,
    prior_species_assignments = prior_split$species_assignments,
    prior_species_values = prior_split$prior_species_values,
    prior_sampling_configuration = prior_split$configuration,
    prior_compatibility_audit =
      prior_bundle$prior_compatibility_audit,
    ma_result = ma_result,
    ma_qc = ma_qc,
    site_variability_summary = site_variability_summary,
    posterior_samples_21 = posterior_samples,
    pecan_export = pecan_export,
    pipeline_summary = pipeline_summary
  )
  saveRDS(
    pipeline_bundle,
    file.path(output_dir, "prema_pecan_pipeline_bundle.rds"),
    compress = FALSE
  )

  message(
    "\nPre-MA PEcAn-target pipeline complete.",
    "\nPFT: ", pft_name,
    "\nPFT assignment mode: ", pft_assignment_mode,
    "\nEligible PFT species: ",
    pipeline_summary$n_selected_pft_species,
    "\nSpecies included because all coordinates were missing: ",
    pipeline_summary$n_selected_no_coordinate_species,
    "\nSpecies excluded because known coordinates were outside range: ",
    pipeline_summary$n_excluded_outside_range_species,
    "\nTargets entering random-effect MA: ", length(ma_traits),
    "\nPrior species randomly sampled across target-specific fits: ",
    pipeline_summary$n_prior_species_randomly_selected_across_traits,
    "\nPrior species removed from likelihood: ",
    pipeline_summary$n_prior_species_removed_from_likelihood,
    "\nTargets with <=2 species and no random prior sample: ",
    pipeline_summary$n_traits_no_random_prior_sample_le2,
    "\nPriors widened for PEcAn likelihood-median compatibility: ",
    pipeline_summary$n_priors_adjusted_for_likelihood_compatibility,
    "\nQC policy: ", ma_qc$classification_policy,
    "\nQC PASS: ", length(ma_qc$passed_traits),
    "\nTargets using sd.site in new-site draws: ",
    pipeline_summary$n_targets_using_site_variability,
    "\nDefault-only fallback targets: ",
    pipeline_summary$n_default_only_targets,
    "\nPrior split audit: ",
    file.path(target_dir, "prior_species_split_audit.csv"),
    "\nPEcAn samples: ", file.path(export_dir, "samples.Rdata"),
    "\nOutput: ", output_dir
  )

  invisible(pipeline_bundle)
}
