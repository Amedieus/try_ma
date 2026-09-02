options(stringsAsFactors = FALSE)


# =============================================================================
# RStudio: QC PASS posterior -> exactly 21 PEcAn traits
# =============================================================================
#
# Prerequisite
# ------------
# Run ma_prep first. This script can use the in-memory ma_qc object, or this
# saved file when starting from a fresh RStudio session:
#
#   <OUTPUT_ROOT>/<PFT_NAME>/03_ma_qc/trait.mcmc.QC_PASS.Rdata
#
# This script does NOT rerun TRY preparation or meta-analysis. It starts from
# the QC PASS posterior, applies the repository bridge, validates the declared
# PEcAn output units against the SIPNET writer unit contract, performs output
# domain QC, and saves exactly the 21 allowed PEcAn trait columns + metadata.
#
# RStudio use
# -----------
# 1. Edit PFT_NAME and, if needed, CODE_DIR / OUTPUT_ROOT below.
# 2. Click Source.
#
# soilWHC and AmaxFrac are deliberately not allowed as output targets.
# =============================================================================


# =============================================================================
# 0. User configuration
# =============================================================================

PFT_NAME <- "Evergreen_Broadleaf_Forest"

CODE_DIR <- paste0(
  "/projectnb/dietzelab/guYANG/",
  "TRY_meta_analysis/try_ma"
)

OUTPUT_ROOT <- paste0(
  "/projectnb/dietzelab/guYANG/",
  "TRY_meta_analysis"
)

N_BRIDGE_DRAWS <- 5000L
BRIDGE_SEED <- 20260902L

# When this script is sourced immediately after ma_prep in the same RStudio
# session, use ma_qc$ma_result_qc directly if the PFT names match. Otherwise,
# load trait.mcmc.QC_PASS.Rdata from disk.
USE_IN_MEMORY_QC <- TRUE

# TRUE: stop after saving audit files unless all 21 traits are complete.
# FALSE: save all 21 columns, using NA for unresolved/invalid values.
REQUIRE_ALL_21 <- FALSE

# A long CSV contains N_BRIDGE_DRAWS * 21 rows and is convenient for a front end.
WRITE_LONG_CSV <- TRUE


# =============================================================================
# 1. Fixed allowlist: these are the only PEcAn targets this script may save
# =============================================================================

PECAN_TARGETS_21 <- c(
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

EXCLUDED_TARGETS <- c("soilWHC", "AmaxFrac")

if (
  length(PECAN_TARGETS_21) != 21L ||
  anyDuplicated(PECAN_TARGETS_21) ||
  length(intersect(PECAN_TARGETS_21, EXCLUDED_TARGETS)) > 0L
) {
  stop("PECAN_TARGETS_21 allowlist is invalid.", call. = FALSE)
}


# =============================================================================
# 2. Basic checks and local source files
# =============================================================================

check_positive_integer <- function(value, name) {
  value_numeric <- suppressWarnings(as.numeric(value))
  
  if (
    length(value_numeric) != 1L ||
    is.na(value_numeric) ||
    !is.finite(value_numeric) ||
    value_numeric < 1 ||
    value_numeric != round(value_numeric)
  ) {
    stop(name, " must be a positive integer.", call. = FALSE)
  }
  
  as.integer(value_numeric)
}

load_rdata_object <- function(path, preferred_names) {
  if (!file.exists(path)) {
    stop("Cannot find QC PASS posterior file: ", path, call. = FALSE)
  }
  
  object_env <- new.env(parent = baseenv())
  loaded_names <- load(path, envir = object_env)
  preferred <- preferred_names[preferred_names %in% loaded_names]
  
  if (length(preferred) > 0L) {
    return(get(preferred[[1L]], envir = object_env, inherits = FALSE))
  }
  
  if (length(loaded_names) == 1L) {
    return(get(loaded_names[[1L]], envir = object_env, inherits = FALSE))
  }
  
  stop(
    "Cannot identify the trait.mcmc object in ", path,
    ". Objects found: ", paste(loaded_names, collapse = ", "),
    call. = FALSE
  )
}

collapse_unique <- function(x, separator = " || ") {
  x <- unique(trimws(as.character(x)))
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0L) "" else paste(x, collapse = separator)
}

session_pft_before_bridge <- if (
  exists("pftname", envir = .GlobalEnv, inherits = FALSE)
) {
  get("pftname", envir = .GlobalEnv, inherits = FALSE)
} else {
  NULL
}

session_ma_qc_before_bridge <- if (
  exists("ma_qc", envir = .GlobalEnv, inherits = FALSE)
) {
  get("ma_qc", envir = .GlobalEnv, inherits = FALSE)
} else {
  NULL
}

pftname <- trimws(as.character(PFT_NAME))

if (length(pftname) != 1L || is.na(pftname) || !nzchar(pftname)) {
  stop("Please set one valid PFT_NAME at the top of the script.", call. = FALSE)
}

if (grepl("[/\\\\]", pftname)) {
  stop("PFT_NAME cannot contain / or \\\\.", call. = FALSE)
}

n_bridge_draws <- check_positive_integer(N_BRIDGE_DRAWS, "N_BRIDGE_DRAWS")
bridge_seed <- check_positive_integer(BRIDGE_SEED, "BRIDGE_SEED")

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("R package `data.table` is required.", call. = FALSE)
}

if (!dir.exists(CODE_DIR)) {
  stop("CODE_DIR does not exist: ", CODE_DIR, call. = FALSE)
}

code_dir <- normalizePath(CODE_DIR, mustWork = TRUE)

required_source_files <- c(
  "pecan_to_sipnet.R",
  "sipnet_writer_unit_contract.R"
)

missing_source_files <- required_source_files[
  !file.exists(file.path(code_dir, required_source_files))
]

if (length(missing_source_files) > 0L) {
  stop(
    "Missing source files in CODE_DIR: ",
    paste(missing_source_files, collapse = ", "),
    call. = FALSE
  )
}

# Do not source pecan_traits.R: it can overwrite sipnet_writer_traits_v2().
# pecan_to_sipnet.R currently contains the bridge functions used below.
for (source_file in required_source_files) {
  sys.source(file.path(code_dir, source_file), envir = .GlobalEnv)
}

required_functions <- c(
  "extract_ma_beta_draws",
  "bridge_md_posteriors_to_pecan",
  "md_to_pecan_bridge_registry",
  "all_enabled_md_bridge_context",
  "all_enabled_md_bridge_models",
  "sipnet_writer_unit_contract_v2",
  "validate_sipnet_writer_unit_contract"
)

missing_functions <- required_functions[
  !vapply(required_functions, exists, logical(1), mode = "function")
]

if (length(missing_functions) > 0L) {
  stop(
    "Required bridge functions were not loaded: ",
    paste(missing_functions, collapse = ", "),
    call. = FALSE
  )
}


# =============================================================================
# 3. Input and output paths
# =============================================================================

dir.create(OUTPUT_ROOT, recursive = TRUE, showWarnings = FALSE)

if (!dir.exists(OUTPUT_ROOT)) {
  stop("Cannot create OUTPUT_ROOT: ", OUTPUT_ROOT, call. = FALSE)
}

output_root <- normalizePath(OUTPUT_ROOT, mustWork = TRUE)
pipeline_out_dir <- file.path(output_root, pftname)
prep_dir <- file.path(pipeline_out_dir, "01_preparation")
qc_dir <- file.path(pipeline_out_dir, "03_ma_qc")
bridge_dir <- file.path(pipeline_out_dir, "04_pecan_traits_21")

trait_mcmc_pass_file <- file.path(
  qc_dir,
  "trait.mcmc.QC_PASS.Rdata"
)

qc_summary_file <- file.path(qc_dir, "ma_qc_summary.csv")
unit_map_file <- file.path(prep_dir, "unit_map.rds")

dir.create(bridge_dir, recursive = TRUE, showWarnings = FALSE)

message("============================================================")
message("QC PASS posterior -> 21 PEcAn traits")
message("PFT: ", pftname)
message("PASS posterior: ", trait_mcmc_pass_file)
message("Output: ", bridge_dir)
message("============================================================")


# =============================================================================
# 4. Load QC PASS posterior and perform a second PASS-only check
# =============================================================================

use_in_memory_qc <- isTRUE(USE_IN_MEMORY_QC) &&
  !is.null(session_pft_before_bridge) &&
  identical(trimws(as.character(session_pft_before_bridge)), pftname) &&
  is.list(session_ma_qc_before_bridge) &&
  is.list(session_ma_qc_before_bridge$ma_result_qc) &&
  is.list(session_ma_qc_before_bridge$ma_result_qc$trait.mcmc)

if (use_in_memory_qc) {
  trait_mcmc_pass <- session_ma_qc_before_bridge$ma_result_qc$trait.mcmc
  qc_input_source <- "IN_MEMORY_ma_qc$ma_result_qc"
  message("Using in-memory QC PASS posterior from ma_qc$ma_result_qc.")
} else {
  trait_mcmc_pass <- load_rdata_object(
    trait_mcmc_pass_file,
    preferred_names = c(
      "trait.mcmc",
      "trait.mcmc.QC_PASS",
      "trait_mcmc",
      "trait_mcmc_pass"
    )
  )
  qc_input_source <- normalizePath(trait_mcmc_pass_file, mustWork = TRUE)
  message("Using saved QC PASS posterior: ", qc_input_source)
}

if (
  !is.list(trait_mcmc_pass) ||
  is.null(names(trait_mcmc_pass)) ||
  any(!nzchar(names(trait_mcmc_pass)))
) {
  stop("QC PASS trait.mcmc must be a named list.", call. = FALSE)
}

if (file.exists(qc_summary_file)) {
  qc_summary <- data.table::fread(qc_summary_file)
  required_qc_columns <- c("trait", "status")
  missing_qc_columns <- setdiff(required_qc_columns, names(qc_summary))
  
  if (length(missing_qc_columns) > 0L) {
    stop(
      "ma_qc_summary.csv is missing columns: ",
      paste(missing_qc_columns, collapse = ", "),
      call. = FALSE
    )
  }
  
  qc_summary[, trait := as.character(trait)]
  qc_summary[, status := toupper(trimws(as.character(status)))]
  pass_names_from_summary <- qc_summary[status == "PASS", trait]
  
  unexpected_nonpass <- setdiff(
    names(trait_mcmc_pass),
    pass_names_from_summary
  )
  
  if (length(unexpected_nonpass) > 0L) {
    warning(
      "Removing posterior objects not marked PASS in ma_qc_summary.csv: ",
      paste(unexpected_nonpass, collapse = ", "),
      call. = FALSE
    )
  }
  
  trait_mcmc_pass <- trait_mcmc_pass[
    intersect(names(trait_mcmc_pass), pass_names_from_summary)
  ]
}

if (length(trait_mcmc_pass) == 0L) {
  stop("No QC PASS posterior remains for the bridge.", call. = FALSE)
}

ma_result_pass <- list(trait.mcmc = trait_mcmc_pass)


# =============================================================================
# 5. Sample beta.o draws and run the bridge
# =============================================================================

ma_draws_pass <- extract_ma_beta_draws(
  ma_result = ma_result_pass,
  n_draws = n_bridge_draws,
  seed = bridge_seed
)

# Keep the repository's current all-enabled behavior requested for this stage,
# but explicitly disable the removed soilWHC approximation.
bridge_context_21 <- all_enabled_md_bridge_context(
  pftname = pftname,
  context_overrides = list(
    allow_root_depth_soilwhc_approx = FALSE
  )
)

bridge_models_21 <- all_enabled_md_bridge_models()
bridge_models_21 <- bridge_models_21[
  intersect(names(bridge_models_21), PECAN_TARGETS_21)
]

bridge_result <- bridge_md_posteriors_to_pecan(
  ma_draws = ma_draws_pass,
  context = bridge_context_21,
  models = bridge_models_21,
  registry = md_to_pecan_bridge_registry(),
  strict = FALSE
)

# The raw output below is forcibly projected onto the allowlist. Even if a
# direct soilWHC or AmaxFrac posterior exists, it cannot enter saved outputs.
pecan_draws_21_before_output_qc <- data.table::data.table(
  draw_id = ma_draws_pass$draw_id
)

for (target_i in PECAN_TARGETS_21) {
  value_i <- if (target_i %in% names(bridge_result$candidate_trait_draws)) {
    suppressWarnings(as.numeric(
      bridge_result$candidate_trait_draws[[target_i]]
    ))
  } else {
    rep(NA_real_, nrow(ma_draws_pass))
  }
  
  data.table::set(
    pecan_draws_21_before_output_qc,
    j = target_i,
    value = value_i
  )
}

expected_draw_columns <- c("draw_id", PECAN_TARGETS_21)

if (!identical(names(pecan_draws_21_before_output_qc), expected_draw_columns)) {
  stop("Internal error: bridge output is not exactly the 21-target schema.", call. = FALSE)
}


# =============================================================================
# 6. Bridge output unit validation
# =============================================================================
#
# What this validation does:
#   1. verifies the complete 54-row SIPNET writer contract is internally valid;
#   2. verifies every allowed PEcAn trait occurs exactly once in that contract;
#   3. verifies the unit declared by this bridge equals the unit expected by
#      write.config.SIPNET for that PEcAn trait.
#
# What it does NOT do:
#   It does not prove that every numeric formula is dimensionally correct.
#   Formula/source provenance and output-domain QC are saved separately.
# =============================================================================

bridge_output_units_21 <- data.table::data.table(
  pecan_trait = PECAN_TARGETS_21,
  bridge_output_unit = c(
    "m2 leaf kg-1 leaf dry mass",
    "percent leaf dry mass",
    "g C g-1 N",
    "g C g-1 N",
    "g C g-1 N",
    "umol CO2 m-2 leaf s-1",
    "umol CO2 m-2 leaf s-1",
    "fraction",
    "mol photons m-2 ground day-1",
    "kPa-1 by official convention",
    "year-1",
    "year-1",
    "year-1",
    "year-1",
    "mg CO2 kPa g-1 H2O",
    "unitless Q10",
    "unitless Q10",
    "unitless Q10",
    "umol CO2 kg-1 s-1",
    "umol CO2 kg-1 s-1",
    "day-1"
  )
)

normalize_unit_text <- function(x) {
  tolower(gsub("[[:space:]]+", " ", trimws(as.character(x))))
}

writer_contract_full <- sipnet_writer_unit_contract_v2()

writer_contract_validation <- validate_sipnet_writer_unit_contract(
  writer_contract_full,
  stop_on_error = TRUE
)

writer_contract_21 <- data.table::as.data.table(writer_contract_full)[
  match(PECAN_TARGETS_21, pecan_trait)
]

if (
  nrow(writer_contract_21) != 21L ||
  anyNA(writer_contract_21$pecan_trait) ||
  anyDuplicated(writer_contract_21$pecan_trait) ||
  !identical(as.character(writer_contract_21$pecan_trait), PECAN_TARGETS_21)
) {
  stop(
    "The SIPNET writer contract does not contain exactly one row for each allowed target.",
    call. = FALSE
  )
}

unit_validation_21 <- merge(
  bridge_output_units_21,
  writer_contract_21[, .(
    pecan_trait,
    writer_expected_pecan_unit = pecan_input_unit,
    sipnet_parameter,
    sipnet_file_unit,
    sipnet_internal_unit,
    writer_transformation_class = transformation_class,
    writer_formula,
    writer_expected_domain = expected_domain,
    official_unit_source,
    writer_audit_status = audit_status
  )],
  by = "pecan_trait",
  all.x = TRUE,
  sort = FALSE
)

unit_validation_21[, target_order := match(pecan_trait, PECAN_TARGETS_21)]
data.table::setorder(unit_validation_21, target_order)

unit_validation_21[, unit_match :=
                     normalize_unit_text(bridge_output_unit) ==
                     normalize_unit_text(writer_expected_pecan_unit)
]

unit_validation_21[, unit_validation_status := data.table::fcase(
  is.na(writer_expected_pecan_unit), "MISSING_WRITER_CONTRACT",
  unit_match, "MATCH",
  default = "MISMATCH"
)]

if (!all(unit_validation_21$unit_match)) {
  bad_units <- unit_validation_21[unit_match != TRUE, pecan_trait]
  stop(
    "Bridge output unit mismatch for: ",
    paste(bad_units, collapse = ", "),
    call. = FALSE
  )
}


# =============================================================================
# 7. Output-domain QC; invalid finite draws become NA
# =============================================================================

valid_pecan_domain <- function(trait_name, value) {
  value <- suppressWarnings(as.numeric(value))
  finite <- is.finite(value)
  valid <- rep(FALSE, length(value))
  
  valid[finite] <- switch(
    trait_name,
    SLA = value[finite] > 0,
    leafC = value[finite] >= 0 & value[finite] <= 100,
    c2n_leaf = value[finite] > 0,
    c2n_fineroot = value[finite] > 0,
    c2n_wood = value[finite] > 0,
    Amax = value[finite] > 0,
    leaf_respiration_rate_m2 = value[finite] >= 0,
    growth_resp_factor = value[finite] >= 0 & value[finite] <= 1,
    half_saturation_PAR = value[finite] > 0,
    dVPDSlope = value[finite] >= 0,
    leaf_turnover_rate = value[finite] >= 0,
    root_turnover_rate = value[finite] >= 0,
    wood_turnover_rate = value[finite] >= 0,
    turn_over_time = value[finite] >= 0,
    wueConst = value[finite] > 0,
    veg_respiration_Q10 = value[finite] > 0,
    fine_root_respiration_Q10 = value[finite] > 0,
    coarse_root_respiration_Q10 = value[finite] > 0,
    root_respiration_rate = value[finite] >= 0,
    stem_respiration_rate = value[finite] >= 0,
    waterRemoveFrac = value[finite] >= 0 & value[finite] <= 1,
    stop("No output-domain rule for trait: ", trait_name, call. = FALSE)
  )
  
  valid
}

pecan_trait_draws_21 <- data.table::copy(
  pecan_draws_21_before_output_qc
)

output_qc_rows <- vector("list", length(PECAN_TARGETS_21))

for (target_index in seq_along(PECAN_TARGETS_21)) {
  target_i <- PECAN_TARGETS_21[[target_index]]
  raw_value <- pecan_draws_21_before_output_qc[[target_i]]
  finite_before <- is.finite(raw_value)
  domain_valid <- valid_pecan_domain(target_i, raw_value)
  invalid_domain <- finite_before & !domain_valid
  
  clean_value <- raw_value
  clean_value[invalid_domain] <- NA_real_
  data.table::set(pecan_trait_draws_21, j = target_i, value = clean_value)
  
  finite_after <- is.finite(clean_value)
  n_total <- length(clean_value)
  n_finite_after <- sum(finite_after)
  
  target_status <- if (n_finite_after == n_total) {
    "COMPLETE"
  } else if (n_finite_after == 0L) {
    "MISSING"
  } else {
    "PARTIAL"
  }
  
  quantiles <- if (n_finite_after > 0L) {
    stats::quantile(
      clean_value[finite_after],
      probs = c(0.025, 0.5, 0.975),
      names = FALSE,
      na.rm = TRUE
    )
  } else {
    rep(NA_real_, 3L)
  }
  
  output_qc_rows[[target_index]] <- data.table::data.table(
    pecan_trait = target_i,
    n_draws = n_total,
    n_finite_before_output_qc = sum(finite_before),
    n_invalid_domain = sum(invalid_domain),
    n_finite_after_output_qc = n_finite_after,
    n_missing_after_output_qc = n_total - n_finite_after,
    posterior_q025 = quantiles[[1L]],
    posterior_median = quantiles[[2L]],
    posterior_q975 = quantiles[[3L]],
    bridge_output_status = target_status,
    writer_ready = identical(target_status, "COMPLETE")
  )
}

output_qc_21 <- data.table::rbindlist(output_qc_rows)

if (!identical(names(pecan_trait_draws_21), expected_draw_columns)) {
  stop("Internal error: final draw table is not exactly the 21-target schema.", call. = FALSE)
}


# =============================================================================
# 8. Provenance and source canonical-unit metadata
# =============================================================================

rule_attempts_21 <- data.table::as.data.table(
  data.table::copy(bridge_result$rule_attempts)
)[target_pecan_trait %in% PECAN_TARGETS_21]

successful_attempts_21 <- rule_attempts_21[
  is.finite(n_filled) & n_filled > 0L
]

if (nrow(successful_attempts_21) > 0L) {
  provenance_21 <- successful_attempts_21[
    ,
    .(
      bridge_source_traits = collapse_unique(source_traits),
      bridge_conversion_classes = collapse_unique(conversion_class),
      bridge_formulas = collapse_unique(formula),
      bridge_notes = collapse_unique(note),
      uses_user_supplied_model = any(conversion_class == "USER_SUPPLIED_MODEL")
    ),
    by = target_pecan_trait
  ]
  data.table::setnames(provenance_21, "target_pecan_trait", "pecan_trait")
} else {
  provenance_21 <- data.table::data.table(
    pecan_trait = character(),
    bridge_source_traits = character(),
    bridge_conversion_classes = character(),
    bridge_formulas = character(),
    bridge_notes = character(),
    uses_user_supplied_model = logical()
  )
}

source_unit_lookup <- data.table::data.table(
  pecan_vname = character(),
  canonical_unit = character()
)

if (file.exists(unit_map_file)) {
  unit_map_saved <- data.table::as.data.table(readRDS(unit_map_file))
  
  if (all(c("pecan_vname", "canonical_unit") %in% names(unit_map_saved))) {
    source_unit_lookup <- unique(unit_map_saved[, .(
      pecan_vname = as.character(pecan_vname),
      canonical_unit = as.character(canonical_unit)
    )])
  }
}

source_unit_label <- function(source_text) {
  if (length(source_text) == 0L || is.na(source_text) || !nzchar(source_text)) {
    return("")
  }
  
  source_tokens <- unique(trimws(unlist(strsplit(source_text, "\\|"))))
  source_tokens <- source_tokens[nzchar(source_tokens)]
  
  labels <- vapply(source_tokens, function(source_i) {
    source_unit <- source_unit_lookup[
      pecan_vname == source_i,
      canonical_unit
    ]
    
    if (length(source_unit) == 0L && source_i %in% writer_contract_21$pecan_trait) {
      source_unit <- writer_contract_21[
        pecan_trait == source_i,
        pecan_input_unit
      ]
    }
    
    if (identical(source_i, "user_model")) {
      source_unit <- "see model formula and context"
    }
    
    source_unit <- unique(source_unit[!is.na(source_unit) & nzchar(source_unit)])
    if (length(source_unit) == 0L) source_unit <- "unit not registered"
    
    paste0(source_i, " [", paste(source_unit, collapse = " | "), "]")
  }, character(1))
  
  paste(labels, collapse = " | ")
}


# =============================================================================
# 9. Build and save 21-trait metadata + front-end products
# =============================================================================

metadata_21 <- merge(
  unit_validation_21,
  output_qc_21,
  by = "pecan_trait",
  all.x = TRUE,
  sort = FALSE
)

metadata_21 <- merge(
  metadata_21,
  provenance_21,
  by = "pecan_trait",
  all.x = TRUE,
  sort = FALSE
)

metadata_21[, target_order := match(pecan_trait, PECAN_TARGETS_21)]
data.table::setorder(metadata_21, target_order)

character_metadata_columns <- c(
  "bridge_source_traits",
  "bridge_conversion_classes",
  "bridge_formulas",
  "bridge_notes"
)

for (column_i in character_metadata_columns) {
  metadata_21[is.na(get(column_i)), (column_i) := ""]
}

metadata_21[
  is.na(uses_user_supplied_model),
  uses_user_supplied_model := FALSE
]

metadata_21[, source_canonical_units := vapply(
  bridge_source_traits,
  source_unit_label,
  character(1)
)]

metadata_21[, `:=`(
  pft = pftname,
  posterior_qc_input = "PASS_ONLY",
  posterior_qc_source = qc_input_source,
  excluded_targets = paste(EXCLUDED_TARGETS, collapse = "|"),
  bridge_seed = bridge_seed,
  bridge_draw_count = n_bridge_draws
)]

data.table::setcolorder(
  metadata_21,
  c(
    "target_order", "pft", "pecan_trait", "posterior_qc_input",
    "posterior_qc_source",
    "bridge_output_status", "writer_ready", "bridge_output_unit",
    "writer_expected_pecan_unit", "unit_validation_status",
    setdiff(names(metadata_21), c(
      "target_order", "pft", "pecan_trait", "posterior_qc_input",
      "posterior_qc_source",
      "bridge_output_status", "writer_ready", "bridge_output_unit",
      "writer_expected_pecan_unit", "unit_validation_status"
    ))
  )
)

complete_traits_21 <- metadata_21[
  writer_ready == TRUE,
  pecan_trait
]

pecan_trait_draws_complete <- pecan_trait_draws_21[
  ,
  c("draw_id", complete_traits_21),
  with = FALSE
]

unresolved_21 <- data.table::as.data.table(
  data.table::copy(bridge_result$unresolved)
)[candidate_pecan_trait %in% PECAN_TARGETS_21]

bundle_21 <- list(
  schema_version = "1.0.0",
  created_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  pft = pftname,
  posterior_qc_input = "PASS_ONLY",
  posterior_qc_source = qc_input_source,
  target_allowlist = PECAN_TARGETS_21,
  excluded_targets = EXCLUDED_TARGETS,
  n_pass_ma_traits = length(trait_mcmc_pass),
  pass_ma_traits = names(trait_mcmc_pass),
  n_bridge_draws = n_bridge_draws,
  bridge_seed = bridge_seed,
  complete_traits = complete_traits_21,
  missing_or_partial_traits = setdiff(PECAN_TARGETS_21, complete_traits_21),
  unit_contract_validation = writer_contract_validation,
  pecan_trait_draws_21 = pecan_trait_draws_21,
  metadata_21 = metadata_21,
  unit_contract_21 = writer_contract_21,
  rule_attempts_21 = rule_attempts_21,
  unresolved_21 = unresolved_21,
  bridge_context = bridge_context_21,
  bridge_model_targets = names(bridge_models_21)
)

saveRDS(
  pecan_draws_21_before_output_qc,
  file.path(bridge_dir, "pecan_trait_draws_21_before_output_qc.rds"),
  compress = FALSE
)

saveRDS(
  pecan_trait_draws_21,
  file.path(bridge_dir, "pecan_trait_draws_21.rds"),
  compress = FALSE
)

saveRDS(
  metadata_21,
  file.path(bridge_dir, "pecan_trait_metadata_21.rds"),
  compress = FALSE
)

saveRDS(
  writer_contract_21,
  file.path(bridge_dir, "sipnet_writer_unit_contract_21.rds"),
  compress = FALSE
)

saveRDS(
  bundle_21,
  file.path(bridge_dir, "pecan_traits_21_bundle.rds"),
  compress = FALSE
)

data.table::fwrite(
  pecan_trait_draws_21,
  file.path(bridge_dir, "pecan_trait_draws_21.csv")
)

data.table::fwrite(
  pecan_trait_draws_complete,
  file.path(bridge_dir, "pecan_trait_draws_complete.csv")
)

data.table::fwrite(
  metadata_21,
  file.path(bridge_dir, "pecan_trait_metadata_21.csv")
)

data.table::fwrite(
  unit_validation_21,
  file.path(bridge_dir, "bridge_unit_validation_21.csv")
)

data.table::fwrite(
  writer_contract_21,
  file.path(bridge_dir, "sipnet_writer_unit_contract_21.csv")
)

data.table::fwrite(
  rule_attempts_21,
  file.path(bridge_dir, "bridge_rule_attempts_21.csv")
)

data.table::fwrite(
  unresolved_21,
  file.path(bridge_dir, "bridge_unresolved_21.csv")
)

if (isTRUE(WRITE_LONG_CSV)) {
  pecan_trait_draws_21_long <- data.table::melt(
    pecan_trait_draws_21,
    id.vars = "draw_id",
    measure.vars = PECAN_TARGETS_21,
    variable.name = "pecan_trait",
    value.name = "value",
    variable.factor = FALSE
  )
  
  data.table::fwrite(
    pecan_trait_draws_21_long,
    file.path(bridge_dir, "pecan_trait_draws_21_long.csv")
  )
}

if (requireNamespace("jsonlite", quietly = TRUE)) {
  metadata_records <- lapply(seq_len(nrow(metadata_21)), function(row_i) {
    as.list(metadata_21[row_i])
  })
  
  metadata_payload <- list(
    schema_version = "1.0.0",
    pft = pftname,
    target_count = 21L,
    target_allowlist = PECAN_TARGETS_21,
    excluded_targets = EXCLUDED_TARGETS,
    draw_file_wide = "pecan_trait_draws_21.csv",
    draw_file_long = if (isTRUE(WRITE_LONG_CSV)) {
      "pecan_trait_draws_21_long.csv"
    } else {
      NULL
    },
    records = metadata_records
  )
  
  jsonlite::write_json(
    metadata_payload,
    file.path(bridge_dir, "pecan_trait_metadata_21.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    na = "null"
  )
} else {
  warning(
    "Package `jsonlite` is not installed; CSV and RDS were saved, JSON was skipped.",
    call. = FALSE
  )
}


# =============================================================================
# 10. Final gate and summary
# =============================================================================

missing_or_partial_21 <- setdiff(PECAN_TARGETS_21, complete_traits_21)

message("============================================================")
message("21-target bridge finished for: ", pftname)
message("QC PASS MA traits used: ", length(trait_mcmc_pass))
message("Allowed PEcAn targets: 21")
message("Complete PEcAn targets: ", length(complete_traits_21))
message("Missing/partial targets: ", length(missing_or_partial_21))

if (length(missing_or_partial_21) > 0L) {
  message("Missing/partial names: ", paste(missing_or_partial_21, collapse = ", "))
}

message("Unit contract matches: ", sum(unit_validation_21$unit_match), "/21")
message("Output directory: ", bridge_dir)
message("============================================================")

print(
  metadata_21[, .(
    target_order,
    pecan_trait,
    bridge_output_status,
    n_invalid_domain,
    unit_validation_status,
    writer_ready
  )],
  nrows = Inf
)

if (isTRUE(REQUIRE_ALL_21) && length(missing_or_partial_21) > 0L) {
  stop(
    "Audit files were saved, but REQUIRE_ALL_21=TRUE and these targets are not complete: ",
    paste(missing_or_partial_21, collapse = ", "),
    call. = FALSE
  )
}

if (!isTRUE(REQUIRE_ALL_21) && length(missing_or_partial_21) > 0L) {
  warning(
    "The 21-column schema was saved, but missing/partial targets contain NA. See metadata.",
    call. = FALSE
  )
}
