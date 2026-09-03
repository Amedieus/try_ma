# =============================================================================
# RStudio caller: TRY -> 21 PEcAn target observations -> random-effect MA
#                 -> QC -> PEcAn samples (no pecan.xml is read)
# =============================================================================

options(stringsAsFactors = FALSE)


# -----------------------------------------------------------------------------
# USER INPUTS: edit these paths for a normal run
# -----------------------------------------------------------------------------

PFT_NAME <- "Evergreen_Broadleaf_Forest"
PFT_NAME <- trimws(PFT_NAME)

OUTPUT_DIR <- file.path(
  "/projectnb/dietzelab/guYANG/TRY_meta_analysis",
  PFT_NAME,
  "prema_pecan_random_site"
)

PFTSPECIES_RDATA <- paste0(
  "/projectnb/dietzelab/guYANG/TRY_meta_analysis/",
  "pftspecies.RData"
)

TRYDAT_USE_SPECIES_RDATA <- paste0(
  "/projectnb/dietzelab/guYANG/TRY_meta_analysis/",
  "trydat_use_species.RData"
)

# Reference sites carrying final_pft plus latitude/longitude. Ambiguous TRY
# species are assigned observation-by-observation to the nearest reference
# point among that species' candidate PFTs.
PFT_COORDINATE_MAP_FILE <- paste0(
  "/projectnb/dietzelab/guYANG/",
  "SIPNET_Model_Calibration/Final_PFT_assignment_v4/",
  "final_8000_sites_with_final_pft_v4.csv"
)

# This optional file is only one possible coordinate source. The caller first
# checks the current RStudio session for `try_observation_coordinates` or
# `na_species_res$observation_coordinates`, then this file, and finally the
# Latitude/Longitude columns already present in trydat_use_species.
TRY_OBSERVATION_COORDINATE_FILE <- file.path(
  "/projectnb/dietzelab/guYANG/TRY_meta_analysis",
  "try_observation_coordinates.rds"
)


# -----------------------------------------------------------------------------
# Optional run controls
# -----------------------------------------------------------------------------

MA_ITERATIONS <- 3000L
MA_WORKERS <- 2L
N_OUTPUT_DRAWS <- 1000L
RANDOM_SEED <- 20260903L
RESUME_EXISTING_TRAIT_RUNS <- TRUE
PFT_MAX_DISTANCE_KM <- 250

# `new_site_predictive` exports beta.o + a newly sampled site effect whenever
# sd.site passed its own convergence checks. Use `global_mean` only when a
# PFT-level mean prior without site-to-site variability is wanted.
PECAN_SAMPLE_MODE <- "new_site_predictive"


# -----------------------------------------------------------------------------
# Locate this script and source repository functions
# -----------------------------------------------------------------------------

get_rstudio_script_dir <- function() {
  frame_files <- vapply(
    sys.frames(),
    function(frame_i) {
      value <- frame_i$ofile
      if (is.null(value) || length(value) == 0L) NA_character_ else value[[1L]]
    },
    character(1)
  )
  frame_files <- frame_files[!is.na(frame_files) & nzchar(frame_files)]
  if (length(frame_files) > 0L) {
    return(dirname(normalizePath(
      frame_files[[length(frame_files)]],
      mustWork = TRUE
    )))
  }

  if (
    requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()
  ) {
    active_path <- tryCatch(
      rstudioapi::getSourceEditorContext()$path,
      error = function(e) ""
    )
    if (length(active_path) == 1L && nzchar(active_path)) {
      return(dirname(normalizePath(active_path, mustWork = TRUE)))
    }
  }

  normalizePath(getwd(), mustWork = TRUE)
}


CODE_DIR <- get_rstudio_script_dir()

SOURCE_FILES <- c(
  "ma_functions.R",
  "functions_for_try_data.R",
  "generate_prior_distns.R",
  "run_ma_parallel.R",
  "pecan_to_sipnet.R",
  "sipnet_writer_unit_contract.R",
  "prema_pecan_trait_ma_functions.R"
)

missing_source_files <- SOURCE_FILES[
  !file.exists(file.path(CODE_DIR, SOURCE_FILES))
]
if (length(missing_source_files) > 0L) {
  stop(
    "Cannot find repository source files in CODE_DIR `", CODE_DIR, "`: ",
    paste(missing_source_files, collapse = ", "),
    "\nOpen this caller from the try_ma repository and use RStudio Source.",
    call. = FALSE
  )
}

for (source_file in SOURCE_FILES) {
  sys.source(file.path(CODE_DIR, source_file), envir = .GlobalEnv)
}


# -----------------------------------------------------------------------------
# Validate the user inputs
# -----------------------------------------------------------------------------

if (length(PFT_NAME) != 1L || is.na(PFT_NAME) || !nzchar(trimws(PFT_NAME))) {
  stop("PFT_NAME must be one non-empty string.", call. = FALSE)
}
if (length(OUTPUT_DIR) != 1L || is.na(OUTPUT_DIR) || !nzchar(OUTPUT_DIR)) {
  stop("OUTPUT_DIR must be one non-empty path.", call. = FALSE)
}
required_input_files <- c(
  PFTSPECIES_RDATA,
  TRYDAT_USE_SPECIES_RDATA,
  PFT_COORDINATE_MAP_FILE
)
for (input_file in required_input_files) {
  if (!file.exists(input_file)) {
    stop("Cannot find required input file: ", input_file, call. = FALSE)
  }
}

# Prefer an observation-coordinate table that already exists in this RStudio
# session. This is useful immediately after the PFT mapping code has produced
# `na_species_res$observation_coordinates`; saving an intermediate RDS is then
# optional.
TRY_OBSERVATION_COORDINATE_DATA <- NULL
TRY_OBSERVATION_COORDINATE_SOURCE <- "TRY_INPUT_COLUMNS_ONLY"

if (
  exists(
    "try_observation_coordinates",
    envir = .GlobalEnv,
    inherits = FALSE
  )
) {
  TRY_OBSERVATION_COORDINATE_DATA <- get(
    "try_observation_coordinates",
    envir = .GlobalEnv,
    inherits = FALSE
  )
  TRY_OBSERVATION_COORDINATE_SOURCE <-
    "R_OBJECT:try_observation_coordinates"
} else if (
  exists("na_species_res", envir = .GlobalEnv, inherits = FALSE)
) {
  na_species_res_current <- get(
    "na_species_res",
    envir = .GlobalEnv,
    inherits = FALSE
  )
  if (
    is.list(na_species_res_current) &&
    !is.null(na_species_res_current$observation_coordinates)
  ) {
    TRY_OBSERVATION_COORDINATE_DATA <-
      na_species_res_current$observation_coordinates
    TRY_OBSERVATION_COORDINATE_SOURCE <-
      "R_OBJECT:na_species_res$observation_coordinates"
  }
}

TRY_OBSERVATION_COORDINATE_FILE_USE <- NULL
coordinate_file_requested <-
  length(TRY_OBSERVATION_COORDINATE_FILE) == 1L &&
  !is.na(TRY_OBSERVATION_COORDINATE_FILE) &&
  nzchar(trimws(TRY_OBSERVATION_COORDINATE_FILE))

if (is.null(TRY_OBSERVATION_COORDINATE_DATA) &&
    coordinate_file_requested &&
    file.exists(TRY_OBSERVATION_COORDINATE_FILE)) {
  TRY_OBSERVATION_COORDINATE_FILE_USE <-
    TRY_OBSERVATION_COORDINATE_FILE
  TRY_OBSERVATION_COORDINATE_SOURCE <- paste0(
    "FILE:",
    TRY_OBSERVATION_COORDINATE_FILE
  )
} else if (
  is.null(TRY_OBSERVATION_COORDINATE_DATA) &&
  coordinate_file_requested &&
  !file.exists(TRY_OBSERVATION_COORDINATE_FILE)
) {
  message(
    "Optional coordinate file was not found: ",
    TRY_OBSERVATION_COORDINATE_FILE,
    "\nThe pipeline will check TRY's own Latitude/Longitude columns. ",
    "Ambiguous observations still lacking coordinates will be excluded ",
    "and audited; the run stops only if no target-PFT observations remain."
  )
}


# -----------------------------------------------------------------------------
# Run the complete pipeline
# -----------------------------------------------------------------------------

message("============================================================")
message("PFT: ", PFT_NAME)
message("Output: ", OUTPUT_DIR)
message("Random site effects: TRUE")
message("PEcAn export mode: ", PECAN_SAMPLE_MODE)
message("TRY observation coordinate source: ",
        TRY_OBSERVATION_COORDINATE_SOURCE)
message("No pecan.xml will be read.")
message("============================================================")

prema_result <- run_prema_pecan_trait_ma(
  pft_name = PFT_NAME,
  output_dir = OUTPUT_DIR,
  pftspecies_rdata = PFTSPECIES_RDATA,
  trydat_use_species_rdata = TRYDAT_USE_SPECIES_RDATA,
  pft_coordinate_map_file = PFT_COORDINATE_MAP_FILE,
  observation_coordinate_file = TRY_OBSERVATION_COORDINATE_FILE_USE,
  observation_coordinate_data = TRY_OBSERVATION_COORDINATE_DATA,
  iterations = MA_ITERATIONS,
  workers = MA_WORKERS,
  n_output_draws = N_OUTPUT_DRAWS,
  random = TRUE,
  seed = RANDOM_SEED,
  include_site_variability_in_samples = TRUE,
  sample_mode = PECAN_SAMPLE_MODE,
  unsupported_unit_action = "stop",
  max_pft_distance_km = PFT_MAX_DISTANCE_KM,
  resume = RESUME_EXISTING_TRAIT_RUNS
)


# -----------------------------------------------------------------------------
# Console summary. The full result remains available as `prema_result`.
# -----------------------------------------------------------------------------

print(prema_result$pipeline_summary)

print(
  prema_result$posterior_samples_21$metadata_21[, .(
    target_order,
    pecan_trait,
    pecan_unit,
    ma_qc_status,
    site_variability_status,
    site_variability_used_in_predictive_draws,
    output_source,
    writer_ready
  )],
  nrows = Inf
)

message(
  "\nOpen in RStudio with:",
  "\nView(prema_result$site_variability_summary)",
  "\nView(prema_result$posterior_samples_21$metadata_21)",
  "\n\nMain random-effect MA result:",
  "\n", file.path(OUTPUT_DIR, "03_meta_analysis", "ma_result.rds"),
  "\n\nPEcAn sample bundle (21 traits; no XML):",
  "\n", file.path(OUTPUT_DIR, "05_pecan_samples", "samples.Rdata")
)
