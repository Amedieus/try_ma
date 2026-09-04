options(stringsAsFactors = FALSE)

NO_XML_EXPORT_SCRIPT_VERSION <- "2026-09-03.1"


# =============================================================================
# RStudio: bridge output -> PEcAn samples + SIPNET trait.values (NO XML)
# =============================================================================
#
# This script never reads or requests pecan.xml and never creates a PEcAn
# settings object. It only converts the completed single-PFT bridge bundle into
# two downstream-native R structures:
#
# 1. PEcAn samples.Rdata
#      trait.samples
#      ensemble.samples
#      sa.samples
#      runs.samples
#      env.samples
#
# 2. SIPNET-writer trait.values objects
#      trait.values.by.draw[[ensemble_member]]
#
# It does not write sipnet.param or sipnet.in. Those files can only be created
# later by a PEcAn/SIPNET run that already has its own settings, run directory,
# model templates, and environmental inputs.
#
# RStudio use
# -----------
# 1. Edit section 0.
# 2. Click Source.
# =============================================================================


# =============================================================================
# 0. User configuration: no XML setting exists anywhere in this script
# =============================================================================

# Directory name used by the TRY meta-analysis and bridge output.
PFT_NAME <- "Evergreen_Broadleaf_Forest"

# Name to use inside the exported PEcAn/SIPNET objects. This must match the PFT
# name used by the future PEcAn workflow. It does not come from an XML here.
WORKFLOW_PFT_NAME <- "Evergreen_Broadleaf_Forest"

OUTPUT_ROOT <- paste0(
  "/projectnb/dietzelab/guYANG/",
  "TRY_meta_analysis"
)

N_EXPORT_DRAWS <- 100L
EXPORT_SEED <- 20260903L

# A context-only Q10=2 is a model default, not a TRY MA posterior. When the
# PASS MA set has no leaf_respiration_Q10_md, omit all three Q10 targets so the
# later SIPNET writer retains its own template/default Q10 parameters.
EXCLUDE_Q10_WHEN_NO_PASS_Q10_POSTERIOR <- TRUE


# =============================================================================
# 1. Input/output paths and basic checks
# =============================================================================

bridge_dir <- file.path(
  OUTPUT_ROOT,
  PFT_NAME,
  "04_pecan_traits_21"
)

bundle_file <- file.path(
  bridge_dir,
  "pecan_traits_21_bundle.rds"
)

export_dir <- file.path(
  bridge_dir,
  "pecan_sipnet_no_xml_export"
)

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("R package `data.table` is required.", call. = FALSE)
}

check_nonempty_name <- function(value, name) {
  value <- trimws(as.character(value))
  
  if (
    length(value) != 1L ||
    is.na(value) ||
    !nzchar(value)
  ) {
    stop(name, " must be one non-empty character value.", call. = FALSE)
  }
  
  value
}

check_positive_integer <- function(value, name) {
  value_numeric <- suppressWarnings(as.numeric(value))
  
  if (
    length(value_numeric) != 1L ||
    is.na(value_numeric) ||
    !is.finite(value_numeric) ||
    value_numeric < 1 ||
    value_numeric != round(value_numeric)
  ) {
    stop(name, " must be one positive integer.", call. = FALSE)
  }
  
  as.integer(value_numeric)
}

PFT_NAME <- check_nonempty_name(PFT_NAME, "PFT_NAME")
WORKFLOW_PFT_NAME <- check_nonempty_name(
  WORKFLOW_PFT_NAME,
  "WORKFLOW_PFT_NAME"
)
n_export_draws <- check_positive_integer(
  N_EXPORT_DRAWS,
  "N_EXPORT_DRAWS"
)
export_seed <- check_positive_integer(EXPORT_SEED, "EXPORT_SEED")

if (!file.exists(bundle_file)) {
  stop(
    "Cannot find bridge bundle: ",
    bundle_file,
    "\nRun ma_prep_with_pecan_bridge.R for this PFT first.",
    call. = FALSE
  )
}

dir.create(
  export_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# =============================================================================
# 2. Read and validate the 21-trait bridge bundle
# =============================================================================

bundle_21 <- readRDS(bundle_file)

required_bundle_elements <- c(
  "pft",
  "pass_ma_traits",
  "pecan_trait_draws_21",
  "metadata_21"
)

missing_bundle_elements <- setdiff(
  required_bundle_elements,
  names(bundle_21)
)

if (length(missing_bundle_elements) > 0L) {
  stop(
    "Bridge bundle is missing elements: ",
    paste(missing_bundle_elements, collapse = ", "),
    call. = FALSE
  )
}

if (!identical(as.character(bundle_21$pft), PFT_NAME)) {
  stop(
    "PFT mismatch: bundle contains `", bundle_21$pft,
    "`, but PFT_NAME is `", PFT_NAME, "`.",
    call. = FALSE
  )
}

draws_21 <- data.table::as.data.table(
  data.table::copy(bundle_21$pecan_trait_draws_21)
)

metadata_21 <- data.table::as.data.table(
  data.table::copy(bundle_21$metadata_21)
)

required_metadata_columns <- c(
  "pecan_trait",
  "bridge_output_status",
  "writer_ready",
  "unit_validation_status"
)

missing_metadata_columns <- setdiff(
  required_metadata_columns,
  names(metadata_21)
)

if (length(missing_metadata_columns) > 0L) {
  stop(
    "Bridge metadata is missing columns: ",
    paste(missing_metadata_columns, collapse = ", "),
    call. = FALSE
  )
}

if (!"draw_id" %in% names(draws_21)) {
  stop("Bridge draws must contain draw_id.", call. = FALSE)
}

if (anyDuplicated(draws_21$draw_id)) {
  stop("Bridge draw_id values must be unique.", call. = FALSE)
}

if (anyDuplicated(metadata_21$pecan_trait)) {
  stop("Bridge metadata contains duplicate pecan_trait names.", call. = FALSE)
}


# =============================================================================
# 3. Select complete, unit-matched traits and handle context-only Q10
# =============================================================================

export_traits <- metadata_21[
  writer_ready %in% TRUE &
    bridge_output_status == "COMPLETE" &
    unit_validation_status == "MATCH",
  as.character(pecan_trait)
]

export_traits <- unique(export_traits)

q10_targets <- c(
  "veg_respiration_Q10",
  "fine_root_respiration_Q10",
  "coarse_root_respiration_Q10"
)

has_pass_leaf_q10_posterior <-
  "leaf_respiration_Q10_md" %in%
  as.character(bundle_21$pass_ma_traits)

q10_omitted <- character()

if (
  isTRUE(EXCLUDE_Q10_WHEN_NO_PASS_Q10_POSTERIOR) &&
  !has_pass_leaf_q10_posterior
) {
  q10_omitted <- intersect(export_traits, q10_targets)
  export_traits <- setdiff(export_traits, q10_targets)
  
  message(
    "No PASS leaf_respiration_Q10_md posterior.\n",
    "Context-only Q10 values are omitted from the export."
  )
}

if (length(export_traits) == 0L) {
  stop(
    "No COMPLETE + MATCH + writer_ready traits remain after screening.",
    call. = FALSE
  )
}

missing_draw_columns <- setdiff(export_traits, names(draws_21))

if (length(missing_draw_columns) > 0L) {
  stop(
    "Bridge draw table is missing export columns: ",
    paste(missing_draw_columns, collapse = ", "),
    call. = FALSE
  )
}

for (trait_i in export_traits) {
  original_i <- draws_21[[trait_i]]
  numeric_i <- suppressWarnings(as.numeric(as.character(original_i)))
  introduced_na <- !is.na(original_i) & is.na(numeric_i)
  
  if (any(introduced_na)) {
    stop(
      "Trait ", trait_i,
      " contains values that cannot be converted to numeric.",
      call. = FALSE
    )
  }
  
  data.table::set(draws_21, j = trait_i, value = numeric_i)
}

complete_draws <- draws_21[
  ,
  c("draw_id", export_traits),
  with = FALSE
]

finite_row <- apply(
  as.matrix(complete_draws[, ..export_traits]),
  1L,
  function(value_i) all(is.finite(value_i))
)

n_nonfinite_rows_removed <- sum(!finite_row)
complete_draws <- complete_draws[finite_row]

if (nrow(complete_draws) == 0L) {
  stop("No complete finite joint bridge draws remain.", call. = FALSE)
}

if (n_export_draws > nrow(complete_draws)) {
  stop(
    "N_EXPORT_DRAWS = ", n_export_draws,
    " exceeds available complete joint draws = ", nrow(complete_draws),
    ".",
    call. = FALSE
  )
}

constant_export_traits <- export_traits[
  vapply(
    export_traits,
    function(trait_i) {
      data.table::uniqueN(complete_draws[[trait_i]]) == 1L
    },
    logical(1)
  )
]

if (length(constant_export_traits) > 0L) {
  warning(
    "These exported traits are constant across bridge draws: ",
    paste(constant_export_traits, collapse = ", "),
    ". They are retained but add no ensemble uncertainty.",
    call. = FALSE
  )
}


# =============================================================================
# 4. Select whole joint rows; never sample traits independently
# =============================================================================

set.seed(export_seed)

selected_row_index <- sample.int(
  n = nrow(complete_draws),
  size = n_export_draws,
  replace = FALSE
)

selected_draws <- data.table::copy(
  complete_draws[selected_row_index]
)

selected_draw_ids <- selected_draws$draw_id

selected_parameters <- as.data.frame(
  selected_draws[, ..export_traits],
  stringsAsFactors = FALSE
)

rownames(selected_parameters) <- NULL


# =============================================================================
# 5. Create native PEcAn samples.Rdata objects
# =============================================================================
#
# This is the five-object file contract used by PEcAn's get.parameter.samples()
# and run.write.configs(). Both trait.samples and ensemble.samples contain the
# same selected joint rows in the same order, so a future param index 1:N maps
# to the intended joint draw.
# =============================================================================

pft_trait_samples <- lapply(
  selected_parameters,
  as.numeric
)

names(pft_trait_samples) <- export_traits

trait.samples <- stats::setNames(
  list(pft_trait_samples),
  WORKFLOW_PFT_NAME
)

ensemble.samples <- stats::setNames(
  list(selected_parameters),
  WORKFLOW_PFT_NAME
)

sa.samples <- list()
runs.samples <- list()
env.samples <- list()

pecan_samples <- list(
  trait.samples = trait.samples,
  sa.samples = sa.samples,
  ensemble.samples = ensemble.samples,
  runs.samples = runs.samples,
  env.samples = env.samples
)


# =============================================================================
# 6. Create SIPNET writer trait.values for every selected draw
# =============================================================================
#
# Each element has the structure expected by write.config.SIPNET():
#
#   list(
#     PFT_NAME = c(SLA = ..., leafC = ..., Amax = ..., ...)
#   )
# =============================================================================

trait.values.by.draw <- lapply(
  seq_len(nrow(selected_parameters)),
  function(draw_index) {
    values_i <- as.numeric(unlist(
      selected_parameters[draw_index, , drop = FALSE],
      use.names = FALSE
    ))
    
    names(values_i) <- export_traits
    
    stats::setNames(
      list(values_i),
      WORKFLOW_PFT_NAME
    )
  }
)

names(trait.values.by.draw) <- sprintf(
  "ensemble_%06d",
  seq_along(trait.values.by.draw)
)

# Convenient one-draw object for inspecting or testing a direct writer call.
trait.values <- trait.values.by.draw[[1L]]


# =============================================================================
# 7. Create audit metadata
# =============================================================================

export_metadata <- data.table::copy(metadata_21)

export_metadata[, no_xml_export_status := data.table::fcase(
  pecan_trait %in% export_traits,
  "EXPORTED_TO_PECAN_AND_SIPNET",
  pecan_trait %in% q10_omitted,
  "OMITTED_CONTEXT_DEFAULT_NO_PASS_Q10",
  default = "OMITTED_NOT_COMPLETE_OR_UNIT_INVALID"
)]

export_metadata[, export_draw_count := n_export_draws]
export_metadata[, export_seed := export_seed]

export_summary <- data.table::data.table(
  script_version = NO_XML_EXPORT_SCRIPT_VERSION,
  source_pft_name = PFT_NAME,
  workflow_pft_name = WORKFLOW_PFT_NAME,
  source_bundle = normalizePath(bundle_file, mustWork = TRUE),
  n_bridge_draws = nrow(draws_21),
  n_complete_joint_draws = nrow(complete_draws),
  n_nonfinite_joint_rows_removed = n_nonfinite_rows_removed,
  n_export_draws = n_export_draws,
  export_seed = export_seed,
  n_export_traits = length(export_traits),
  export_traits = paste(export_traits, collapse = "|"),
  constant_export_traits = paste(constant_export_traits, collapse = "|"),
  has_pass_leaf_q10_posterior = has_pass_leaf_q10_posterior,
  q10_omitted = paste(q10_omitted, collapse = "|")
)

export_bundle <- list(
  schema_version = "1.0.0",
  script_version = NO_XML_EXPORT_SCRIPT_VERSION,
  source_bundle = normalizePath(bundle_file, mustWork = TRUE),
  source_pft_name = PFT_NAME,
  workflow_pft_name = WORKFLOW_PFT_NAME,
  export_traits = export_traits,
  selected_draw_ids = selected_draw_ids,
  selected_draws = selected_draws,
  pecan_samples = pecan_samples,
  trait.values.by.draw = trait.values.by.draw,
  export_metadata = export_metadata,
  export_summary = export_summary
)


# =============================================================================
# 8. Save native PEcAn/SIPNET products
# =============================================================================

# Native PEcAn file contract. Later, a PEcAn workflow can place this exact file
# at its own settings$outdir/samples.Rdata or pass the equivalent pecan_samples
# object through an in-memory samples argument.
save(
  ensemble.samples,
  trait.samples,
  sa.samples,
  runs.samples,
  env.samples,
  file = file.path(export_dir, "samples.Rdata"),
  compress = FALSE
)

saveRDS(
  pecan_samples,
  file.path(export_dir, "pecan_samples.rds"),
  compress = FALSE
)

saveRDS(
  trait.values.by.draw,
  file.path(export_dir, "sipnet_trait_values_by_draw.rds"),
  compress = FALSE
)

save(
  trait.values,
  file = file.path(export_dir, "sipnet_trait.values_first_draw.Rdata"),
  compress = FALSE
)

saveRDS(
  export_bundle,
  file.path(export_dir, "pecan_sipnet_no_xml_export_bundle.rds"),
  compress = FALSE
)

data.table::fwrite(
  selected_draws,
  file.path(export_dir, "selected_joint_draws.csv")
)

data.table::fwrite(
  export_metadata,
  file.path(export_dir, "export_trait_metadata.csv")
)

data.table::fwrite(
  export_summary,
  file.path(export_dir, "export_summary.csv")
)

file_contract <- data.table::data.table(
  file = c(
    "samples.Rdata",
    "pecan_samples.rds",
    "sipnet_trait_values_by_draw.rds",
    "sipnet_trait.values_first_draw.Rdata",
    "pecan_sipnet_no_xml_export_bundle.rds",
    "selected_joint_draws.csv",
    "export_trait_metadata.csv",
    "export_summary.csv"
  ),
  consumer = c(
    "PEcAn file-based run.write.configs workflow",
    "PEcAn in-memory samples workflow",
    "write.config.SIPNET trait.values by ensemble member",
    "write.config.SIPNET one-draw inspection/test",
    "complete reusable adapter bundle",
    "human/front-end joint-draw audit",
    "human/front-end trait provenance audit",
    "human/front-end export summary"
  )
)

data.table::fwrite(
  file_contract,
  file.path(export_dir, "file_contract.csv")
)


# =============================================================================
# 9. In-session helper functions
# =============================================================================

get_exported_sipnet_trait_values <- function(draw_index = 1L) {
  draw_index <- check_positive_integer(draw_index, "draw_index")
  
  if (draw_index > length(trait.values.by.draw)) {
    stop(
      "draw_index exceeds exported draws: ",
      length(trait.values.by.draw),
      call. = FALSE
    )
  }
  
  trait.values.by.draw[[draw_index]]
}

load_exported_pecan_samples <- function(
    path = file.path(export_dir, "pecan_samples.rds")
) {
  if (!file.exists(path)) {
    stop("Cannot find PEcAn samples file: ", path, call. = FALSE)
  }
  
  readRDS(path)
}


# =============================================================================
# 10. Final report
# =============================================================================

message(
  "\nNo-XML PEcAn/SIPNET export complete.",
  "\nScript version: ", NO_XML_EXPORT_SCRIPT_VERSION,
  "\nSource PFT: ", PFT_NAME,
  "\nWorkflow PFT: ", WORKFLOW_PFT_NAME,
  "\nExported traits: ", length(export_traits),
  "\nExported joint draws: ", n_export_draws,
  "\nOutput: ", export_dir,
  "\nNo XML or PEcAn settings object was read."
)

print(
  export_metadata[
    ,
    .(
      pecan_trait,
      bridge_output_status,
      unit_validation_status,
      no_xml_export_status
    )
  ],
  nrows = Inf
)

print(file_contract, nrows = Inf)

