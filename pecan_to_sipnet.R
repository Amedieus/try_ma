# =============================================================================
# PASS-only single-PFT PEcAn MA -> writer-ready PEcAn traits for SIPNET v2
# =============================================================================
#
# This wrapper expects the following PFT directory layout:
#
#   <ma_root>/<pftname>/03_ma_qc/
#     trait.mcmc.QC_PASS.Rdata
#     post.distns.QC_PASS.Rdata       # optional for the bridge itself
#     jagged.data.QC_PASS.Rdata       # optional for the bridge itself
#     ma_qc_summary.csv               # optional; used for a second PASS check
#
# It depends on functions in md_to_pecan_bridge_v1.R. Source that file first,
# or provide bridge_file to run_pass_ma_to_writer_ready_pecan().
#
# Missing PASS traits are deliberately ignored. A target is included in
# writer_trait_draws only when every returned posterior draw is finite.
# Relationships requiring an ecological model are not fabricated: supply
# those functions through `models`, named by writer-ready PEcAn target.
# =============================================================================


.pass_bridge_require_data_table <- function() {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("需要安装 R package 'data.table'。", call. = FALSE)
  }
}


.pass_bridge_required_functions <- function() {
  c(
    "default_md_bridge_context",
    "md_to_pecan_bridge_registry",
    "extract_ma_beta_draws",
    "bridge_md_posteriors_to_pecan"
  )
}


.pass_bridge_load_functions <- function(bridge_file = NULL) {
  required <- .pass_bridge_required_functions()
  missing <- required[
    !vapply(required, exists, logical(1), mode = "function", inherits = TRUE)
  ]
  
  if (length(missing) > 0L && !is.null(bridge_file)) {
    if (!file.exists(bridge_file)) {
      stop("找不到 bridge_file：", bridge_file, call. = FALSE)
    }
    sys.source(bridge_file, envir = .GlobalEnv)
    missing <- required[
      !vapply(required, exists, logical(1), mode = "function", inherits = TRUE)
    ]
  }
  
  if (length(missing) > 0L) {
    stop(
      "缺少 bridge 函数：", paste(missing, collapse = ", "), "。\n",
      "请先 source('md_to_pecan_bridge_v1.R')，",
      "或设置 bridge_file。",
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}


.load_one_rdata_object <- function(path, preferred_names = character()) {
  if (!file.exists(path)) {
    stop("找不到文件：", path, call. = FALSE)
  }
  
  e <- new.env(parent = baseenv())
  loaded <- load(path, envir = e)
  
  preferred <- preferred_names[preferred_names %in% loaded]
  if (length(preferred) > 0L) {
    return(get(preferred[1L], envir = e, inherits = FALSE))
  }
  
  if (length(loaded) == 1L) {
    return(get(loaded, envir = e, inherits = FALSE))
  }
  
  stop(
    "文件包含多个对象，无法自动判断应读取哪一个：", path, "\n",
    "对象：", paste(loaded, collapse = ", "),
    call. = FALSE
  )
}


.load_optional_rdata_object <- function(path, preferred_names = character()) {
  if (!file.exists(path)) return(NULL)
  .load_one_rdata_object(path, preferred_names = preferred_names)
}


.subset_named_object <- function(x, keep_traits) {
  if (is.null(x)) return(NULL)
  
  if (is.list(x) && !is.data.frame(x)) {
    if (is.null(names(x))) return(x)
    return(x[intersect(keep_traits, names(x))])
  }
  
  if (is.matrix(x) || is.data.frame(x)) {
    rn <- rownames(x)
    if (!is.null(rn)) {
      keep <- intersect(keep_traits, rn)
      return(x[keep, , drop = FALSE])
    }
  }
  
  x
}


.find_first_column <- function(x, candidates) {
  nms <- names(x)
  index <- match(tolower(candidates), tolower(nms), nomatch = 0L)
  index <- index[index > 0L]
  if (length(index) == 0L) return(NA_character_)
  nms[index[1L]]
}


.pass_traits_from_qc_summary <- function(summary_file, available_traits) {
  if (!file.exists(summary_file)) return(available_traits)
  
  qc <- data.table::fread(summary_file)
  trait_col <- .find_first_column(
    qc,
    c("trait", "vname", "trait_name", "pecan_trait")
  )
  status_col <- .find_first_column(
    qc,
    c("status", "qc_status", "decision", "qc_decision")
  )
  
  if (is.na(trait_col) || is.na(status_col)) {
    warning(
      "ma_qc_summary.csv 中没有识别到 trait/status 列；",
      "将信任 QC_PASS Rdata 的内容。",
      call. = FALSE
    )
    return(available_traits)
  }
  
  qc_trait <- trimws(as.character(qc[[trait_col]]))
  qc_status <- toupper(trimws(as.character(qc[[status_col]])))
  pass_from_summary <- unique(qc_trait[qc_status == "PASS"])
  pass_from_summary <- pass_from_summary[
    !is.na(pass_from_summary) & nzchar(pass_from_summary)
  ]
  
  keep <- intersect(available_traits, pass_from_summary)
  if (length(keep) == 0L) {
    stop(
      "QC summary 与 trait.mcmc.QC_PASS.Rdata 交叉后没有 PASS traits。",
      call. = FALSE
    )
  }
  
  keep
}


.make_writer_trait_summary <- function(writer_trait_draws) {
  dt <- data.table::as.data.table(data.table::copy(writer_trait_draws))
  trait_names <- setdiff(names(dt), "draw_id")
  
  if (length(trait_names) == 0L) {
    return(data.table::data.table(
      pecan_trait = character(),
      n_draws = integer(),
      mean = numeric(),
      sd = numeric(),
      q025 = numeric(),
      q25 = numeric(),
      median = numeric(),
      q75 = numeric(),
      q975 = numeric(),
      min = numeric(),
      max = numeric()
    ))
  }
  
  data.table::rbindlist(
    lapply(trait_names, function(trait_i) {
      z <- suppressWarnings(as.numeric(dt[[trait_i]]))
      z <- z[is.finite(z)]
      q <- stats::quantile(
        z,
        probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
        na.rm = TRUE,
        names = FALSE,
        type = 8
      )
      
      data.table::data.table(
        pecan_trait = trait_i,
        n_draws = length(z),
        mean = mean(z),
        sd = if (length(z) > 1L) stats::sd(z) else NA_real_,
        q025 = q[1L],
        q25 = q[2L],
        median = q[3L],
        q75 = q[4L],
        q975 = q[5L],
        min = min(z),
        max = max(z)
      )
    }),
    use.names = TRUE,
    fill = TRUE
  )
}


.make_trait_values_by_draw <- function(writer_trait_draws, pftname) {
  dt <- data.table::as.data.table(data.table::copy(writer_trait_draws))
  trait_names <- setdiff(names(dt), "draw_id")
  
  if (length(trait_names) == 0L) return(list())
  
  result <- lapply(seq_len(nrow(dt)), function(i) {
    values <- unlist(dt[i, ..trait_names], use.names = FALSE)
    values <- suppressWarnings(as.numeric(values))
    names(values) <- trait_names
    
    if (any(!is.finite(values))) {
      stop(
        "内部错误：writer trait draw 中仍有非有限值，draw_id = ",
        dt$draw_id[i],
        call. = FALSE
      )
    }
    
    stats::setNames(list(values), pftname)
  })
  
  names(result) <- sprintf("draw_%06d", as.integer(dt$draw_id))
  result
}


.bridge_sources_for_target <- function(registry, target, pass_traits) {
  secondary_text <- ifelse(
    is.na(registry$secondary_writer_traits),
    "",
    as.character(registry$secondary_writer_traits)
  )
  secondary_has_target <- vapply(
    strsplit(secondary_text, "\\|"),
    function(z) target %in% trimws(z[nzchar(z)]),
    logical(1)
  )
  primary_has_target <- (
    !is.na(registry$candidate_pecan_trait) &
      registry$candidate_pecan_trait == target
  )
  
  unique(
    registry$source_md[
      (primary_has_target | secondary_has_target) &
        registry$source_md %in% pass_traits
    ]
  )
}


.make_bridge_target_status <- function(
    registry,
    pass_traits,
    complete_traits,
    partial_traits,
    models
) {
  bridge_targets_13 <- c(
    "Amax",
    "SLA",
    "growth_resp_factor",
    "wueConst",
    "root_turnover_rate",
    "root_respiration_rate",
    "turn_over_time",
    "wood_turnover_rate",
    "c2n_wood",
    "waterRemoveFrac",
    "dVPDSlope",
    "leaf_turnover_rate",
    "stem_respiration_rate"
  )
  
  data.table::rbindlist(
    lapply(bridge_targets_13, function(target_i) {
      pass_sources <- .bridge_sources_for_target(
        registry = registry,
        target = target_i,
        pass_traits = pass_traits
      )
      direct_pass <- target_i %in% pass_traits
      generated <- target_i %in% complete_traits
      partial <- target_i %in% partial_traits
      model_supplied <- target_i %in% names(models)
      
      status <- if (generated) {
        "GENERATED_WRITER_READY"
      } else if (partial) {
        "PARTIAL_DRAWS_DROPPED"
      } else if (!direct_pass && length(pass_sources) == 0L) {
        "NO_PASS_SOURCE"
      } else {
        "PASS_SOURCE_PRESENT_BUT_MODEL_OR_CONTEXT_MISSING"
      }
      
      data.table::data.table(
        target_pecan_trait = target_i,
        generated_writer_ready = generated,
        status = status,
        direct_pass_trait = direct_pass,
        n_pass_md_sources = length(pass_sources),
        pass_md_sources = paste(pass_sources, collapse = "|"),
        custom_model_supplied = model_supplied
      )
    }),
    use.names = TRUE,
    fill = TRUE
  )
}


#' Run a PASS-only single-PFT bridge into SIPNET writer-ready PEcAn traits
#'
#' @param pftname Exact PFT directory/name, e.g.
#'   "CroplandPool__Cereal_Croplands".
#' @param ma_root Parent directory containing one directory per PFT.
#' @param bridge_file Optional path to md_to_pecan_bridge_v1.R. It is only
#'   sourced when required bridge functions are not already loaded.
#' @param qc_subdir Subdirectory containing the QC_PASS Rdata files.
#' @param out_subdir New output subdirectory created below the PFT directory.
#' @param n_draws Number of beta.o posterior draws sampled per PASS trait.
#' @param seed Reproducible sampling seed.
#' @param context Named list of audited conversion assumptions/context. Missing
#'   entries inherit conservative defaults from default_md_bridge_context().
#' @param models Named list of custom functions function(draws, context), one
#'   per target. Models must propagate coefficient and residual uncertainty.
#' @param strict Passed to bridge_md_posteriors_to_pecan(). FALSE is suitable
#'   when missing targets should simply be omitted.
#' @param save_ma_draws_csv Whether to save the sampled beta.o table as CSV.
#'
#' @return A list containing writer_trait_draws, trait_values_by_draw,
#'   writer_trait_summary, target_status_13, bridge audits, and source paths.
run_pass_ma_to_writer_ready_pecan <- function(
    pftname,
    ma_root = "/projectnb/dietzelab/guYANG/TRY_meta_analysis",
    bridge_file = NULL,
    qc_subdir = "03_ma_qc",
    out_subdir = "04_writer_ready_pecan",
    n_draws = 5000L,
    seed = 20260828L,
    context = list(),
    models = list(),
    strict = FALSE,
    save_ma_draws_csv = FALSE
) {
  .pass_bridge_require_data_table()
  .pass_bridge_load_functions(bridge_file = bridge_file)
  
  if (length(pftname) != 1L || is.na(pftname) || !nzchar(trimws(pftname))) {
    stop("pftname 必须是一个非空字符串。", call. = FALSE)
  }
  pftname <- trimws(as.character(pftname))
  
  n_draws <- as.integer(n_draws)
  if (is.na(n_draws) || n_draws < 1L) {
    stop("n_draws 必须 >= 1。", call. = FALSE)
  }
  if (!is.list(context)) stop("context 必须是 named list。", call. = FALSE)
  if (!is.list(models)) stop("models 必须是 named list。", call. = FALSE)
  
  pft_dir <- file.path(ma_root, pftname)
  qc_dir <- file.path(pft_dir, qc_subdir)
  out_dir <- file.path(pft_dir, out_subdir)
  
  trait_file <- file.path(qc_dir, "trait.mcmc.QC_PASS.Rdata")
  post_file <- file.path(qc_dir, "post.distns.QC_PASS.Rdata")
  jagged_file <- file.path(qc_dir, "jagged.data.QC_PASS.Rdata")
  qc_summary_file <- file.path(qc_dir, "ma_qc_summary.csv")
  
  trait.mcmc.pass <- .load_one_rdata_object(
    trait_file,
    preferred_names = c(
      "trait.mcmc",
      "trait.mcmc.QC_PASS",
      "trait_mcmc",
      "trait_mcmc_pass"
    )
  )
  post.distns.pass <- .load_optional_rdata_object(
    post_file,
    preferred_names = c(
      "post.distns",
      "post.distns.QC_PASS",
      "post_distns",
      "post_distns_pass"
    )
  )
  jagged.data.pass <- .load_optional_rdata_object(
    jagged_file,
    preferred_names = c(
      "jagged.data",
      "jagged.data.QC_PASS",
      "jagged_data",
      "jagged_data_pass"
    )
  )
  
  if (!is.list(trait.mcmc.pass) || is.null(names(trait.mcmc.pass))) {
    stop(
      "trait.mcmc.QC_PASS.Rdata 中的 trait.mcmc 必须是命名 list。",
      call. = FALSE
    )
  }
  
  pass_traits <- .pass_traits_from_qc_summary(
    summary_file = qc_summary_file,
    available_traits = names(trait.mcmc.pass)
  )
  trait.mcmc.pass <- trait.mcmc.pass[pass_traits]
  post.distns.pass <- .subset_named_object(post.distns.pass, pass_traits)
  jagged.data.pass <- .subset_named_object(jagged.data.pass, pass_traits)
  
  ma_result_pass <- list(
    trait.mcmc = trait.mcmc.pass,
    post.distns = post.distns.pass,
    jagged.data = jagged.data.pass
  )
  
  context_final <- utils::modifyList(
    default_md_bridge_context(),
    context,
    keep.null = TRUE
  )
  
  ma_draws <- extract_ma_beta_draws(
    ma_result = ma_result_pass,
    n_draws = n_draws,
    seed = seed
  )
  
  registry <- md_to_pecan_bridge_registry()
  bridge_result <- bridge_md_posteriors_to_pecan(
    ma_draws = ma_draws,
    context = context_final,
    models = models,
    registry = registry,
    strict = strict
  )
  
  writer_trait_summary <- .make_writer_trait_summary(
    bridge_result$writer_trait_draws
  )
  trait_values_by_draw <- .make_trait_values_by_draw(
    bridge_result$writer_trait_draws,
    pftname = pftname
  )
  target_status_13 <- .make_bridge_target_status(
    registry = registry,
    pass_traits = pass_traits,
    complete_traits = bridge_result$complete_writer_traits,
    partial_traits = bridge_result$partial_writer_traits,
    models = models
  )
  
  result <- bridge_result
  result$pftname <- pftname
  result$pft_dir <- pft_dir
  result$qc_dir <- qc_dir
  result$out_dir <- out_dir
  result$pass_traits <- pass_traits
  result$n_pass_traits <- length(pass_traits)
  result$ma_draws <- ma_draws
  result$writer_trait_summary <- writer_trait_summary
  result$trait_values_by_draw <- trait_values_by_draw
  result$target_status_13 <- target_status_13
  result$context <- context_final
  result$custom_model_targets <- names(models)
  result$source_files <- list(
    trait_mcmc_pass = trait_file,
    post_distns_pass = if (file.exists(post_file)) post_file else NA_character_,
    jagged_data_pass = if (file.exists(jagged_file)) jagged_file else NA_character_,
    ma_qc_summary = if (file.exists(qc_summary_file)) qc_summary_file else NA_character_
  )
  
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  saveRDS(
    result,
    file.path(out_dir, "pass_ma_to_writer_ready_pecan_result.rds"),
    compress = FALSE
  )
  saveRDS(
    trait_values_by_draw,
    file.path(out_dir, "writer_trait_values_by_draw.rds"),
    compress = FALSE
  )
  
  writer_trait_draws <- bridge_result$writer_trait_draws
  save(
    writer_trait_draws,
    writer_trait_summary,
    target_status_13,
    file = file.path(out_dir, "writer_ready_pecan_traits.Rdata"),
    compress = FALSE
  )
  
  if (length(trait_values_by_draw) > 0L) {
    trait.values <- trait_values_by_draw[[1L]]
    save(
      trait.values,
      file = file.path(out_dir, "trait.values.example_first_draw.Rdata"),
      compress = FALSE
    )
  }
  
  data.table::fwrite(
    bridge_result$writer_trait_draws,
    file.path(out_dir, "writer_ready_pecan_trait_draws.csv")
  )
  data.table::fwrite(
    writer_trait_summary,
    file.path(out_dir, "writer_ready_pecan_trait_summary.csv")
  )
  data.table::fwrite(
    target_status_13,
    file.path(out_dir, "bridge_target_status_13.csv")
  )
  data.table::fwrite(
    bridge_result$rule_attempts,
    file.path(out_dir, "bridge_rule_attempts.csv")
  )
  data.table::fwrite(
    bridge_result$md_usage,
    file.path(out_dir, "md_trait_usage.csv")
  )
  data.table::fwrite(
    bridge_result$unresolved,
    file.path(out_dir, "md_traits_unresolved.csv")
  )
  
  if (isTRUE(save_ma_draws_csv)) {
    data.table::fwrite(
      ma_draws,
      file.path(out_dir, "pass_beta_o_draws.csv")
    )
  }
  
  message(
    "PASS-only bridge finished for ", pftname,
    ". PASS MA traits: ", length(pass_traits),
    "; complete writer-ready traits: ",
    length(bridge_result$complete_writer_traits),
    "; output: ", out_dir
  )
  
  result
}


# Return the named `trait.values` list expected by write.config.SIPNET() for a
# single posterior draw.
get_sipnet_trait_values <- function(result, draw_id = NULL) {
  if (!is.list(result) || is.null(result$writer_trait_draws)) {
    stop("result 必须来自 run_pass_ma_to_writer_ready_pecan()。", call. = FALSE)
  }
  
  dt <- data.table::as.data.table(result$writer_trait_draws)
  if (nrow(dt) == 0L || ncol(dt) <= 1L) {
    stop("结果中没有可送入 SIPNET writer 的完整 trait。", call. = FALSE)
  }
  
  if (is.null(draw_id)) draw_id <- dt$draw_id[1L]
  index <- which(dt$draw_id == draw_id)
  if (length(index) != 1L) {
    stop("draw_id 不存在或不唯一：", draw_id, call. = FALSE)
  }
  
  trait_names <- setdiff(names(dt), "draw_id")
  values <- unlist(dt[index, ..trait_names], use.names = FALSE)
  values <- suppressWarnings(as.numeric(values))
  names(values) <- trait_names
  stats::setNames(list(values), result$pftname)
}


# =============================================================================
# ALL-ENABLED scenario bridge
# =============================================================================
#
# This mode is intentionally different from the conservative bridge above.
# It enables every conditional switch and supplies explicit proxy models for
# the 13 candidate writer targets.  It is useful when the requested behavior
# is "use every PASS posterior that can contribute and skip only unavailable
# sources".  Results from proxy models are marked ALL_ENABLED_SCENARIO and
# should not be confused with direct or strictly algebraic conversions.
# =============================================================================


.scenario_context_vector <- function(context, name, n, default = NA_real_) {
  value <- context[[name]]
  if (is.null(value)) value <- default
  value <- suppressWarnings(as.numeric(value))
  if (length(value) == 1L) value <- rep(value, n)
  if (length(value) != n) {
    stop("context$", name, " 的长度必须是1或 posterior draw 数。", call. = FALSE)
  }
  value
}


.scenario_context_scalar <- function(context, name, default = NA_real_) {
  value <- context[[name]]
  if (is.null(value) || length(value) == 0L) return(default)
  value <- suppressWarnings(as.numeric(value[1L]))
  if (!is.finite(value)) default else value
}


.scenario_draw <- function(draws, name) {
  n <- nrow(draws)
  if (!name %in% names(draws)) return(rep(NA_real_, n))
  suppressWarnings(as.numeric(draws[[name]]))
}


.scenario_first_finite <- function(draws, names) {
  n <- nrow(draws)
  out <- rep(NA_real_, n)
  for (name_i in names) {
    candidate <- .scenario_draw(draws, name_i)
    use <- !is.finite(out) & is.finite(candidate)
    out[use] <- candidate[use]
  }
  out
}


.scenario_geometric_mean <- function(values) {
  if (length(values) == 0L) return(numeric())
  m <- do.call(cbind, values)
  if (is.null(dim(m))) m <- matrix(m, ncol = 1L)
  m[!is.finite(m) | m <= 0] <- NA_real_
  n_present <- rowSums(is.finite(m))
  out <- rep(NA_real_, nrow(m))
  use <- n_present > 0L
  if (any(use)) {
    out[use] <- exp(
      rowSums(log(m[use, , drop = FALSE]), na.rm = TRUE) /
        n_present[use]
    )
  }
  out
}


.scenario_clamp <- function(x, lower, upper) {
  x <- suppressWarnings(as.numeric(x))
  use <- is.finite(x)
  x[use] <- pmin(pmax(x[use], lower), upper)
  x
}


.scenario_positive_noise <- function(x, sdlog = 0) {
  x <- suppressWarnings(as.numeric(x))
  use <- which(is.finite(x) & x > 0)
  if (length(use) > 0L && is.finite(sdlog) && sdlog > 0) {
    # -0.5*sdlog^2 keeps the multiplicative residual mean near one.
    x[use] <- x[use] * exp(
      stats::rnorm(length(use), mean = -0.5 * sdlog^2, sd = sdlog)
    )
  }
  x
}


.scenario_result <- function(value, source_traits) {
  value <- suppressWarnings(as.numeric(value))
  attr(value, "source_traits") <- unique(as.character(source_traits))
  value
}


.scenario_has_finite_source <- function(draws, names) {
  any(vapply(
    names,
    function(name_i) any(is.finite(.scenario_draw(draws, name_i))),
    logical(1)
  ))
}


# All conditional switches plus explicit numerical assumptions.  The values
# below are scenario defaults, not claims that TRY measured the missing
# environmental covariates.  Override any entry through context_overrides.
all_enabled_md_bridge_context <- function(
    pftname,
    context_overrides = list()
) {
  is_crop <- grepl("crop|cereal", pftname, ignore.case = TRUE)
  
  context <- list(
    # Enable every conditional relationship in md_to_pecan_bridge_v1.R.
    photosynthesis_area_is_amax = TRUE,
    photosynthesis_mass_is_amax = TRUE,
    pnue_conditions_match = TRUE,
    rd_vcmax_same_reference = TRUE,
    leaf_lifespan_inverse_valid = TRUE,
    wue_definition_matches_sipnet = TRUE,
    gas_exchange_conditions_match = TRUE,
    generic_root_matches_fine_root = TRUE,
    fine_root_gas_is_co2 = TRUE,
    generic_root_gas_is_co2 = TRUE,
    stem_represents_sipnet_wood_pool = TRUE,
    litter_rate_is_first_order_k = TRUE,
    litter_rate_matches_sipnet_reference = TRUE,
    allow_root_depth_soilwhc_approx = TRUE,
    allow_leaf_q10_as_veg_proxy = TRUE,
    allow_leaf_q10_as_fine_root_proxy = TRUE,
    allow_leaf_q10_as_coarse_root_proxy = TRUE,
    light_response_model = "rectangular_hyperbola",
    par_basis_compatible = TRUE,
    
    # Measurement/reference assumptions used when TRY metadata are absent.
    leaf_respiration_tmeas_c = 25,
    leaf_respiration_tref_c = 25,
    leaf_respiration_q10 = 2.0,
    generic_root_respiration_tmeas_c = 25,
    fine_root_respiration_tmeas_c = 25,
    root_respiration_tref_c = 25,
    root_respiration_q10 = 2.0,
    vpd_kpa = 1.5,
    soil_water_capacity_fraction = 0.15,
    
    # C3 FvCB scenario constants at 25 C.
    ambient_co2_umol_mol = 400,
    ci_ca_default = 0.70,
    gamma_star_umol_mol = 42.75,
    kc_umol_mol = 404.9,
    ko_mmol_mol = 278.4,
    oxygen_mmol_mol = 210,
    rd_vcmax_default_fraction = 0.015,
    
    # Reference values and scenario coefficients for empirical proxies.
    construction_substrate_carbon_fraction = 0.40,
    default_leaf_carbon_fraction = 0.45,
    default_wood_carbon_percent = 45,
    atmospheric_pressure_kpa = 101.325,
    
    sla_reference = 20,
    ldmc_reference = 0.25,
    leaf_thickness_reference_mm = 0.20,
    leaf_density_reference_kg_m3 = 400,
    
    root_turnover_base_year = if (is_crop) 1.0 else 0.137,
    root_respiration_base_umol_kg_s = 750,
    root_density_reference_g_cm3 = 0.25,
    root_diameter_reference_mm = 0.50,
    root_n_reference_g_kg = 15,
    root_rdmc_reference = 0.30,
    
    litter_breakdown_base_year = 0.40,
    litter_lignin_reference_percent = 15,
    litter_n_reference_g_kg = 10,
    litter_cn_reference = 40,
    cwd_to_litter_rate_factor = 0.50,
    
    wood_turnover_base_year = if (is_crop) 1.0 else 0.014,
    wood_density_reference_kg_m3 = 500,
    
    water_remove_base_day = 0.088,
    rooting_depth_reference_m = 1.0,
    leaf_hydraulic_reference = 5.0,
    leaf_wsc_reference = 0.20,
    
    dvpd_slope_base = 0.05,
    turgor_loss_reference_mpa = 1.5,
    
    leaf_turnover_base_year = if (is_crop) 1.0 else 0.50,
    stem_respiration_base_umol_kg_s = 33,
    stem_n_reference_g_kg = 3,
    
    # Residual uncertainty attached to proxy equations.
    proxy_sdlog_sla = 0.15,
    proxy_sdlog_amax = 0.15,
    proxy_sdlog_wue = 0.10,
    proxy_sdlog_root_turnover = 0.25,
    proxy_sdlog_root_respiration = 0.25,
    proxy_sdlog_litter = 0.25,
    proxy_sdlog_wood_turnover = 0.25,
    proxy_sdlog_hydrology = 0.20,
    proxy_sdlog_leaf_turnover = 0.20,
    proxy_sdlog_stem_respiration = 0.25
  )
  
  if (!is.list(context_overrides)) {
    stop("context_overrides 必须是 named list。", call. = FALSE)
  }
  utils::modifyList(context, context_overrides, keep.null = TRUE)
}


# Default proxy/mechanistic models for the 13 candidate targets.  Each model
# returns NA when none of its required PASS sources exists, so missing PASS
# traits are still ignored as requested.
all_enabled_md_bridge_models <- function() {
  list(
    # 1. SLA: direct and thickness*density rules have priority.  This fills
    # remaining draws from LDMC, thickness, or density relative scaling.
    SLA = function(draws, context) {
      sources <- c(
        "LDMC_md", "leaf_thickness_md", "leaf_tissue_density_md"
      )
      n <- nrow(draws)
      if (!.scenario_has_finite_source(draws, sources)) {
        return(.scenario_result(rep(NA_real_, n), sources))
      }
      ref <- .scenario_context_scalar(context, "sla_reference", 20)
      estimates <- list(
        ref * (.scenario_draw(draws, "LDMC_md") /
                 .scenario_context_scalar(context, "ldmc_reference", 0.25))^-1,
        ref * (.scenario_draw(draws, "leaf_thickness_md") /
                 .scenario_context_scalar(context, "leaf_thickness_reference_mm", 0.20))^-0.8,
        ref * (.scenario_draw(draws, "leaf_tissue_density_md") /
                 .scenario_context_scalar(context, "leaf_density_reference_kg_m3", 400))^-0.8
      )
      value <- .scenario_geometric_mean(estimates)
      value <- .scenario_positive_noise(
        value,
        .scenario_context_scalar(context, "proxy_sdlog_sla", 0.15)
      )
      value <- .scenario_clamp(value, 1, 80)
      .scenario_result(value, sources)
    },
    
    # 2. Amax: use a C3 FvCB light-saturated approximation.  Direct observed
    # Amax routes are filled earlier and retain priority.
    Amax = function(draws, context) {
      sources <- c(
        "Vcmax_area_Tref_md", "Vcmax_mass_Tref_md",
        "Jmax_area_Tref_md", "Jmax_mass_Tref_md",
        "ci_md", "ci_ca", "leaf_respiration_rate_m2",
        "Rd_Vcmax_fraction_md", "leaf_N_area_md", "leaf_N_mass_md", "SLA"
      )
      n <- nrow(draws)
      sla <- .scenario_draw(draws, "SLA")
      vcmax <- .scenario_draw(draws, "Vcmax_area_Tref_md")
      vcmax_mass <- .scenario_draw(draws, "Vcmax_mass_Tref_md")
      use <- !is.finite(vcmax) & is.finite(vcmax_mass) & is.finite(sla) & sla > 0
      vcmax[use] <- vcmax_mass[use] / sla[use]
      
      jmax <- .scenario_draw(draws, "Jmax_area_Tref_md")
      jmax_mass <- .scenario_draw(draws, "Jmax_mass_Tref_md")
      use <- !is.finite(jmax) & is.finite(jmax_mass) & is.finite(sla) & sla > 0
      jmax[use] <- jmax_mass[use] / sla[use]
      
      ca <- .scenario_context_vector(
        context, "ambient_co2_umol_mol", n, 400
      )
      ci <- .scenario_draw(draws, "ci_md")
      ci_ca <- .scenario_draw(draws, "ci_ca")
      use <- !is.finite(ci) & is.finite(ci_ca)
      ci[use] <- ci_ca[use] * ca[use]
      ci_default <- .scenario_context_scalar(context, "ci_ca_default", 0.70)
      ci[!is.finite(ci)] <- ci_default * ca[!is.finite(ci)]
      
      gamma <- .scenario_context_scalar(context, "gamma_star_umol_mol", 42.75)
      kc <- .scenario_context_scalar(context, "kc_umol_mol", 404.9)
      ko <- .scenario_context_scalar(context, "ko_mmol_mol", 278.4)
      oxygen <- .scenario_context_scalar(context, "oxygen_mmol_mol", 210)
      
      ac <- vcmax * (ci - gamma) / (ci + kc * (1 + oxygen / ko))
      aj <- jmax * (ci - gamma) / (4 * (ci + 2 * gamma))
      ac[!is.finite(ac) | ac <= 0] <- NA_real_
      aj[!is.finite(aj) | aj <= 0] <- NA_real_
      
      gross <- ac
      both <- is.finite(ac) & is.finite(aj)
      gross[both] <- pmin(ac[both], aj[both])
      only_j <- !is.finite(gross) & is.finite(aj)
      gross[only_j] <- aj[only_j]
      
      rd <- .scenario_draw(draws, "leaf_respiration_rate_m2")
      rd_fraction <- .scenario_draw(draws, "Rd_Vcmax_fraction_md")
      use <- !is.finite(rd) & is.finite(rd_fraction) & is.finite(vcmax)
      rd[use] <- rd_fraction[use] * vcmax[use]
      default_fraction <- .scenario_context_scalar(
        context, "rd_vcmax_default_fraction", 0.015
      )
      use <- !is.finite(rd) & is.finite(vcmax)
      rd[use] <- default_fraction * vcmax[use]
      
      value <- gross - ifelse(is.finite(rd), rd, 0)
      
      # Leaf-N fallback when neither Vcmax nor Jmax is available.
      n_area <- .scenario_draw(draws, "leaf_N_area_md")
      n_mass <- .scenario_draw(draws, "leaf_N_mass_md")
      use <- !is.finite(n_area) & is.finite(n_mass) & is.finite(sla) & sla > 0
      n_area[use] <- n_mass[use] / sla[use]
      n_fallback <- 15 * (n_area / 1.5)^0.70
      use <- (!is.finite(value) | value <= 0) & is.finite(n_fallback)
      value[use] <- n_fallback[use]
      
      value[!is.finite(value) | value <= 0] <- NA_real_
      value <- .scenario_positive_noise(
        value,
        .scenario_context_scalar(context, "proxy_sdlog_amax", 0.15)
      )
      value <- .scenario_clamp(value, 0.1, 80)
      .scenario_result(value, sources)
    },
    
    # 3. Growth respiration fraction from construction-cost carbon balance.
    growth_resp_factor = function(draws, context) {
      sources <- c(
        "leaf_construction_cost_mass_md",
        "leaf_construction_cost_area_md", "SLA", "leafC"
      )
      n <- nrow(draws)
      cost <- .scenario_draw(draws, "leaf_construction_cost_mass_md")
      cost_area <- .scenario_draw(draws, "leaf_construction_cost_area_md")
      sla <- .scenario_draw(draws, "SLA")
      use <- !is.finite(cost) & is.finite(cost_area) & is.finite(sla) & sla > 0
      cost[use] <- cost_area[use] * sla[use] / 1000
      if (!any(is.finite(cost) & cost > 0)) {
        return(.scenario_result(rep(NA_real_, n), sources))
      }
      leaf_c <- .scenario_draw(draws, "leafC") / 100
      leaf_c[!is.finite(leaf_c)] <- .scenario_context_scalar(
        context, "default_leaf_carbon_fraction", 0.45
      )
      substrate_c <- cost * .scenario_context_scalar(
        context, "construction_substrate_carbon_fraction", 0.40
      )
      value <- 1 - leaf_c / substrate_c
      value[!is.finite(value)] <- NA_real_
      value <- .scenario_clamp(value, 0.10, 0.30)
      .scenario_result(value, sources)
    },
    
    # 4. wueConst from ci/ca when direct instantaneous WUE is unavailable.
    wueConst = function(draws, context) {
      sources <- c("ci_ca", "ci_md")
      n <- nrow(draws)
      ca <- .scenario_context_vector(
        context, "ambient_co2_umol_mol", n, 400
      )
      ci <- .scenario_draw(draws, "ci_md")
      ratio <- .scenario_draw(draws, "ci_ca")
      use <- !is.finite(ci) & is.finite(ratio)
      ci[use] <- ratio[use] * ca[use]
      if (!any(is.finite(ci))) {
        return(.scenario_result(rep(NA_real_, n), sources))
      }
      pressure <- .scenario_context_vector(
        context, "atmospheric_pressure_kpa", n, 101.325
      )
      delta_c <- ca - ci
      # Derived from A/E = (Ca-Ci)*P/(1.6*VPD), followed by the writer's
      # mg-CO2/g-H2O conversion. VPD cancels in wueConst.
      value <- 2.442 * delta_c * pressure / 1600
      value[!is.finite(value) | value <= 0] <- NA_real_
      value <- .scenario_positive_noise(
        value,
        .scenario_context_scalar(context, "proxy_sdlog_wue", 0.10)
      )
      value <- .scenario_clamp(value, 0.01, 109)
      .scenario_result(value, sources)
    },
    
    # 5. Fine-root turnover from the multidimensional root-economics space.
    root_turnover_rate = function(draws, context) {
      sources <- c(
        "fine_root_tissue_density_md", "root_tissue_density_md",
        "fine_root_diameter_md", "root_diameter_md",
        "fine_root_N_mass_md", "root_N_mass_md",
        "fine_root_RDMC_md", "root_RDMC_md", "fine_root_SRL_md", "root_SRL_md"
      )
      n <- nrow(draws)
      if (!.scenario_has_finite_source(draws, sources)) {
        return(.scenario_result(rep(NA_real_, n), sources))
      }
      density <- .scenario_first_finite(
        draws, c("fine_root_tissue_density_md", "root_tissue_density_md")
      )
      diameter <- .scenario_first_finite(
        draws, c("fine_root_diameter_md", "root_diameter_md")
      )
      nitrogen <- .scenario_first_finite(
        draws, c("fine_root_N_mass_md", "root_N_mass_md")
      )
      rdmc <- .scenario_first_finite(
        draws, c("fine_root_RDMC_md", "root_RDMC_md")
      )
      base <- .scenario_context_scalar(context, "root_turnover_base_year", 0.137)
      components <- list(
        rep(base, n),
        (density / .scenario_context_scalar(context, "root_density_reference_g_cm3", 0.25))^-0.50,
        (diameter / .scenario_context_scalar(context, "root_diameter_reference_mm", 0.50))^-0.25,
        (nitrogen / .scenario_context_scalar(context, "root_n_reference_g_kg", 15))^0.25,
        (rdmc / .scenario_context_scalar(context, "root_rdmc_reference", 0.30))^-0.25
      )
      # Multiply only available relative components, retaining the base.
      value <- rep(base, n)
      for (component in components[-1L]) {
        use <- is.finite(component) & component > 0
        value[use] <- value[use] * component[use]
      }
      value <- .scenario_positive_noise(
        value,
        .scenario_context_scalar(context, "proxy_sdlog_root_turnover", 0.25)
      )
      value <- .scenario_clamp(value, 0.001, 1.0)
      .scenario_result(value, sources)
    },
    
    # 6. Fine-root respiration at 25 C from root N and structural traits.
    root_respiration_rate = function(draws, context) {
      sources <- c(
        "fine_root_N_mass_md", "root_N_mass_md",
        "fine_root_tissue_density_md", "root_tissue_density_md",
        "fine_root_diameter_md", "root_diameter_md"
      )
      n <- nrow(draws)
      if (!.scenario_has_finite_source(draws, sources)) {
        return(.scenario_result(rep(NA_real_, n), sources))
      }
      nitrogen <- .scenario_first_finite(
        draws, c("fine_root_N_mass_md", "root_N_mass_md")
      )
      density <- .scenario_first_finite(
        draws, c("fine_root_tissue_density_md", "root_tissue_density_md")
      )
      diameter <- .scenario_first_finite(
        draws, c("fine_root_diameter_md", "root_diameter_md")
      )
      base <- .scenario_context_scalar(
        context, "root_respiration_base_umol_kg_s", 750
      )
      value <- rep(base, n)
      components <- list(
        (nitrogen / .scenario_context_scalar(context, "root_n_reference_g_kg", 15))^0.75,
        (density / .scenario_context_scalar(context, "root_density_reference_g_cm3", 0.25))^-0.25,
        (diameter / .scenario_context_scalar(context, "root_diameter_reference_mm", 0.50))^-0.10
      )
      for (component in components) {
        use <- is.finite(component) & component > 0
        value[use] <- value[use] * component[use]
      }
      value <- .scenario_positive_noise(
        value,
        .scenario_context_scalar(context, "proxy_sdlog_root_respiration", 0.25)
      )
      value <- .scenario_clamp(value, 1, 10000)
      .scenario_result(value, sources)
    },
    
    # 7. SIPNET litterBreakdownRate from litter chemistry or CWD rate proxy.
    turn_over_time = function(draws, context) {
      sources <- c(
        "litter_lignin_md", "litter_N_mass_md", "litter_CN_md",
        "litter_cellulose_md", "cwd_decomposition_k_md",
        "cwd_decomposition_rate_md"
      )
      n <- nrow(draws)
      if (!.scenario_has_finite_source(draws, sources)) {
        return(.scenario_result(rep(NA_real_, n), sources))
      }
      lignin <- .scenario_draw(draws, "litter_lignin_md")
      nitrogen <- .scenario_draw(draws, "litter_N_mass_md")
      cn <- .scenario_draw(draws, "litter_CN_md")
      base <- .scenario_context_scalar(context, "litter_breakdown_base_year", 0.40)
      value <- rep(base, n)
      lignin_component <- exp(
        -3 * (lignin - .scenario_context_scalar(
          context, "litter_lignin_reference_percent", 15
        )) / 100
      )
      n_component <- (nitrogen / .scenario_context_scalar(
        context, "litter_n_reference_g_kg", 10
      ))^0.25
      cn_component <- (cn / .scenario_context_scalar(
        context, "litter_cn_reference", 40
      ))^-0.25
      for (component in list(lignin_component, n_component, cn_component)) {
        use <- is.finite(component) & component > 0
        value[use] <- value[use] * component[use]
      }
      cwd_k <- .scenario_first_finite(
        draws, c("cwd_decomposition_k_md", "cwd_decomposition_rate_md")
      )
      chemistry_missing <- !is.finite(lignin) & !is.finite(nitrogen) & !is.finite(cn)
      use <- chemistry_missing & is.finite(cwd_k) & cwd_k > 0
      value[use] <- cwd_k[use] * .scenario_context_scalar(
        context, "cwd_to_litter_rate_factor", 0.50
      )
      value <- .scenario_positive_noise(
        value,
        .scenario_context_scalar(context, "proxy_sdlog_litter", 0.25)
      )
      value <- .scenario_clamp(value, 0.13, 1.20)
      .scenario_result(value, sources)
    },
    
    # 8. Structural-pool turnover from wood density strategy.
    wood_turnover_rate = function(draws, context) {
      sources <- c(
        "wood_density_md", "sapwood_density_md", "branch_density_md", "stem_DMC_md"
      )
      n <- nrow(draws)
      if (!.scenario_has_finite_source(draws, sources)) {
        return(.scenario_result(rep(NA_real_, n), sources))
      }
      density <- .scenario_first_finite(
        draws, c("wood_density_md", "sapwood_density_md", "branch_density_md")
      )
      base <- .scenario_context_scalar(context, "wood_turnover_base_year", 0.014)
      value <- rep(base, n)
      component <- (density / .scenario_context_scalar(
        context, "wood_density_reference_kg_m3", 500
      ))^-0.50
      use <- is.finite(component) & component > 0
      value[use] <- value[use] * component[use]
      stem_dmc <- .scenario_draw(draws, "stem_DMC_md")
      component <- (stem_dmc / 0.50)^-0.25
      use <- is.finite(component) & component > 0
      value[use] <- value[use] * component[use]
      value <- .scenario_positive_noise(
        value,
        .scenario_context_scalar(context, "proxy_sdlog_wood_turnover", 0.25)
      )
      value <- .scenario_clamp(value, 0.001, 1.0)
      .scenario_result(value, sources)
    },
    
    # 9. Wood C:N, including wood-N with an explicitly recorded default C%.
    c2n_wood = function(draws, context) {
      sources <- c("stem_CN_md", "stem_C_md", "stem_N_mass_md", "wood_N_mass_md")
      n <- nrow(draws)
      wood_n <- .scenario_draw(draws, "wood_N_mass_md")
      if (!any(is.finite(wood_n) & wood_n > 0)) {
        return(.scenario_result(rep(NA_real_, n), sources))
      }
      carbon <- .scenario_draw(draws, "stem_C_md")
      carbon[!is.finite(carbon)] <- .scenario_context_scalar(
        context, "default_wood_carbon_percent", 45
      )
      value <- 10 * carbon / wood_n
      value[!is.finite(value) | value <= 0] <- NA_real_
      value <- .scenario_clamp(value, 5, 500)
      .scenario_result(value, sources)
    },
    
    # 10. Daily removable water fraction from root access and hydraulics.
    waterRemoveFrac = function(draws, context) {
      sources <- c(
        "fine_root_rooting_depth_md", "rooting_depth_md",
        "leaf_hydraulic_conductance_md", "sapwood_specific_conductivity_md",
        "leaf_WSC_md"
      )
      n <- nrow(draws)
      if (!.scenario_has_finite_source(draws, sources)) {
        return(.scenario_result(rep(NA_real_, n), sources))
      }
      depth <- .scenario_first_finite(
        draws, c("fine_root_rooting_depth_md", "rooting_depth_md")
      )
      leaf_k <- .scenario_draw(draws, "leaf_hydraulic_conductance_md")
      leaf_wsc <- .scenario_draw(draws, "leaf_WSC_md")
      base <- .scenario_context_scalar(context, "water_remove_base_day", 0.088)
      value <- rep(base, n)
      components <- list(
        (depth / .scenario_context_scalar(context, "rooting_depth_reference_m", 1.0))^0.25,
        (leaf_k / .scenario_context_scalar(context, "leaf_hydraulic_reference", 5.0))^0.15,
        (leaf_wsc / .scenario_context_scalar(context, "leaf_wsc_reference", 0.20))^0.10
      )
      for (component in components) {
        use <- is.finite(component) & component > 0
        value[use] <- value[use] * component[use]
      }
      value <- .scenario_positive_noise(
        value,
        .scenario_context_scalar(context, "proxy_sdlog_hydrology", 0.20)
      )
      value <- .scenario_clamp(value, 0.001, 0.160)
      .scenario_result(value, sources)
    },
    
    # 11. SIPNET VPD sensitivity from drought-tolerance water potentials.
    dVPDSlope = function(draws, context) {
      sources <- c(
        "leaf_turgor_loss_point_md", "osmotic_potential_tlp_md",
        "osmotic_potential_full_turgor_md", "midday_leaf_water_potential_md",
        "predawn_leaf_water_potential_md", "leaf_hydraulic_conductance_md"
      )
      n <- nrow(draws)
      if (!.scenario_has_finite_source(draws, sources)) {
        return(.scenario_result(rep(NA_real_, n), sources))
      }
      psi <- abs(.scenario_first_finite(
        draws,
        c(
          "leaf_turgor_loss_point_md", "osmotic_potential_tlp_md",
          "osmotic_potential_full_turgor_md", "midday_leaf_water_potential_md"
        )
      ))
      base <- .scenario_context_scalar(context, "dvpd_slope_base", 0.05)
      value <- rep(base, n)
      component <- (psi / .scenario_context_scalar(
        context, "turgor_loss_reference_mpa", 1.5
      ))^-1
      use <- is.finite(component) & component > 0
      value[use] <- value[use] * component[use]
      value <- .scenario_positive_noise(
        value,
        .scenario_context_scalar(context, "proxy_sdlog_hydrology", 0.20)
      )
      value <- .scenario_clamp(value, 0.010, 0.250)
      .scenario_result(value, sources)
    },
    
    # 12. Leaf turnover from leaf-economics proxies when lifespan is absent.
    leaf_turnover_rate = function(draws, context) {
      sources <- c("SLA", "LDMC_md", "leaf_lignin_md")
      n <- nrow(draws)
      if (!.scenario_has_finite_source(draws, sources)) {
        return(.scenario_result(rep(NA_real_, n), sources))
      }
      sla <- .scenario_draw(draws, "SLA")
      ldmc <- .scenario_draw(draws, "LDMC_md")
      lignin <- .scenario_draw(draws, "leaf_lignin_md")
      base <- .scenario_context_scalar(context, "leaf_turnover_base_year", 0.50)
      value <- rep(base, n)
      components <- list(
        (sla / .scenario_context_scalar(context, "sla_reference", 20))^0.50,
        (ldmc / .scenario_context_scalar(context, "ldmc_reference", 0.25))^-0.50,
        (lignin / 15)^-0.25
      )
      for (component in components) {
        use <- is.finite(component) & component > 0
        value[use] <- value[use] * component[use]
      }
      value <- .scenario_positive_noise(
        value,
        .scenario_context_scalar(context, "proxy_sdlog_leaf_turnover", 0.20)
      )
      value <- .scenario_clamp(value, 0.001, 1.0)
      .scenario_result(value, sources)
    },
    
    # 13. Stem maintenance respiration at 25 C from stem/wood N.
    stem_respiration_rate = function(draws, context) {
      sources <- c(
        "stem_N_mass_md", "wood_N_mass_md", "wood_density_md", "sapwood_density_md"
      )
      n <- nrow(draws)
      if (!.scenario_has_finite_source(draws, sources)) {
        return(.scenario_result(rep(NA_real_, n), sources))
      }
      nitrogen <- .scenario_first_finite(
        draws, c("stem_N_mass_md", "wood_N_mass_md")
      )
      density <- .scenario_first_finite(
        draws, c("wood_density_md", "sapwood_density_md")
      )
      base <- .scenario_context_scalar(
        context, "stem_respiration_base_umol_kg_s", 33
      )
      value <- rep(base, n)
      components <- list(
        (nitrogen / .scenario_context_scalar(context, "stem_n_reference_g_kg", 3))^0.75,
        (density / .scenario_context_scalar(
          context, "wood_density_reference_kg_m3", 500
        ))^-0.25
      )
      for (component in components) {
        use <- is.finite(component) & component > 0
        value[use] <- value[use] * component[use]
      }
      value <- .scenario_positive_noise(
        value,
        .scenario_context_scalar(context, "proxy_sdlog_stem_respiration", 0.25)
      )
      value <- .scenario_clamp(value, 1, 1000)
      .scenario_result(value, sources)
    }
  )
}


.all_enabled_assumption_table <- function(context) {
  scalar <- context[vapply(context, length, integer(1)) == 1L]
  data.table::data.table(
    assumption = names(scalar),
    value = vapply(
      scalar,
      function(x) paste(as.character(x), collapse = "|"),
      character(1)
    ),
    assumption_mode = "ALL_ENABLED_SCENARIO_DEFAULT_OR_OVERRIDE"
  )
}


.all_enabled_provenance <- function(result) {
  attempts <- data.table::as.data.table(result$rule_attempts)
  successful <- attempts[n_filled > 0L]
  if (nrow(successful) == 0L) {
    return(data.table::data.table(
      pecan_trait = character(),
      conversion_classes = character(),
      source_traits = character(),
      contains_scenario_model = logical()
    ))
  }
  successful[
    ,
    .(
      conversion_classes = paste(unique(conversion_class), collapse = "|"),
      source_traits = paste(unique(source_traits), collapse = "|"),
      contains_scenario_model = any(conversion_class == "USER_SUPPLIED_MODEL")
    ),
    by = .(pecan_trait = target_pecan_trait)
  ]
}


#' Run the PASS-only bridge with every conditional and proxy model enabled
#'
#' Missing PASS source traits are ignored.  This returns more writer-ready
#' traits than the conservative function, while retaining an explicit audit of
#' which outputs depend on scenario assumptions.
run_pass_ma_to_writer_ready_pecan_all_enabled <- function(
    pftname,
    
    # 现在 ma_root 直接指向该 PFT 文件夹
    ma_root = file.path(
      "/projectnb/dietzelab/guYANG",
      "TRY_meta_analysis",
      pftname
    ),
    
    bridge_file = NULL,
    qc_subdir = "03_ma_qc",
    out_subdir = "04_writer_ready_pecan_all_enabled",
    n_draws = 6000L,
    seed = 20260828L,
    context_overrides = list(),
    model_overrides = list(),
    save_ma_draws_csv = FALSE
) {
  
  # ==========================================================
  # 1. 检查 PFT 名称和目录
  # ==========================================================
  
  if (
    length(pftname) != 1L ||
    is.na(pftname) ||
    !nzchar(trimws(pftname))
  ) {
    stop(
      "pftname 必须是一个非空字符串。",
      call. = FALSE
    )
  }
  
  pftname <- trimws(as.character(pftname))
  
  # 删除路径末尾可能存在的 /
  pft_dir <- sub(
    "[/\\\\]+$",
    "",
    as.character(ma_root)
  )
  
  if (
    length(pft_dir) != 1L ||
    is.na(pft_dir) ||
    !nzchar(pft_dir)
  ) {
    stop(
      "ma_root 必须是有效的 PFT 文件夹路径。",
      call. = FALSE
    )
  }
  
  # ma_root 现在必须以 pftname 结尾
  if (!identical(basename(pft_dir), pftname)) {
    stop(
      paste0(
        "当前版本要求 ma_root 直接指向 PFT 文件夹。\n",
        "ma_root 最后一层：", basename(pft_dir), "\n",
        "pftname：", pftname, "\n",
        "正确格式应为：TRY_meta_analysis/<pftname>"
      ),
      call. = FALSE
    )
  }
  
  qc_dir <- file.path(
    pft_dir,
    qc_subdir
  )
  
  if (!dir.exists(qc_dir)) {
    stop(
      paste0(
        "没有找到 QC 文件夹：\n",
        qc_dir
      ),
      call. = FALSE
    )
  }
  
  required_qc_file <- file.path(
    qc_dir,
    "trait.mcmc.QC_PASS.Rdata"
  )
  
  if (!file.exists(required_qc_file)) {
    stop(
      paste0(
        "没有找到 PASS trait.mcmc 文件：\n",
        required_qc_file
      ),
      call. = FALSE
    )
  }
  
  
  # ==========================================================
  # 2. 建立全部开启的 bridge context 和模型
  # ==========================================================
  
  context <- all_enabled_md_bridge_context(
    pftname = pftname,
    context_overrides = context_overrides
  )
  
  models <- all_enabled_md_bridge_models()
  
  if (!is.list(model_overrides)) {
    stop(
      "model_overrides 必须是 named list。",
      call. = FALSE
    )
  }
  
  if (length(model_overrides) > 0L) {
    
    if (
      is.null(names(model_overrides)) ||
      any(!nzchar(names(model_overrides)))
    ) {
      stop(
        paste0(
          "model_overrides 的每个函数必须按",
          "目标 PEcAn trait 命名。"
        ),
        call. = FALSE
      )
    }
    
    for (target_i in names(model_overrides)) {
      models[[target_i]] <-
        model_overrides[[target_i]]
    }
  }
  
  
  # ==========================================================
  # 3. 调用原始 PASS bridge
  #
  # 原始 run_pass_ma_to_writer_ready_pecan()
  # 内部还会执行 file.path(ma_root, pftname)，
  # 所以这里传入 PFT 文件夹的上一级目录。
  # ==========================================================
  
  ma_parent_root <- dirname(pft_dir)
  
  result <- run_pass_ma_to_writer_ready_pecan(
    pftname = pftname,
    
    # 传给底层函数的是 TRY_meta_analysis 根目录
    ma_root = ma_parent_root,
    
    bridge_file = bridge_file,
    qc_subdir = qc_subdir,
    out_subdir = out_subdir,
    n_draws = n_draws,
    seed = seed,
    context = context,
    models = models,
    strict = FALSE,
    save_ma_draws_csv = save_ma_draws_csv
  )
  
  
  # ==========================================================
  # 4. 添加全部开启模式的审核结果
  # ==========================================================
  
  result$bridge_mode <-
    "ALL_ENABLED_SCENARIO"
  
  result$assumption_table <-
    .all_enabled_assumption_table(
      context
    )
  
  result$writer_trait_provenance <-
    .all_enabled_provenance(
      result
    )
  
  generated_13 <- result$target_status_13[
    generated_writer_ready == TRUE
  ]
  
  
  # ==========================================================
  # 5. 保存额外审核文件
  # ==========================================================
  
  data.table::fwrite(
    result$assumption_table,
    file.path(
      result$out_dir,
      "all_enabled_context_assumptions.csv"
    )
  )
  
  data.table::fwrite(
    result$writer_trait_provenance,
    file.path(
      result$out_dir,
      "writer_trait_provenance.csv"
    )
  )
  
  # 完整结果
  saveRDS(
    result,
    file.path(
      result$out_dir,
      "pass_ma_to_writer_ready_pecan_result.rds"
    ),
    compress = FALSE
  )
  
  # 再保存一个名称更直接的副本
  saveRDS(
    result,
    file.path(
      result$out_dir,
      "writer_res_all.rds"
    ),
    compress = FALSE
  )
  
  
  # ==========================================================
  # 6. 输出信息
  # ==========================================================
  
  message(
    "ALL-ENABLED PASS bridge finished for ",
    pftname,
    ".\nPFT directory: ",
    pft_dir,
    "\nPASS MA traits: ",
    result$n_pass_traits,
    "\nTotal complete writer-ready traits: ",
    length(result$complete_writer_traits),
    "\nGenerated among 13 candidate targets: ",
    nrow(generated_13),
    "\nOutput directory: ",
    result$out_dir
  )
  
  result
}


# =============================================================================
# Example call
# =============================================================================
#
# source("/path/to/md_to_pecan_bridge_v1.R")
# source("/path/to/run_pass_ma_to_writer_ready_pecan.R")
#
# ALL-ENABLED call requested for the single-PFT bridge.  Every conditional
# switch and all 13 proxy models are enabled; unavailable PASS sources are
# skipped automatically.
# writer_res <- run_pass_ma_to_writer_ready_pecan_all_enabled(
#   pftname = "CroplandPool__Cereal_Croplands",
#   ma_root = "/projectnb/dietzelab/guYANG/TRY_meta_analysis",
#   bridge_file = "/path/to/md_to_pecan_bridge_v1.R",
#   n_draws = 6000L,
#   seed = 20260828L,
#   context_overrides = list(),
#   model_overrides = list()
# )
#
# print(writer_res$complete_writer_traits)
# print(writer_res$target_status_13, nrows = Inf)
# print(writer_res$writer_trait_summary, nrows = Inf)
#
# trait.values <- get_sipnet_trait_values(
#   writer_res,
#   draw_id = writer_res$writer_trait_draws$draw_id[1L]
# )
#
# PEcAn.SIPNET::write.config.SIPNET(
#   defaults = defaults,
#   trait.values = trait.values,
#   settings = settings,
#   run.id = run.id,
#   inputs = inputs,
#   IC = IC
# )








# =============================================================================
# TRY/PEcAn MA *_md posterior draws -> SIPNET-writer-ready PEcAn traits
# =============================================================================
#
# Scope
# -----
# This file starts after PEcAn.MA.  Its input is a wide posterior-draw table:
# one row per draw; columns are beta.o posterior draws named with the user's
# direct PEcAn vnames and/or the 84 intermediate vnames ending in "_md".
#
# Scientific rule
# ---------------
# Removing "_md" is NEVER itself a conversion.  The registry below covers all
# 84 intermediate names and identifies whether a bridge is algebraic,
# conditional, mechanistic, empirical, a tissue proxy, or unavailable.
#
# bridge_md_posteriors_to_pecan() automatically evaluates only relationships
# whose assumptions are explicitly confirmed in `context`.  Any model that
# needs fitted coefficients can be supplied through `models`; otherwise the
# source remains in `unresolved`.
#
# Output
# ------
# - writer_trait_draws: only current SIPNET-v2 writer traits that are finite in
#   every draw; safe to place in PEcAn ensemble.samples.
# - candidate_trait_draws: includes partially filled candidate target columns.
# - rule_attempts: audit of every attempted automatic/custom relationship.
# - md_usage: one row for each of the 84 *_md sources.
# - unresolved: present *_md sources that did not produce a writer trait.
#
# Current writer contract checked against PEcAn develop commit
# 906d1a2652d5cc6e9d03b17bdc7558af16e6d436 (2026-08-19).
# =============================================================================


.md_bridge_require_data_table <- function() {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("需要先安装 data.table。", call. = FALSE)
  }
  invisible(TRUE)
}


sipnet_writer_traits_v2 <- function() {
  c(
    "leafC", "SLA", "Amax", "AmaxFrac", "extinction_coefficient",
    "leaf_respiration_rate_m2", "Vm_low_temp", "psnTOpt",
    "growth_resp_factor", "half_saturation_PAR", "dVPDSlope", "dVpdExp",
    "leaf_turnover_rate", "wueConst", "veg_respiration_Q10",
    "stem_respiration_rate", "root_turnover_rate",
    "fine_root_respiration_Q10", "root_respiration_rate",
    "coarse_root_respiration_Q10", "root_allocation_fraction",
    "wood_allocation_fraction", "leaf_allocation_fraction",
    "wood_turnover_rate", "soil_respiration_Q10", "som_respiration_rate",
    "turn_over_time", "fracLitterRespired", "frozenSoilEff",
    "frozenSoilFolREff", "soilWHC", "immedEvapFrac", "leafWHC",
    "waterRemoveFrac", "fastFlowFrac", "rdConst", "water_drain_frac",
    "GDD", "leafOnReallocFrac", "fracLeafFall", "leafGrowth",
    "c2n_leaf", "c2n_wood", "c2n_fineroot", "kCN",
    "n_volatilization_rate", "n_leaching_frac", "n_fixation_frac_max",
    "n_fix_half_sat", "f_anoxia", "anaerobic_decomp_rate",
    "anaerobic_trans_exp", "soil_methane_rate", "litter_methane_rate"
  )
}


md_to_pecan_bridge_registry <- function() {
  .md_bridge_require_data_table()
  
  row <- function(
    source_md,
    candidate_pecan_trait = NA_character_,
    secondary_writer_traits = "",
    conversion_class,
    bridge_status,
    required_sources = "",
    required_context = "",
    same_entity_required = FALSE,
    formula_or_model = "",
    output_unit = "",
    warning_cn = ""
  ) {
    data.table::data.table(
      source_md = source_md,
      candidate_pecan_trait = candidate_pecan_trait,
      secondary_writer_traits = secondary_writer_traits,
      conversion_class = conversion_class,
      bridge_status = bridge_status,
      required_sources = required_sources,
      required_context = required_context,
      same_entity_required = same_entity_required,
      formula_or_model = formula_or_model,
      output_unit = output_unit,
      warning_cn = warning_cn
    )
  }
  
  rows <- list(
    # -----------------------------------------------------------------------
    # Leaf structure and chemistry (14)
    # -----------------------------------------------------------------------
    row(
      "wood_density_md", "wood_turnover_rate", "",
      "EMPIRICAL_MODEL", "CUSTOM_MODEL_REQUIRED", "", "PFT; fitted coefficients; residual draw",
      FALSE, "log(kwood) = alpha_PFT + beta * log(wood_density) + epsilon",
      "year-1", "不能使用固定反比；需要外部拟合系数和残差。"
    ),
    row(
      "leaf_thickness_md", "SLA", "", "DERIVED_ALGEBRAIC", "AUTO_IF_COMPLETE",
      "leaf_tissue_density_md", "", TRUE,
      "SLA = 1 / ((thickness_mm / 1000) * density_kg_m3)",
      "m2 leaf kg-1 dry mass", "最好在同一 ObservationID 内、MA 前配对计算。"
    ),
    row(
      "leaf_N_mass_md", "c2n_leaf", "Amax", "DERIVED_ALGEBRAIC", "AUTO_IF_COMPLETE",
      "leafC", "", TRUE, "c2n_leaf = 10 * leafC_percent / leaf_N_g_kg",
      "g C g-1 N", "用于 Amax 时需要另行拟合叶氮—光合模型。"
    ),
    row(
      "leaf_P_mass_md", "Amax", "", "EMPIRICAL_MODEL", "CUSTOM_MODEL_REQUIRED",
      "leaf_N_mass_md; SLA", "PFT; measurement conditions; fitted coefficients; residual draw",
      FALSE, "validated nutrient-limitation model", "umol CO2 m-2 leaf s-1",
      "叶磷不能单独确定 Amax。"
    ),
    row(
      "LDMC_md", "SLA", "leaf_turnover_rate", "EMPIRICAL_MODEL", "CUSTOM_MODEL_REQUIRED",
      "", "PFT; fitted coefficients; residual draw", FALSE,
      "log(target) = alpha_PFT + beta * logit(LDMC) + epsilon", "target-specific",
      "不能用固定系数把 LDMC 转成 SLA 或叶周转。"
    ),
    row(
      "leaf_tissue_density_md", "SLA", "", "DERIVED_ALGEBRAIC", "AUTO_IF_COMPLETE",
      "leaf_thickness_md", "", TRUE,
      "SLA = 1 / ((thickness_mm / 1000) * density_kg_m3)",
      "m2 leaf kg-1 dry mass", "最好在同一 ObservationID 内、MA 前配对计算。"
    ),
    row(
      "leaf_lignin_md", "turn_over_time", "leaf_turnover_rate", "EMPIRICAL_MODEL",
      "CUSTOM_MODEL_REQUIRED", "litter_N_mass_md; litter_cellulose_md", "climate; tissue offset; fitted coefficients; residual draw",
      FALSE, "validated litter-decomposition model", "year-1",
      "绿叶木质素不是凋落物分解率。"
    ),
    row(
      "leaf_cellulose_md", "turn_over_time", "", "EMPIRICAL_MODEL", "CUSTOM_MODEL_REQUIRED",
      "leaf_lignin_md; litter_N_mass_md", "climate; tissue offset; fitted coefficients; residual draw",
      FALSE, "validated litter-decomposition model", "year-1", "只能作分解模型协变量。"
    ),
    row(
      "leaf_hemicellulose_md", "turn_over_time", "", "EMPIRICAL_MODEL", "CUSTOM_MODEL_REQUIRED",
      "leaf_lignin_md; litter_N_mass_md", "climate; tissue offset; fitted coefficients; residual draw",
      FALSE, "validated litter-decomposition model", "year-1", "只能作分解模型协变量。"
    ),
    row(
      "leaf_NP_ratio_md", "Amax", "", "EMPIRICAL_MODEL", "CUSTOM_MODEL_REQUIRED",
      "leaf_N_mass_md; leaf_P_mass_md", "PFT; fitted coefficients; residual draw", FALSE,
      "validated N/P-limitation model", "umol CO2 m-2 leaf s-1", "N:P 不是 Amax。"
    ),
    row(
      "leaf_N_area_md", "c2n_leaf", "Amax", "DERIVED_ALGEBRAIC", "AUTO_IF_COMPLETE",
      "leafC_area", "", TRUE, "c2n_leaf = leafC_area / leaf_N_area",
      "g C g-1 N", "与 PNUE 联合时也可条件性生成 Amax。"
    ),
    row(
      "leaf_P_area_md", "Amax", "", "EMPIRICAL_MODEL", "CUSTOM_MODEL_REQUIRED",
      "leaf_N_area_md", "PFT; measurement conditions; fitted coefficients; residual draw",
      FALSE, "validated nutrient-limitation model", "umol CO2 m-2 leaf s-1", "叶磷不能单独确定 Amax。"
    ),
    row(
      "leaf_construction_cost_mass_md", "growth_resp_factor", "", "MECHANISTIC_MODEL",
      "CUSTOM_MODEL_REQUIRED", "leafC", "construction-cost definition; carbon accounting convention",
      FALSE, "growth respiration C / total assimilated C", "fraction",
      "必须核对 construction cost 是否包含保留在生物量中的碳。"
    ),
    row(
      "leaf_construction_cost_area_md", "growth_resp_factor", "", "MECHANISTIC_MODEL",
      "CUSTOM_MODEL_REQUIRED", "SLA; leafC", "construction-cost definition; carbon accounting convention",
      TRUE, "cost_mass = cost_area * SLA / 1000; then carbon-accounting model", "fraction",
      "面积基准先转质量基准，再运行建造成本模型。"
    ),
    
    # -----------------------------------------------------------------------
    # Photosynthesis, gas exchange, and leaf respiration (17)
    # -----------------------------------------------------------------------
    row(
      "photosynthesis_rate_area_observed_md", "Amax", "", "CONDITIONAL_DIRECT",
      "AUTO_WITH_CONTEXT", "", "photosynthesis_area_is_amax=TRUE", FALSE,
      "Amax = observed area-basis photosynthesis", "umol CO2 m-2 leaf s-1",
      "必须确认成熟叶、光饱和、无胁迫、最大净光合。"
    ),
    row(
      "photosynthesis_rate_mass_observed_md", "Amax", "", "DERIVED_ALGEBRAIC",
      "AUTO_WITH_CONTEXT", "SLA", "photosynthesis_mass_is_amax=TRUE", FALSE,
      "Amax_area = Amax_mass / SLA", "umol CO2 m-2 leaf s-1",
      "测量协议必须符合 Amax 定义。"
    ),
    row(
      "ci_md", "wueConst", "dVPDSlope", "MECHANISTIC_MODEL", "CUSTOM_MODEL_REQUIRED",
      "", "ambient_CO2; VPD; stomatal model", TRUE,
      "ci_ca = ci / ca, followed by a validated stomatal model", "target-specific",
      "ci/ca 本身不是 SIPNET-v2 writer trait。"
    ),
    row(
      "leaf_transpiration_area_md", "wueConst", "", "DERIVED_ALGEBRAIC",
      "AUTO_WITH_CONTEXT", "photosynthesis_rate_area_observed_md", "gas_exchange_conditions_match=TRUE; vpd_kpa",
      TRUE, "wueConst = 2.442 * (A / E) * VPD", "mg CO2 g-1 H2O kPa",
      "A、E 和 VPD 必须来自相同条件；独立 MA 后组合是近似。"
    ),
    row(
      "leaf_dark_respiration_area_Tmeas_md", "leaf_respiration_rate_m2", "",
      "DERIVED_ALGEBRAIC", "AUTO_WITH_CONTEXT", "leaf_respiration_Q10_md",
      "leaf_respiration_tmeas_c; leaf_respiration_tref_c", FALSE,
      "Rd_ref = Rd_meas * Q10^((Tref - Tmeas) / 10)", "umol CO2 m-2 leaf s-1",
      "Tref 必须与用于 writer 比值的 Amax 定义一致。"
    ),
    row(
      "leaf_dark_respiration_mass_Tmeas_md", "leaf_respiration_rate_m2", "",
      "DERIVED_ALGEBRAIC", "AUTO_WITH_CONTEXT", "SLA; leaf_respiration_Q10_md",
      "leaf_respiration_tmeas_c; leaf_respiration_tref_c", FALSE,
      "Rd_area_ref = Rd_mass * Q10^((Tref - Tmeas) / 10) / SLA",
      "umol CO2 m-2 leaf s-1", "质量和面积基准必须用同一 SLA。"
    ),
    row(
      "Vcmax_mass_Tref_md", "Amax", "", "MECHANISTIC_MODEL", "CUSTOM_MODEL_REQUIRED",
      "Jmax_mass_Tref_md; Rd; ci or ca; SLA", "C3/C4 pathway; Tref; PAR; FvCB constants",
      TRUE, "FvCB model", "umol CO2 m-2 leaf s-1", "不能使用固定 Vcmax:Amax 倍率。"
    ),
    row(
      "Vcmax_area_Tref_md", "Amax", "", "MECHANISTIC_MODEL", "CUSTOM_MODEL_REQUIRED",
      "Jmax_area_Tref_md; Rd; ci or ca", "C3/C4 pathway; Tref; PAR; FvCB constants",
      TRUE, "FvCB model", "umol CO2 m-2 leaf s-1", "Cereal PFT 可能混有 C3/C4。"
    ),
    row(
      "LUE_md", "half_saturation_PAR", "AmaxFrac", "MECHANISTIC_MODEL", "AUTO_WITH_CONTEXT",
      "Amax", "light_response_model=rectangular_hyperbola; par_basis_compatible=TRUE",
      FALSE, "halfSatPAR = Amax / initial_slope, then umol s-1 to mol day-1",
      "mol photons m-2 ground day-1", "需要指定光响应曲线和冠层/PAR 基准。"
    ),
    row(
      "PNUE_md", "Amax", "", "DERIVED_ALGEBRAIC", "AUTO_WITH_CONTEXT",
      "leaf_N_area_md or leaf_N_mass_md; SLA", "pnue_conditions_match=TRUE", TRUE,
      "Amax = PNUE * leaf_N_area; or PNUE * leaf_N_mass / SLA",
      "umol CO2 m-2 leaf s-1", "PNUE 与 N 必须对应相同组织和光合条件。"
    ),
    row(
      "Jmax_area_Tref_md", "Amax", "", "MECHANISTIC_MODEL", "CUSTOM_MODEL_REQUIRED",
      "Vcmax_area_Tref_md; Rd; ci or ca", "C3/C4 pathway; Tref; PAR; FvCB constants",
      TRUE, "FvCB model", "umol CO2 m-2 leaf s-1", "Jmax 本身不是 writer trait。"
    ),
    row(
      "leaf_respiration_Q10_md", "veg_respiration_Q10", "fine_root_respiration_Q10|coarse_root_respiration_Q10",
      "TISSUE_SCOPE_PROXY", "AUTO_ONLY_IF_PROXY_ALLOWED", "", "explicit tissue-proxy flags",
      FALSE, "copy Q10 only under explicit cross-tissue proxy assumption", "unitless Q10",
      "默认只用于叶暗呼吸温度校正，不自动当全植被或根 Q10。"
    ),
    row(
      "WUE_md", "wueConst", "dVPDSlope", "DERIVED_ALGEBRAIC", "AUTO_WITH_CONTEXT",
      "", "wue_definition_matches_sipnet=TRUE; vpd_kpa", FALSE,
      "wueConst = 2.442 * WUE_umol_per_mmol * VPD_kPa",
      "mg CO2 g-1 H2O kPa", "必须核对 TRY WUE 是瞬时净光合/蒸腾定义。"
    ),
    row(
      "Jmax_mass_Tref_md", "Amax", "", "MECHANISTIC_MODEL", "CUSTOM_MODEL_REQUIRED",
      "Vcmax_mass_Tref_md; Rd; ci or ca; SLA", "C3/C4 pathway; Tref; PAR; FvCB constants",
      TRUE, "FvCB model", "umol CO2 m-2 leaf s-1", "先统一面积/质量基准。"
    ),
    row(
      "Rd_Vcmax_fraction_md", "leaf_respiration_rate_m2", "", "DERIVED_ALGEBRAIC",
      "AUTO_WITH_CONTEXT", "Vcmax_area_Tref_md or Vcmax_mass_Tref_md; SLA",
      "rd_vcmax_same_reference=TRUE", TRUE,
      "Rd_area = Rd_Vcmax_fraction * Vcmax_area", "umol CO2 m-2 leaf s-1",
      "Rd 与 Vcmax 必须同温度、同面积基准、定义一致。"
    ),
    row(
      "mesophyll_conductance_md", "Amax", "", "MECHANISTIC_MODEL", "CUSTOM_MODEL_REQUIRED",
      "Vcmax; Jmax; Rd; ci", "gm pressure/mole-fraction convention; Tref; PAR; pathway",
      TRUE, "FvCB with Cc = Ci - A/gm", "umol CO2 m-2 leaf s-1",
      "必须回到原文确认 gm 的压力基准。"
    ),
    row(
      "quantum_yield_md", "half_saturation_PAR", "AmaxFrac", "MECHANISTIC_MODEL",
      "AUTO_WITH_CONTEXT", "Amax", "light_response_model=rectangular_hyperbola; par_basis_compatible=TRUE",
      FALSE, "halfSatPAR = Amax / quantum_yield, then umol s-1 to mol day-1",
      "mol photons m-2 ground day-1", "writer 中 Jmax/quantum 的旧代码仍未启用。"
    ),
    
    # -----------------------------------------------------------------------
    # Roots (18)
    # -----------------------------------------------------------------------
    row(
      "root_tissue_density_md", "root_turnover_rate", "root_respiration_rate",
      "TISSUE_SCOPE_PROXY", "CUSTOM_MODEL_REQUIRED", "root_diameter_md; root_N_mass_md",
      "root-order definition; PFT; fitted coefficients; residual draw", FALSE,
      "validated root-economics model", "target-specific", "不能单独决定细根周转或呼吸。"
    ),
    row(
      "root_diameter_md", "root_turnover_rate", "root_respiration_rate",
      "TISSUE_SCOPE_PROXY", "CUSTOM_MODEL_REQUIRED", "root_tissue_density_md; root_N_mass_md",
      "root-order definition; PFT; fitted coefficients; residual draw", FALSE,
      "validated root-economics model", "target-specific", "根径小于 2 mm 也不自动等于吸收根。"
    ),
    row(
      "root_N_mass_md", "c2n_fineroot", "root_respiration_rate|root_turnover_rate",
      "TISSUE_SCOPE_PROXY", "AUTO_WITH_CONTEXT", "root_C_md", "generic_root_matches_fine_root=TRUE",
      TRUE, "c2n_fineroot = 10 * root_C_percent / root_N_g_kg", "g C g-1 N",
      "generic-root 组织范围必须与 SIPNET fine-root pool 一致。"
    ),
    row(
      "root_C_md", "c2n_fineroot", "root_respiration_rate", "TISSUE_SCOPE_PROXY",
      "AUTO_WITH_CONTEXT", "root_N_mass_md", "generic_root_matches_fine_root=TRUE",
      TRUE, "c2n_fineroot = 10 * root_C_percent / root_N_g_kg", "g C g-1 N",
      "generic-root 组织范围必须与 SIPNET fine-root pool 一致。"
    ),
    row(
      "root_RDMC_md", "root_turnover_rate", "root_respiration_rate", "TISSUE_SCOPE_PROXY",
      "CUSTOM_MODEL_REQUIRED", "root_diameter_md; root_N_mass_md", "PFT; fitted coefficients; residual draw",
      FALSE, "validated root-economics model", "target-specific", "不能固定换算。"
    ),
    row(
      "fine_root_C_md", "c2n_fineroot", "root_respiration_rate", "DERIVED_ALGEBRAIC",
      "AUTO_IF_COMPLETE", "fine_root_N_mass_md", "", TRUE,
      "c2n_fineroot = 10 * fine_root_C_percent / fine_root_N_g_kg", "g C g-1 N",
      "呼吸若需按根 C 归一化，应在自定义模型中显式处理。"
    ),
    row(
      "fine_root_N_mass_md", "c2n_fineroot", "root_respiration_rate|root_turnover_rate",
      "DERIVED_ALGEBRAIC", "AUTO_IF_COMPLETE", "fine_root_C_md", "", TRUE,
      "c2n_fineroot = 10 * fine_root_C_percent / fine_root_N_g_kg", "g C g-1 N",
      "最好按同一 ObservationID 配对后再做 MA。"
    ),
    row(
      "root_respiration_mass_Tmeas_generic_md", "root_respiration_rate", "",
      "TISSUE_SCOPE_PROXY", "AUTO_WITH_CONTEXT", "",
      "generic_root_matches_fine_root=TRUE; generic_root_gas_is_co2=TRUE; root_respiration_tmeas_c; root_respiration_tref_c; root_respiration_q10",
      FALSE, "R_ref = R_meas * Q10^((Tref - Tmeas) / 10)",
      "umol CO2 kg-1 tissue s-1 at 25 C", "O2 或 gas-unspecified 数据不能在 bridge 中默认为 CO2。"
    ),
    row(
      "fine_root_SRL_md", "root_respiration_rate", "root_turnover_rate", "EMPIRICAL_MODEL",
      "CUSTOM_MODEL_REQUIRED", "fine_root_N_mass_md; fine_root_tissue_density_md; fine_root_diameter_md",
      "PFT; fitted coefficients; residual draw", FALSE, "validated multidimensional root-economics model",
      "target-specific", "不能单变量固定换算。"
    ),
    row(
      "fine_root_RDMC_md", "root_turnover_rate", "root_respiration_rate", "EMPIRICAL_MODEL",
      "CUSTOM_MODEL_REQUIRED", "fine_root_N_mass_md; fine_root_diameter_md",
      "PFT; fitted coefficients; residual draw", FALSE, "validated root-economics model",
      "target-specific", "不能单变量固定换算。"
    ),
    row(
      "fine_root_P_mass_md", "root_respiration_rate", "root_turnover_rate", "EMPIRICAL_MODEL",
      "CUSTOM_MODEL_REQUIRED", "fine_root_N_mass_md; fine_root_diameter_md",
      "PFT; fitted coefficients; residual draw", FALSE, "validated root-nutrient model",
      "target-specific", "只能作模型协变量。"
    ),
    row(
      "root_P_mass_md", "root_respiration_rate", "root_turnover_rate", "TISSUE_SCOPE_PROXY",
      "CUSTOM_MODEL_REQUIRED", "root_N_mass_md; root_diameter_md",
      "root-order definition; PFT; fitted coefficients; residual draw", FALSE,
      "validated root-nutrient model", "target-specific", "generic-root P 不能确定性生成 writer trait。"
    ),
    row(
      "root_SRL_md", "root_respiration_rate", "root_turnover_rate", "TISSUE_SCOPE_PROXY",
      "CUSTOM_MODEL_REQUIRED", "root_N_mass_md; root_tissue_density_md; root_diameter_md",
      "root-order definition; PFT; fitted coefficients; residual draw", FALSE,
      "validated root-economics model", "target-specific", "需要 generic-root 到 fine-root 的组织偏差。"
    ),
    row(
      "c2n_root_generic_md", "c2n_fineroot", "", "CONDITIONAL_DIRECT", "AUTO_WITH_CONTEXT",
      "", "generic_root_matches_fine_root=TRUE", FALSE, "c2n_fineroot = c2n_root_generic",
      "g C g-1 N", "只有组织范围一致时才允许。"
    ),
    row(
      "fine_root_diameter_md", "root_respiration_rate", "root_turnover_rate", "EMPIRICAL_MODEL",
      "CUSTOM_MODEL_REQUIRED", "fine_root_SRL_md; fine_root_tissue_density_md; fine_root_N_mass_md",
      "PFT; fitted coefficients; residual draw", FALSE, "validated root-economics model",
      "target-specific", "不能单变量固定换算。"
    ),
    row(
      "fine_root_tissue_density_md", "root_turnover_rate", "root_respiration_rate", "EMPIRICAL_MODEL",
      "CUSTOM_MODEL_REQUIRED", "fine_root_SRL_md; fine_root_diameter_md; fine_root_N_mass_md",
      "PFT; fitted coefficients; residual draw", FALSE, "validated root-economics model",
      "target-specific", "不能单变量固定换算。"
    ),
    row(
      "fine_root_respiration_mass_Tmeas_md", "root_respiration_rate", "",
      "MECHANISTIC_MODEL", "AUTO_WITH_CONTEXT", "",
      "fine_root_gas_is_co2=TRUE; fine_root_respiration_tmeas_c; root_respiration_tref_c; root_respiration_q10",
      FALSE, "R_ref = R_meas * Q10^((Tref - Tmeas) / 10)",
      "umol CO2 kg-1 tissue s-1 at 25 C", "nmol g-1 s-1 数值等于 umol kg-1 s-1；O2 需 RQ。"
    ),
    row(
      "root_turnover_rate_generic_md", "root_turnover_rate", "", "CONDITIONAL_DIRECT",
      "AUTO_WITH_CONTEXT", "", "generic_root_matches_fine_root=TRUE", FALSE,
      "root_turnover_rate = root_turnover_rate_generic", "year-1",
      "只有采样组织等同 SIPNET fine-root pool 时才允许。"
    ),
    
    # -----------------------------------------------------------------------
    # Litter and coarse woody debris (15)
    # -----------------------------------------------------------------------
    row(
      "litter_decomposition_rate_observed_md", "turn_over_time", "", "MECHANISTIC_INVERSION",
      "AUTO_WITH_CONTEXT", "", "litter_rate_is_first_order_k=TRUE; litter_rate_matches_sipnet_reference=TRUE",
      FALSE, "turn_over_time = verified first-order litter k", "year-1",
      "若原值是区间质量损失，必须先用 -log(1-f)/dt 反演。"
    ),
    row(
      "litter_N_mass_md", "turn_over_time", "", "EMPIRICAL_MODEL", "CUSTOM_MODEL_REQUIRED",
      "litter_CN_md; litter_lignin_md", "climate; fitted coefficients; residual draw", FALSE,
      "validated litter-decomposition model", "year-1", "不能单独决定分解率。"
    ),
    row(
      "litter_C_mass_md", "turn_over_time", "", "SUPPORTING_ONLY", "CUSTOM_MODEL_REQUIRED",
      "litter_N_mass_md", "climate; fitted coefficients; residual draw", TRUE,
      "derive litter C:N, then validated decomposition model", "year-1",
      "litter C:N 绝不等于 writer 的 kCN。"
    ),
    row(
      "litter_lignin_md", "turn_over_time", "", "EMPIRICAL_MODEL", "CUSTOM_MODEL_REQUIRED",
      "litter_N_mass_md; litter_cellulose_md", "climate; fitted coefficients; residual draw", FALSE,
      "log(k0) = X beta + epsilon", "year-1", "必须传播经验模型残差。"
    ),
    row(
      "litter_tannin_md", "turn_over_time", "", "EMPIRICAL_MODEL", "CUSTOM_MODEL_REQUIRED",
      "litter_N_mass_md; litter_lignin_md", "climate; fitted coefficients; residual draw", FALSE,
      "validated litter-decomposition model", "year-1", "只能作模型协变量。"
    ),
    row(
      "litter_cellulose_md", "turn_over_time", "", "EMPIRICAL_MODEL", "CUSTOM_MODEL_REQUIRED",
      "litter_N_mass_md; litter_lignin_md", "climate; fitted coefficients; residual draw", FALSE,
      "validated litter-decomposition model", "year-1", "只能作模型协变量。"
    ),
    row(
      "litter_P_mass_md", "turn_over_time", "", "EMPIRICAL_MODEL", "CUSTOM_MODEL_REQUIRED",
      "litter_N_mass_md; litter_lignin_md", "climate; fitted coefficients; residual draw", FALSE,
      "validated litter-decomposition model", "year-1", "只能作模型协变量。"
    ),
    row(
      "litter_SLA_md", "turn_over_time", "", "EMPIRICAL_MODEL", "CUSTOM_MODEL_REQUIRED",
      "litter_lignin_md; litter_N_mass_md", "climate; fitted coefficients; residual draw", FALSE,
      "validated litter-structure model", "year-1", "不能固定换算。"
    ),
    row(
      "litter_CN_md", "turn_over_time", "", "EMPIRICAL_MODEL", "CUSTOM_MODEL_REQUIRED",
      "litter_lignin_md", "climate; fitted coefficients; residual draw", FALSE,
      "validated C:N-limited decomposition model", "year-1",
      "litter_CN 不是 kCN；禁止直接赋值给 kCN。"
    ),
    row(
      "litter_decomposition_k_observed_md", "turn_over_time", "", "CONDITIONAL_DIRECT",
      "AUTO_WITH_CONTEXT", "", "litter_rate_is_first_order_k=TRUE; litter_rate_matches_sipnet_reference=TRUE",
      FALSE, "turn_over_time = verified first-order litter k", "year-1",
      "writer 名称 turn_over_time 实际写入 litterBreakdownRate，不是周转时间。"
    ),
    row(
      "cwd_decomposition_k_md", "turn_over_time", "", "TISSUE_SCOPE_PROXY", "CUSTOM_MODEL_REQUIRED",
      "cwd_CN_md; wood_density_md", "CWD-to-litter pool offset; climate; residual draw", FALSE,
      "validated CWD-to-SIPNET-litter offset model", "year-1",
      "SIPNET v2 没有独立 CWD pool；CWD k 也不是活木周转。"
    ),
    row(
      "cwd_decomposition_rate_md", "turn_over_time", "", "TISSUE_SCOPE_PROXY", "CUSTOM_MODEL_REQUIRED",
      "cwd_CN_md; wood_density_md", "rate definition; CWD-to-litter pool offset; climate", FALSE,
      "rate-definition inversion plus pool-offset model", "year-1", "不能直接复制。"
    ),
    row(
      "cwd_N_mass_md", "turn_over_time", "", "SUPPORTING_ONLY", "CUSTOM_MODEL_REQUIRED",
      "cwd_C_mass_md", "CWD-to-litter pool offset; climate", TRUE,
      "derive CWD C:N, then pool-offset model", "year-1", "只能作 CWD 模型协变量。"
    ),
    row(
      "cwd_C_mass_md", "turn_over_time", "", "SUPPORTING_ONLY", "CUSTOM_MODEL_REQUIRED",
      "cwd_N_mass_md", "CWD-to-litter pool offset; climate", TRUE,
      "derive CWD C:N, then pool-offset model", "year-1", "只能作 CWD 模型协变量。"
    ),
    row(
      "cwd_CN_md", "turn_over_time", "", "TISSUE_SCOPE_PROXY", "CUSTOM_MODEL_REQUIRED",
      "cwd_decomposition_k_md", "CWD-to-litter pool offset; climate", FALSE,
      "validated CWD-to-SIPNET-litter offset model", "year-1", "不能直接复制。"
    ),
    
    # -----------------------------------------------------------------------
    # Stem, wood, and allocation-related traits (10)
    # -----------------------------------------------------------------------
    row(
      "stem_CN_md", "c2n_wood", "", "TISSUE_SCOPE_PROXY", "AUTO_WITH_CONTEXT",
      "", "stem_represents_sipnet_wood_pool=TRUE", FALSE, "c2n_wood = stem_CN",
      "g C g-1 N", "作物茎或全 wood pool 定义不一致时不能直接使用。"
    ),
    row(
      "sapwood_density_md", "wood_turnover_rate", "", "EMPIRICAL_MODEL", "CUSTOM_MODEL_REQUIRED",
      "", "PFT; fitted coefficients; residual draw", FALSE,
      "log(kwood) = alpha_PFT + beta * log(sapwood_density) + epsilon", "year-1",
      "需要外部拟合系数。"
    ),
    row(
      "stem_C_md", "c2n_wood", "", "DERIVED_ALGEBRAIC", "AUTO_WITH_CONTEXT",
      "stem_N_mass_md", "stem_represents_sipnet_wood_pool=TRUE", TRUE,
      "c2n_wood = 10 * stem_C_percent / stem_N_g_kg", "g C g-1 N",
      "最好在同一 ObservationID 内配对。"
    ),
    row(
      "stem_N_mass_md", "c2n_wood", "stem_respiration_rate", "DERIVED_ALGEBRAIC",
      "AUTO_WITH_CONTEXT", "stem_C_md", "stem_represents_sipnet_wood_pool=TRUE", TRUE,
      "c2n_wood = 10 * stem_C_percent / stem_N_g_kg", "g C g-1 N",
      "N 本身不能生成茎呼吸率。"
    ),
    row(
      "branch_density_md", "wood_turnover_rate", "", "TISSUE_SCOPE_PROXY", "CUSTOM_MODEL_REQUIRED",
      "", "branch-to-whole-wood offset; PFT; residual draw", FALSE,
      "validated branch-density wood-strategy model", "year-1", "枝密度不能当整体木密度。"
    ),
    row(
      "bark_thickness_md", "wood_turnover_rate", "", "EMPIRICAL_MODEL", "CUSTOM_MODEL_REQUIRED",
      "wood_density_md", "fire history; stem diameter; PFT; fitted coefficients; residual draw", FALSE,
      "validated survival/wood-turnover model", "year-1", "普通 crop 建议不启用此弱代理。"
    ),
    row(
      "coarse_fine_root_mass_ratio_md", NA_character_, "", "NO_WRITER_ROUTE", "NOT_CONVERTIBLE_HERE",
      "", "", TRUE, "fineRootFrac = 1/(1+r), coarseRootFrac = r/(1+r) only within the root IC pool",
      "", "只能约束 IC 粗细根库比例，不能生成 NPP root_allocation_fraction。"
    ),
    row(
      "sapwood_specific_conductivity_md", "waterRemoveFrac", "dVPDSlope", "MECHANISTIC_MODEL",
      "CUSTOM_MODEL_REQUIRED", "leaf_hydraulic_conductance_md", "sapwood area:leaf area; LAI; root conductance; water potentials; VPD",
      FALSE, "whole-plant hydraulic model", "target-specific", "不能按名称直接转换。"
    ),
    row(
      "wood_N_mass_md", "c2n_wood", "stem_respiration_rate", "DERIVED_ALGEBRAIC",
      "CUSTOM_MODEL_REQUIRED", "matching wood_C", "same wood tissue", TRUE,
      "c2n_wood = 10 * wood_C_percent / wood_N_g_kg", "g C g-1 N",
      "当前列表没有严格匹配的 wood_C，不能默认用 stem_C。"
    ),
    row(
      "stem_DMC_md", "wood_turnover_rate", "", "EMPIRICAL_MODEL", "CUSTOM_MODEL_REQUIRED",
      "", "stem tissue type; PFT; fitted coefficients; residual draw", FALSE,
      "validated stem-DMC wood-strategy model", "year-1", "DMC 不是 wood density。"
    ),
    
    # -----------------------------------------------------------------------
    # Lifespan, rooting depth, and plant hydraulics (10)
    # -----------------------------------------------------------------------
    row(
      "leaf_lifespan_md", "leaf_turnover_rate", "", "DERIVED_ALGEBRAIC", "AUTO_WITH_CONTEXT",
      "", "leaf_lifespan_inverse_valid=TRUE", FALSE, "leaf_turnover_rate = 1 / lifespan_year",
      "year-1", "一年生 cereal/落叶 PFT 需防止与收获或落叶事件重复计数。"
    ),
    row(
      "osmotic_potential_full_turgor_md", "dVPDSlope", "waterRemoveFrac", "MECHANISTIC_MODEL",
      "CUSTOM_MODEL_REQUIRED", "osmotic_potential_tlp_md", "pressure-volume traits; VPD; conductance; fitted response",
      FALSE, "pressure-volume and stomatal-response model", "target-specific", "MPa 不能直接改名为 dVPDSlope。"
    ),
    row(
      "osmotic_potential_tlp_md", "dVPDSlope", "waterRemoveFrac", "EMPIRICAL_MODEL",
      "CUSTOM_MODEL_REQUIRED", "", "VPD; stomatal conductance; soil water potential; fitted response",
      FALSE, "validated drought-response model", "target-specific", "不能直接复制。"
    ),
    row(
      "leaf_WSC_md", "waterRemoveFrac", "dVPDSlope", "MECHANISTIC_MODEL", "CUSTOM_MODEL_REQUIRED",
      "leaf_hydraulic_conductance_md", "LAI; whole-plant hydraulics; timestep", FALSE,
      "whole-plant hydraulic storage model", "target-specific",
      "leaf_WSC 是叶水力电容，绝不等于 writer leafWHC/leafPoolDepth。"
    ),
    row(
      "rooting_depth_md", "soilWHC", "", "MECHANISTIC_MODEL", "AUTO_WITH_CONTEXT",
      "", "allow_root_depth_soilwhc_approx=TRUE; soil_water_capacity_fraction", FALSE,
      "soilWHC_cm = root_depth_m * 100 * soil_water_capacity_fraction",
      "cm water", "严格做法应积分土壤剖面；soil_physics 文件还可能覆盖该值。"
    ),
    row(
      "leaf_hydraulic_conductance_md", "waterRemoveFrac", "dVPDSlope", "MECHANISTIC_MODEL",
      "CUSTOM_MODEL_REQUIRED", "sapwood_specific_conductivity_md", "root conductance; LAI; VPD; water potentials",
      FALSE, "whole-plant hydraulic model", "target-specific", "不能直接复制。"
    ),
    row(
      "fine_root_rooting_depth_md", "soilWHC", "", "MECHANISTIC_MODEL", "AUTO_WITH_CONTEXT",
      "", "allow_root_depth_soilwhc_approx=TRUE; soil_water_capacity_fraction", FALSE,
      "soilWHC_cm = fine_root_depth_m * 100 * soil_water_capacity_fraction",
      "cm water", "优先代表吸收根层；严格做法应积分土壤剖面。"
    ),
    row(
      "midday_leaf_water_potential_md", "dVPDSlope", "waterRemoveFrac", "SUPPORTING_ONLY",
      "CUSTOM_MODEL_REQUIRED", "predawn_leaf_water_potential_md", "VPD; transpiration; measurement time; fitted hydraulic model",
      TRUE, "validated hydraulic response model", "target-specific", "单次 midday 水势不是模型参数。"
    ),
    row(
      "predawn_leaf_water_potential_md", "dVPDSlope", "waterRemoveFrac", "SUPPORTING_ONLY",
      "CUSTOM_MODEL_REQUIRED", "midday_leaf_water_potential_md", "VPD; transpiration; measurement time; fitted hydraulic model",
      TRUE, "validated hydraulic response model", "target-specific", "水势状态不能直接生成 soilWHC。"
    ),
    row(
      "leaf_turgor_loss_point_md", "dVPDSlope", "waterRemoveFrac", "EMPIRICAL_MODEL",
      "CUSTOM_MODEL_REQUIRED", "", "PFT; VPD; stomatal conductance; soil moisture; fitted response",
      FALSE, "validated drought-response model", "target-specific", "MPa 不能直接改名为 dVPDSlope。"
    )
  )
  
  out <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
  
  expected_md <- c(
    "wood_density_md", "leaf_thickness_md", "leaf_N_mass_md", "leaf_P_mass_md",
    "LDMC_md", "leaf_tissue_density_md", "leaf_lignin_md", "leaf_cellulose_md",
    "leaf_hemicellulose_md", "leaf_NP_ratio_md", "leaf_N_area_md", "leaf_P_area_md",
    "leaf_construction_cost_mass_md", "leaf_construction_cost_area_md",
    "photosynthesis_rate_area_observed_md", "photosynthesis_rate_mass_observed_md",
    "ci_md", "leaf_transpiration_area_md", "leaf_dark_respiration_area_Tmeas_md",
    "leaf_dark_respiration_mass_Tmeas_md", "Vcmax_mass_Tref_md", "Vcmax_area_Tref_md",
    "LUE_md", "PNUE_md", "Jmax_area_Tref_md", "leaf_respiration_Q10_md",
    "WUE_md", "Jmax_mass_Tref_md", "Rd_Vcmax_fraction_md", "mesophyll_conductance_md",
    "quantum_yield_md", "root_tissue_density_md", "root_diameter_md", "root_N_mass_md",
    "root_C_md", "root_RDMC_md", "fine_root_C_md", "fine_root_N_mass_md",
    "root_respiration_mass_Tmeas_generic_md", "fine_root_SRL_md", "fine_root_RDMC_md",
    "fine_root_P_mass_md", "root_P_mass_md", "root_SRL_md", "c2n_root_generic_md",
    "fine_root_diameter_md", "fine_root_tissue_density_md",
    "fine_root_respiration_mass_Tmeas_md", "root_turnover_rate_generic_md",
    "litter_decomposition_rate_observed_md", "litter_N_mass_md", "litter_C_mass_md",
    "litter_lignin_md", "litter_tannin_md", "litter_cellulose_md", "litter_P_mass_md",
    "litter_SLA_md", "litter_CN_md", "litter_decomposition_k_observed_md",
    "cwd_decomposition_k_md", "cwd_decomposition_rate_md", "cwd_N_mass_md",
    "cwd_C_mass_md", "cwd_CN_md", "stem_CN_md", "sapwood_density_md", "stem_C_md",
    "stem_N_mass_md", "branch_density_md", "bark_thickness_md",
    "coarse_fine_root_mass_ratio_md", "sapwood_specific_conductivity_md",
    "wood_N_mass_md", "stem_DMC_md", "leaf_lifespan_md",
    "osmotic_potential_full_turgor_md", "osmotic_potential_tlp_md", "leaf_WSC_md",
    "rooting_depth_md", "leaf_hydraulic_conductance_md", "fine_root_rooting_depth_md",
    "midday_leaf_water_potential_md", "predawn_leaf_water_potential_md",
    "leaf_turgor_loss_point_md"
  )
  
  if (nrow(out) != length(expected_md) || anyDuplicated(out$source_md)) {
    stop("内部错误：*_md registry 行数或唯一性不正确。", call. = FALSE)
  }
  missing <- setdiff(expected_md, out$source_md)
  extra <- setdiff(out$source_md, expected_md)
  if (length(missing) > 0L || length(extra) > 0L) {
    stop(
      "内部错误：*_md registry 覆盖不完整。 missing=",
      paste(missing, collapse = ", "),
      "; extra=",
      paste(extra, collapse = ", "),
      call. = FALSE
    )
  }
  
  writer_traits <- sipnet_writer_traits_v2()
  invalid_targets <- unique(out[
    !is.na(candidate_pecan_trait) &
      !candidate_pecan_trait %in% writer_traits,
    candidate_pecan_trait
  ])
  if (length(invalid_targets) > 0L) {
    stop(
      "registry 含非 writer target：",
      paste(invalid_targets, collapse = ", "),
      call. = FALSE
    )
  }
  
  out[]
}


validate_md_to_pecan_bridge_registry <- function(registry = md_to_pecan_bridge_registry()) {
  .md_bridge_require_data_table()
  required <- c(
    "source_md", "candidate_pecan_trait", "secondary_writer_traits",
    "conversion_class", "bridge_status", "required_sources",
    "required_context", "same_entity_required", "formula_or_model",
    "output_unit", "warning_cn"
  )
  missing <- setdiff(required, names(registry))
  if (length(missing) > 0L) {
    stop("registry 缺少列：", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(registry$source_md)) {
    stop("registry$source_md 必须唯一。", call. = FALSE)
  }
  if (nrow(registry) != 84L) {
    stop("当前 registry 应恰好包含 84 个 *_md traits。", call. = FALSE)
  }
  invisible(TRUE)
}


# Conservative default: no conditional ecological equivalence is assumed.
# Only direct writer traits and strict algebraic relationships whose required
# posterior sources are present can be produced under this context.
default_md_bridge_context <- function() {
  list(
    photosynthesis_area_is_amax = FALSE,
    photosynthesis_mass_is_amax = FALSE,
    pnue_conditions_match = FALSE,
    leaf_respiration_tmeas_c = NA_real_,
    leaf_respiration_tref_c = NA_real_,
    leaf_respiration_q10 = NA_real_,
    rd_vcmax_same_reference = FALSE,
    leaf_lifespan_inverse_valid = FALSE,
    wue_definition_matches_sipnet = FALSE,
    gas_exchange_conditions_match = FALSE,
    vpd_kpa = NA_real_,
    generic_root_matches_fine_root = FALSE,
    fine_root_gas_is_co2 = FALSE,
    generic_root_gas_is_co2 = FALSE,
    generic_root_respiration_tmeas_c = NA_real_,
    fine_root_respiration_tmeas_c = NA_real_,
    root_respiration_tref_c = 25,
    root_respiration_q10 = NA_real_,
    stem_represents_sipnet_wood_pool = FALSE,
    litter_rate_is_first_order_k = FALSE,
    litter_rate_matches_sipnet_reference = FALSE,
    allow_root_depth_soilwhc_approx = FALSE,
    soil_water_capacity_fraction = NA_real_,
    allow_leaf_q10_as_veg_proxy = FALSE,
    allow_leaf_q10_as_fine_root_proxy = FALSE,
    allow_leaf_q10_as_coarse_root_proxy = FALSE,
    light_response_model = "",
    par_basis_compatible = FALSE
  )
}


# Extract beta.o posterior draws from a PEcAn.MA result.  Standard PEcAn MA
# fits traits independently, so sampling across trait posteriors below does not
# claim that cross-trait covariance was estimated by MA.
extract_ma_beta_draws <- function(
    ma_result,
    n_draws = 5000L,
    seed = 20260828L
) {
  .md_bridge_require_data_table()
  
  trait_mcmc <- ma_result$trait.mcmc
  if (is.null(trait_mcmc) || !is.list(trait_mcmc)) {
    stop("ma_result$trait.mcmc 必须是命名 list。", call. = FALSE)
  }
  if (is.null(names(trait_mcmc)) || any(!nzchar(names(trait_mcmc)))) {
    stop("trait.mcmc 的每个元素必须有 trait 名称。", call. = FALSE)
  }
  
  n_draws <- as.integer(n_draws)
  if (!is.finite(n_draws) || n_draws < 1L) {
    stop("n_draws 必须 >= 1。", call. = FALSE)
  }
  
  set.seed(seed)
  out <- data.table::data.table(draw_id = seq_len(n_draws))
  
  for (trait_i in names(trait_mcmc)) {
    m <- try(as.matrix(trait_mcmc[[trait_i]]), silent = TRUE)
    if (inherits(m, "try-error") || is.null(colnames(m)) || !"beta.o" %in% colnames(m)) {
      warning("跳过没有 beta.o 的 trait：", trait_i, call. = FALSE)
      next
    }
    beta <- suppressWarnings(as.numeric(m[, "beta.o"]))
    beta <- beta[is.finite(beta)]
    if (length(beta) == 0L) {
      warning("跳过没有有限 beta.o draws 的 trait：", trait_i, call. = FALSE)
      next
    }
    data.table::set(
      out,
      j = trait_i,
      value = sample(beta, size = n_draws, replace = length(beta) < n_draws)
    )
  }
  
  out[]
}


bridge_md_posteriors_to_pecan <- function(
    ma_draws,
    context = list(),
    models = list(),
    registry = md_to_pecan_bridge_registry(),
    strict = TRUE
) {
  .md_bridge_require_data_table()
  validate_md_to_pecan_bridge_registry(registry)
  
  if (!is.list(context)) {
    stop("context 必须是命名 list。", call. = FALSE)
  }
  if (!is.list(models)) {
    stop("models 必须是命名 list；每个元素为 function(draws, context)。", call. = FALSE)
  }
  
  x <- data.table::as.data.table(data.table::copy(ma_draws))
  if (nrow(x) == 0L) stop("ma_draws 没有记录。", call. = FALSE)
  if (!"draw_id" %in% names(x)) x[, draw_id := .I]
  if (anyDuplicated(x$draw_id)) stop("draw_id 必须唯一。", call. = FALSE)
  
  numeric_columns <- setdiff(names(x), "draw_id")
  for (nm in numeric_columns) {
    converted <- suppressWarnings(as.numeric(x[[nm]]))
    bad <- is.na(converted) & !is.na(x[[nm]])
    if (any(bad)) stop("ma_draws 列不能安全转 numeric：", nm, call. = FALSE)
    data.table::set(x, j = nm, value = converted)
  }
  
  n <- nrow(x)
  writer_traits <- sipnet_writer_traits_v2()
  out <- data.table::data.table(draw_id = x$draw_id)
  attempts <- list()
  used_md <- character()
  
  get_source <- function(name) {
    if (!name %in% names(x)) return(rep(NA_real_, n))
    suppressWarnings(as.numeric(x[[name]]))
  }
  
  # Downstream rules may use a higher-priority target already produced by an
  # earlier rule (for example, thickness+density -> SLA -> mass-rate/Amax).
  get_trait <- function(name) {
    if (name %in% names(out)) return(suppressWarnings(as.numeric(out[[name]])))
    get_source(name)
  }
  
  context_value <- function(name, default = NA_real_) {
    if (!name %in% names(context) || is.null(context[[name]])) {
      return(rep(default, n))
    }
    value <- context[[name]]
    if (length(value) == 1L) value <- rep(value, n)
    if (length(value) != n) {
      stop("context$", name, " 的长度必须是 1 或 nrow(ma_draws)。", call. = FALSE)
    }
    suppressWarnings(as.numeric(value))
  }
  
  context_flag <- function(name, default = FALSE) {
    if (!name %in% names(context) || is.null(context[[name]])) return(default)
    isTRUE(as.logical(context[[name]])[1L])
  }
  
  context_text <- function(name, default = "") {
    if (!name %in% names(context) || is.null(context[[name]])) return(default)
    as.character(context[[name]])[1L]
  }
  
  record_attempt <- function(
    target,
    sources,
    conversion_class,
    formula,
    n_candidate,
    n_filled,
    status,
    note = ""
  ) {
    attempts[[length(attempts) + 1L]] <<- data.table::data.table(
      target_pecan_trait = target,
      source_traits = paste(sources, collapse = "|"),
      conversion_class = conversion_class,
      formula = formula,
      n_candidate = as.integer(n_candidate),
      n_filled = as.integer(n_filled),
      status = status,
      note = note
    )
    invisible(NULL)
  }
  
  fill_target <- function(
    target,
    value,
    sources,
    conversion_class,
    formula,
    note = ""
  ) {
    if (!target %in% writer_traits) {
      stop("内部错误：不是 v2 writer trait：", target, call. = FALSE)
    }
    value <- suppressWarnings(as.numeric(value))
    if (length(value) == 1L) value <- rep(value, n)
    if (length(value) != n) {
      stop("转换结果长度错误：", target, call. = FALSE)
    }
    if (!target %in% names(out)) data.table::set(out, j = target, value = rep(NA_real_, n))
    
    candidate <- is.finite(value)
    empty <- !is.finite(out[[target]])
    use <- candidate & empty
    if (any(use)) {
      data.table::set(out, i = which(use), j = target, value = value[use])
      used_md <<- union(used_md, sources[endsWith(sources, "_md")])
    }
    
    record_attempt(
      target = target,
      sources = sources,
      conversion_class = conversion_class,
      formula = formula,
      n_candidate = sum(candidate),
      n_filled = sum(use),
      status = if (any(use)) "FILLED" else "NOT_FILLED",
      note = note
    )
    invisible(sum(use))
  }
  
  # -------------------------------------------------------------------------
  # 1. Direct writer traits always have first priority.
  # -------------------------------------------------------------------------
  direct <- intersect(writer_traits, names(x))
  for (target in direct) {
    fill_target(
      target,
      get_source(target),
      target,
      "DIRECT_WRITER_TRAIT",
      "identity",
      "Direct MA posterior has priority over all *_md bridges."
    )
  }
  
  # leafC_area is a valid PEcAn observed intermediate, but not a v2 writer trait.
  leafC_from_area <- get_source("leafC_area") * get_trait("SLA") / 10
  fill_target(
    "leafC", leafC_from_area, c("leafC_area", "SLA"),
    "DERIVED_ALGEBRAIC", "leafC_percent = leafC_area_g_m2 * SLA_m2_kg / 10"
  )
  
  # -------------------------------------------------------------------------
  # 2. Strict algebraic bridges.
  # -------------------------------------------------------------------------
  thickness_m <- get_source("leaf_thickness_md") / 1000
  leaf_density <- get_source("leaf_tissue_density_md")
  derived_sla <- 1 / (thickness_m * leaf_density)
  derived_sla[!is.finite(derived_sla) | derived_sla <= 0] <- NA_real_
  fill_target(
    "SLA", derived_sla, c("leaf_thickness_md", "leaf_tissue_density_md"),
    "DERIVED_ALGEBRAIC", "1 / ((thickness_mm/1000) * density_kg_m3)",
    "Combining independent posterior marginals approximates, rather than restores, same-leaf covariance."
  )
  
  # Retry after the SLA algebraic rule, because SLA may not have been a direct
  # MA target but may now exist from thickness and tissue density.
  leafC_from_area <- get_source("leafC_area") * get_trait("SLA") / 10
  fill_target(
    "leafC", leafC_from_area, c("leafC_area", "SLA"),
    "DERIVED_ALGEBRAIC", "leafC_percent = leafC_area_g_m2 * SLA_m2_kg / 10"
  )
  
  c2n_leaf_area <- get_source("leafC_area") / get_source("leaf_N_area_md")
  c2n_leaf_area[!is.finite(c2n_leaf_area) | c2n_leaf_area <= 0] <- NA_real_
  fill_target(
    "c2n_leaf", c2n_leaf_area, c("leafC_area", "leaf_N_area_md"),
    "DERIVED_ALGEBRAIC", "leafC_area / leaf_N_area_md"
  )
  
  c2n_leaf_mass <- 10 * get_trait("leafC") / get_source("leaf_N_mass_md")
  c2n_leaf_mass[!is.finite(c2n_leaf_mass) | c2n_leaf_mass <= 0] <- NA_real_
  fill_target(
    "c2n_leaf", c2n_leaf_mass, c("leafC", "leaf_N_mass_md"),
    "DERIVED_ALGEBRAIC", "10 * leafC_percent / leaf_N_g_kg"
  )
  
  c2n_fine <- 10 * get_source("fine_root_C_md") / get_source("fine_root_N_mass_md")
  c2n_fine[!is.finite(c2n_fine) | c2n_fine <= 0] <- NA_real_
  fill_target(
    "c2n_fineroot", c2n_fine, c("fine_root_C_md", "fine_root_N_mass_md"),
    "DERIVED_ALGEBRAIC", "10 * fine_root_C_percent / fine_root_N_g_kg"
  )
  
  if (context_flag("generic_root_matches_fine_root")) {
    fill_target(
      "c2n_fineroot", get_source("c2n_root_generic_md"), "c2n_root_generic_md",
      "CONDITIONAL_DIRECT", "identity after confirmed tissue-scope match"
    )
    c2n_generic <- 10 * get_source("root_C_md") / get_source("root_N_mass_md")
    c2n_generic[!is.finite(c2n_generic) | c2n_generic <= 0] <- NA_real_
    fill_target(
      "c2n_fineroot", c2n_generic, c("root_C_md", "root_N_mass_md"),
      "CONDITIONAL_ALGEBRAIC", "10 * root_C_percent / root_N_g_kg"
    )
  }
  
  if (context_flag("stem_represents_sipnet_wood_pool")) {
    c2n_stem <- 10 * get_source("stem_C_md") / get_source("stem_N_mass_md")
    c2n_stem[!is.finite(c2n_stem) | c2n_stem <= 0] <- NA_real_
    fill_target(
      "c2n_wood", c2n_stem, c("stem_C_md", "stem_N_mass_md"),
      "CONDITIONAL_ALGEBRAIC", "10 * stem_C_percent / stem_N_g_kg"
    )
    fill_target(
      "c2n_wood", get_source("stem_CN_md"), "stem_CN_md",
      "TISSUE_SCOPE_PROXY", "identity after confirmed stem-to-wood-pool scope match"
    )
  }
  
  # -------------------------------------------------------------------------
  # 3. Conditional photosynthesis bridges.
  # -------------------------------------------------------------------------
  if (context_flag("photosynthesis_area_is_amax")) {
    fill_target(
      "Amax", get_source("photosynthesis_rate_area_observed_md"),
      "photosynthesis_rate_area_observed_md", "CONDITIONAL_DIRECT", "identity after Amax protocol confirmation"
    )
  }
  if (context_flag("photosynthesis_mass_is_amax")) {
    amax_from_mass <- get_source("photosynthesis_rate_mass_observed_md") / get_trait("SLA")
    amax_from_mass[!is.finite(amax_from_mass) | amax_from_mass <= 0] <- NA_real_
    fill_target(
      "Amax", amax_from_mass, c("photosynthesis_rate_mass_observed_md", "SLA"),
      "CONDITIONAL_ALGEBRAIC", "Amax_area = Amax_mass / SLA"
    )
  }
  if (context_flag("pnue_conditions_match")) {
    amax_pnue_area <- get_source("PNUE_md") * get_source("leaf_N_area_md")
    amax_pnue_area[!is.finite(amax_pnue_area) | amax_pnue_area <= 0] <- NA_real_
    fill_target(
      "Amax", amax_pnue_area, c("PNUE_md", "leaf_N_area_md"),
      "CONDITIONAL_ALGEBRAIC", "Amax_area = PNUE * leaf_N_area"
    )
    amax_pnue_mass <- get_source("PNUE_md") * get_source("leaf_N_mass_md") / get_trait("SLA")
    amax_pnue_mass[!is.finite(amax_pnue_mass) | amax_pnue_mass <= 0] <- NA_real_
    fill_target(
      "Amax", amax_pnue_mass, c("PNUE_md", "leaf_N_mass_md", "SLA"),
      "CONDITIONAL_ALGEBRAIC", "Amax_area = PNUE * leaf_N_mass / SLA"
    )
  }
  
  # -------------------------------------------------------------------------
  # 4. Leaf respiration, Q10, and leaf turnover.
  # -------------------------------------------------------------------------
  leaf_q10 <- get_source("leaf_respiration_Q10_md")
  context_leaf_q10 <- context_value("leaf_respiration_q10")
  use_context_q10 <- !is.finite(leaf_q10) & is.finite(context_leaf_q10)
  leaf_q10[use_context_q10] <- context_leaf_q10[use_context_q10]
  t_leaf_meas <- context_value("leaf_respiration_tmeas_c")
  t_leaf_ref <- context_value("leaf_respiration_tref_c")
  leaf_temp_factor <- leaf_q10 ^ ((t_leaf_ref - t_leaf_meas) / 10)
  leaf_temp_factor[!is.finite(leaf_temp_factor) | leaf_temp_factor <= 0] <- NA_real_
  
  rd_area <- get_source("leaf_dark_respiration_area_Tmeas_md") * leaf_temp_factor
  rd_area[!is.finite(rd_area) | rd_area <= 0] <- NA_real_
  fill_target(
    "leaf_respiration_rate_m2", rd_area,
    c("leaf_dark_respiration_area_Tmeas_md", "leaf_respiration_Q10_md"),
    "CONDITIONAL_ALGEBRAIC", "Rd_ref = Rd_Tmeas * Q10^((Tref-Tmeas)/10)"
  )
  
  rd_area_from_mass <- get_source("leaf_dark_respiration_mass_Tmeas_md") *
    leaf_temp_factor / get_trait("SLA")
  rd_area_from_mass[!is.finite(rd_area_from_mass) | rd_area_from_mass <= 0] <- NA_real_
  fill_target(
    "leaf_respiration_rate_m2", rd_area_from_mass,
    c("leaf_dark_respiration_mass_Tmeas_md", "leaf_respiration_Q10_md", "SLA"),
    "CONDITIONAL_ALGEBRAIC", "Rd_area_ref = Rd_mass * Q10^((Tref-Tmeas)/10) / SLA"
  )
  
  if (context_flag("rd_vcmax_same_reference")) {
    rd_from_vcmax_area <- get_source("Rd_Vcmax_fraction_md") * get_source("Vcmax_area_Tref_md")
    rd_from_vcmax_area[!is.finite(rd_from_vcmax_area) | rd_from_vcmax_area <= 0] <- NA_real_
    fill_target(
      "leaf_respiration_rate_m2", rd_from_vcmax_area,
      c("Rd_Vcmax_fraction_md", "Vcmax_area_Tref_md"),
      "CONDITIONAL_ALGEBRAIC", "Rd_area = fraction * Vcmax_area"
    )
    rd_from_vcmax_mass <- get_source("Rd_Vcmax_fraction_md") *
      get_source("Vcmax_mass_Tref_md") / get_trait("SLA")
    rd_from_vcmax_mass[!is.finite(rd_from_vcmax_mass) | rd_from_vcmax_mass <= 0] <- NA_real_
    fill_target(
      "leaf_respiration_rate_m2", rd_from_vcmax_mass,
      c("Rd_Vcmax_fraction_md", "Vcmax_mass_Tref_md", "SLA"),
      "CONDITIONAL_ALGEBRAIC", "Rd_area = fraction * Vcmax_mass / SLA"
    )
  }
  
  if (context_flag("leaf_lifespan_inverse_valid")) {
    leaf_turnover <- 1 / get_source("leaf_lifespan_md")
    leaf_turnover[!is.finite(leaf_turnover) | leaf_turnover <= 0] <- NA_real_
    fill_target(
      "leaf_turnover_rate", leaf_turnover, "leaf_lifespan_md",
      "CONDITIONAL_ALGEBRAIC", "leaf_turnover_rate = 1 / lifespan_year",
      "For cereal/deciduous PFTs this flag should remain FALSE unless harvest and phenology double counting has been resolved."
    )
  }
  
  if (context_flag("allow_leaf_q10_as_veg_proxy")) {
    fill_target(
      "veg_respiration_Q10", leaf_q10, "leaf_respiration_Q10_md",
      "TISSUE_SCOPE_PROXY", "veg Q10 = leaf Q10 by explicit proxy assumption"
    )
  }
  if (context_flag("allow_leaf_q10_as_fine_root_proxy")) {
    fill_target(
      "fine_root_respiration_Q10", leaf_q10, "leaf_respiration_Q10_md",
      "TISSUE_SCOPE_PROXY", "fine-root Q10 = leaf Q10 by explicit proxy assumption"
    )
  }
  if (context_flag("allow_leaf_q10_as_coarse_root_proxy")) {
    fill_target(
      "coarse_root_respiration_Q10", leaf_q10, "leaf_respiration_Q10_md",
      "TISSUE_SCOPE_PROXY", "coarse-root Q10 = leaf Q10 by explicit proxy assumption"
    )
  }
  
  # -------------------------------------------------------------------------
  # 5. WUE and simple light-response bridge.
  # -------------------------------------------------------------------------
  vpd <- context_value("vpd_kpa")
  if (context_flag("wue_definition_matches_sipnet")) {
    wue_const <- 2.442 * get_source("WUE_md") * vpd
    wue_const[!is.finite(wue_const) | wue_const <= 0] <- NA_real_
    fill_target(
      "wueConst", wue_const, "WUE_md", "CONDITIONAL_ALGEBRAIC",
      "2.442 * WUE_umol_CO2_per_mmol_H2O * VPD_kPa"
    )
  }
  if (context_flag("gas_exchange_conditions_match")) {
    wue_from_ae <- get_source("photosynthesis_rate_area_observed_md") /
      get_source("leaf_transpiration_area_md")
    wue_const_ae <- 2.442 * wue_from_ae * vpd
    wue_const_ae[!is.finite(wue_const_ae) | wue_const_ae <= 0] <- NA_real_
    fill_target(
      "wueConst", wue_const_ae,
      c("photosynthesis_rate_area_observed_md", "leaf_transpiration_area_md"),
      "CONDITIONAL_ALGEBRAIC", "2.442 * (A_area/E_area) * VPD_kPa"
    )
  }
  
  if (
    identical(tolower(context_text("light_response_model")), "rectangular_hyperbola") &&
    context_flag("par_basis_compatible")
  ) {
    alpha <- get_source("quantum_yield_md")
    lue <- get_source("LUE_md")
    use_lue <- !is.finite(alpha) & is.finite(lue)
    alpha[use_lue] <- lue[use_lue]
    half_sat <- get_trait("Amax") / alpha * 0.0864
    half_sat[!is.finite(half_sat) | half_sat <= 0] <- NA_real_
    fill_target(
      "half_saturation_PAR", half_sat, c("quantum_yield_md", "LUE_md", "Amax"),
      "CONDITIONAL_MECHANISTIC",
      "halfSatPAR_mol_m2_day = (Amax_umol_m2_s / alpha) * 86400 / 1e6",
      "Only a rectangular-hyperbola approximation; canopy and ground/leaf PAR bases must be compatible."
    )
  }
  
  # -------------------------------------------------------------------------
  # 6. Fine-root respiration and generic-root conditional bridges.
  # -------------------------------------------------------------------------
  root_q10 <- context_value("root_respiration_q10")
  root_tref <- context_value("root_respiration_tref_c", default = 25)
  
  if (context_flag("fine_root_gas_is_co2")) {
    fine_tmeas <- context_value("fine_root_respiration_tmeas_c")
    fine_factor <- root_q10 ^ ((root_tref - fine_tmeas) / 10)
    fine_resp <- get_source("fine_root_respiration_mass_Tmeas_md") * fine_factor
    fine_resp[!is.finite(fine_resp) | fine_resp <= 0] <- NA_real_
    fill_target(
      "root_respiration_rate", fine_resp, "fine_root_respiration_mass_Tmeas_md",
      "CONDITIONAL_MECHANISTIC",
      "R_ref_umol_kg_s = R_T_nmol_g_s * Q10^((Tref-Tmeas)/10)",
      "Writer expects a 25 C reference; do not temperature-correct again after this bridge."
    )
  }
  
  if (
    context_flag("generic_root_matches_fine_root") &&
    context_flag("generic_root_gas_is_co2")
  ) {
    generic_tmeas <- context_value("generic_root_respiration_tmeas_c")
    generic_factor <- root_q10 ^ ((root_tref - generic_tmeas) / 10)
    generic_resp <- get_source("root_respiration_mass_Tmeas_generic_md") * generic_factor
    generic_resp[!is.finite(generic_resp) | generic_resp <= 0] <- NA_real_
    fill_target(
      "root_respiration_rate", generic_resp, "root_respiration_mass_Tmeas_generic_md",
      "TISSUE_SCOPE_PROXY",
      "R_ref = R_T * Q10^((Tref-Tmeas)/10) after confirmed generic-root scope match"
    )
  }
  
  if (context_flag("generic_root_matches_fine_root")) {
    fill_target(
      "root_turnover_rate", get_source("root_turnover_rate_generic_md"),
      "root_turnover_rate_generic_md", "CONDITIONAL_DIRECT",
      "identity after confirmed generic-root to fine-root scope match"
    )
  }
  
  # -------------------------------------------------------------------------
  # 7. Litter decomposition and root-depth approximation to soil WHC.
  # -------------------------------------------------------------------------
  if (
    context_flag("litter_rate_is_first_order_k") &&
    context_flag("litter_rate_matches_sipnet_reference")
  ) {
    fill_target(
      "turn_over_time", get_source("litter_decomposition_k_observed_md"),
      "litter_decomposition_k_observed_md", "CONDITIONAL_DIRECT",
      "verified first-order k at the SIPNET reference state"
    )
    fill_target(
      "turn_over_time", get_source("litter_decomposition_rate_observed_md"),
      "litter_decomposition_rate_observed_md", "CONDITIONAL_DIRECT",
      "verified first-order k at the SIPNET reference state"
    )
  }
  
  if (context_flag("allow_root_depth_soilwhc_approx")) {
    water_fraction <- context_value("soil_water_capacity_fraction")
    fine_depth <- get_source("fine_root_rooting_depth_md")
    generic_depth <- get_source("rooting_depth_md")
    depth <- fine_depth
    use_generic <- !is.finite(depth) & is.finite(generic_depth)
    depth[use_generic] <- generic_depth[use_generic]
    whc <- depth * 100 * water_fraction
    whc[!is.finite(whc) | whc <= 0] <- NA_real_
    fill_target(
      "soilWHC", whc, c("fine_root_rooting_depth_md", "rooting_depth_md"),
      "CONDITIONAL_APPROXIMATION", "soilWHC_cm = depth_m * 100 * soil_water_capacity_fraction",
      "The PEcAn writer may later overwrite soilWHC from settings$run$inputs$soil_physics."
    )
  }
  
  # -------------------------------------------------------------------------
  # 8. User-supplied scientific models for all remaining relationships.
  # -------------------------------------------------------------------------
  if (length(models) > 0L) {
    if (is.null(names(models)) || any(!nzchar(names(models)))) {
      stop("models 必须按目标 writer trait 命名。", call. = FALSE)
    }
    invalid_model_targets <- setdiff(names(models), writer_traits)
    if (length(invalid_model_targets) > 0L) {
      stop(
        "models 含非 writer target：",
        paste(invalid_model_targets, collapse = ", "),
        call. = FALSE
      )
    }
    for (target in names(models)) {
      model_fun <- models[[target]]
      if (!is.function(model_fun)) {
        stop("models$", target, " 必须是 function(draws, context)。", call. = FALSE)
      }
      model_draws <- data.table::copy(x)
      produced_targets <- setdiff(names(out), "draw_id")
      for (produced_target in produced_targets) {
        if (!produced_target %in% names(model_draws)) {
          data.table::set(
            model_draws,
            j = produced_target,
            value = out[[produced_target]]
          )
        } else {
          combined_value <- suppressWarnings(as.numeric(model_draws[[produced_target]]))
          use_produced <- !is.finite(combined_value) & is.finite(out[[produced_target]])
          combined_value[use_produced] <- out[[produced_target]][use_produced]
          data.table::set(
            model_draws,
            j = produced_target,
            value = combined_value
          )
        }
      }
      value <- model_fun(model_draws, context)
      candidate_sources <- attr(value, "source_traits", exact = TRUE)
      if (is.null(candidate_sources)) {
        candidate_sources <- registry[
          candidate_pecan_trait == target & source_md %in% names(x),
          source_md
        ]
      }
      if (length(candidate_sources) == 0L) candidate_sources <- "user_model"
      fill_target(
        target, value, candidate_sources, "USER_SUPPLIED_MODEL",
        paste0("models$", target, "(draws, context)"),
        "The model function must propagate coefficient and residual uncertainty itself."
      )
    }
  }
  
  # -------------------------------------------------------------------------
  # 9. Final validation and audit.
  # -------------------------------------------------------------------------
  target_columns <- intersect(writer_traits, names(out))
  complete_targets <- target_columns[
    vapply(out[, ..target_columns], function(z) all(is.finite(z)), logical(1))
  ]
  partial_targets <- setdiff(target_columns, complete_targets)
  
  if (length(complete_targets) > 0L) {
    writer_trait_draws <- out[, c("draw_id", complete_targets), with = FALSE]
  } else {
    writer_trait_draws <- out[, .(draw_id)]
  }
  
  rule_attempts <- if (length(attempts) > 0L) {
    data.table::rbindlist(attempts, use.names = TRUE, fill = TRUE)
  } else {
    data.table::data.table(
      target_pecan_trait = character(), source_traits = character(),
      conversion_class = character(), formula = character(),
      n_candidate = integer(), n_filled = integer(), status = character(), note = character()
    )
  }
  
  md_usage <- data.table::copy(registry)
  md_usage[, present_in_ma_draws := source_md %in% names(x)]
  md_usage[, used_by_successful_bridge := source_md %in% used_md]
  md_usage[, final_status := data.table::fcase(
    !present_in_ma_draws, "NOT_PRESENT_IN_THIS_PFT_MA",
    used_by_successful_bridge, "USED",
    bridge_status == "NOT_CONVERTIBLE_HERE", "NO_WRITER_ROUTE",
    bridge_status == "CUSTOM_MODEL_REQUIRED", "MODEL_OR_COEFFICIENTS_REQUIRED",
    default = "MISSING_SOURCE_OR_CONTEXT"
  )]
  
  unresolved <- md_usage[
    present_in_ma_draws & !used_by_successful_bridge
  ]
  
  pairing_approximation_used <- any(
    used_md %in% registry[same_entity_required == TRUE, source_md]
  )
  if (pairing_approximation_used) {
    warning(
      paste(
        "至少一条成功 bridge 原本要求同一 ObservationID/entity 配对。",
        "这里组合的是独立单性状 MA posterior，因此隐含跨性状独立假设；",
        "正式分析优先在 MA 前构造配对派生值。"
      ),
      call. = FALSE
    )
  }
  
  if (isTRUE(strict) && length(partial_targets) > 0L) {
    warning(
      "以下 target 只在部分 draws 中有有限值，因此未进入 writer_trait_draws：",
      paste(partial_targets, collapse = ", "),
      call. = FALSE
    )
  }
  
  list(
    writer_trait_draws = writer_trait_draws[],
    candidate_trait_draws = out[],
    complete_writer_traits = complete_targets,
    partial_writer_traits = partial_targets,
    independent_marginal_pairing_used = pairing_approximation_used,
    rule_attempts = rule_attempts[],
    md_usage = md_usage[],
    unresolved = unresolved[],
    registry = registry[]
  )
}


# Convenience saver.  CSV files are optional audit products; the RDS preserves
# the full result object.
save_md_pecan_bridge_result <- function(result, out_dir) {
  .md_bridge_require_data_table()
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(result, file.path(out_dir, "md_to_pecan_bridge_result.rds"))
  data.table::fwrite(result$writer_trait_draws, file.path(out_dir, "writer_ready_pecan_trait_draws.csv"))
  data.table::fwrite(result$rule_attempts, file.path(out_dir, "bridge_rule_attempts.csv"))
  data.table::fwrite(result$md_usage, file.path(out_dir, "md_trait_usage.csv"))
  data.table::fwrite(result$unresolved, file.path(out_dir, "md_traits_unresolved.csv"))
  invisible(result)
}


# Select the MA traits allowed to enter the bridge.
#
# By default this implements the requested rule:
#   keep PASS + REVIEW; remove FAIL.
#
# `ma_qc` can be either the complete object returned by
# qc_pecan_ma_result() or its `$summary` table.
select_ma_traits_for_bridge <- function(
    ma_result,
    ma_qc,
    keep_status = c("PASS", "REVIEW")
) {
  .md_bridge_require_data_table()
  
  required_ma_elements <- c("trait.mcmc", "post.distns", "jagged.data")
  missing_ma_elements <- setdiff(required_ma_elements, names(ma_result))
  if (length(missing_ma_elements) > 0L) {
    stop(
      "ma_result 缺少：",
      paste(missing_ma_elements, collapse = ", "),
      call. = FALSE
    )
  }
  if (is.null(rownames(ma_result$post.distns))) {
    stop("ma_result$post.distns 必须用 trait 名作为 row names。", call. = FALSE)
  }
  
  qc_summary <- if (
    is.list(ma_qc) &&
    !data.table::is.data.table(ma_qc) &&
    !is.data.frame(ma_qc) &&
    "summary" %in% names(ma_qc)
  ) {
    ma_qc$summary
  } else {
    ma_qc
  }
  qc_summary <- data.table::as.data.table(data.table::copy(qc_summary))
  
  missing_qc_columns <- setdiff(c("trait", "status"), names(qc_summary))
  if (length(missing_qc_columns) > 0L) {
    stop(
      "ma_qc summary 缺少列：",
      paste(missing_qc_columns, collapse = ", "),
      call. = FALSE
    )
  }
  if (anyNA(qc_summary$trait) || any(!nzchar(as.character(qc_summary$trait)))) {
    stop("ma_qc$summary$trait 不能缺失或为空。", call. = FALSE)
  }
  if (anyDuplicated(as.character(qc_summary$trait))) {
    stop("ma_qc$summary 中 trait 必须唯一。", call. = FALSE)
  }
  
  qc_summary[, trait := as.character(trait)]
  qc_summary[, status := toupper(trimws(as.character(status)))]
  
  valid_status <- c("PASS", "REVIEW", "FAIL")
  invalid_status <- setdiff(unique(qc_summary$status), valid_status)
  if (length(invalid_status) > 0L) {
    stop(
      "ma_qc 含未知 status：",
      paste(invalid_status, collapse = ", "),
      call. = FALSE
    )
  }
  
  keep_status <- unique(toupper(trimws(as.character(keep_status))))
  if (
    length(keep_status) == 0L ||
    any(!keep_status %in% valid_status)
  ) {
    stop(
      "keep_status 只能包含 PASS、REVIEW、FAIL；",
      "推荐使用 c(\"PASS\", \"REVIEW\")。",
      call. = FALSE
    )
  }
  
  ma_traits <- rownames(ma_result$post.distns)
  qc_missing_from_ma <- setdiff(qc_summary$trait, ma_traits)
  if (length(qc_missing_from_ma) > 0L) {
    warning(
      "ma_qc 中以下 traits 不在 ma_result$post.distns，将忽略：",
      paste(qc_missing_from_ma, collapse = ", "),
      call. = FALSE
    )
  }
  
  selection <- data.table::data.table(trait = ma_traits)
  selection <- qc_summary[selection, on = "trait"]
  selection[, keep_for_bridge := !is.na(status) & status %in% keep_status]
  selection[, selection_reason := data.table::fcase(
    is.na(status), "NO_QC_DECISION",
    keep_for_bridge, paste0("KEEP_", status),
    default = paste0("DROP_", status)
  )]
  
  no_qc_decision <- selection[is.na(status), trait]
  if (length(no_qc_decision) > 0L) {
    warning(
      "以下 MA traits 没有 QC decision，因此不进入 bridge：",
      paste(no_qc_decision, collapse = ", "),
      call. = FALSE
    )
  }
  
  keep_traits <- selection[keep_for_bridge == TRUE, trait]
  if (length(keep_traits) == 0L) {
    stop("QC 筛选后没有 trait 可以进入 bridge。", call. = FALSE)
  }
  
  filtered_result <- list(
    trait.mcmc = ma_result$trait.mcmc[
      keep_traits[keep_traits %in% names(ma_result$trait.mcmc)]
    ],
    post.distns = ma_result$post.distns[keep_traits, , drop = FALSE],
    jagged.data = ma_result$jagged.data[
      keep_traits[keep_traits %in% names(ma_result$jagged.data)]
    ]
  )
  
  list(
    ma_result = filtered_result,
    selection = selection[],
    kept_traits = keep_traits,
    passed_traits = selection[keep_for_bridge == TRUE & status == "PASS", trait],
    review_traits = selection[keep_for_bridge == TRUE & status == "REVIEW", trait],
    failed_traits = selection[status == "FAIL", trait],
    dropped_traits = selection[keep_for_bridge == FALSE, trait],
    keep_status = keep_status
  )
}


# One-call bridge from a QC-screened PEcAn MA result to writer-ready PEcAn
# posterior draws.  It retains PASS + REVIEW by default and removes only FAIL.
bridge_ma_qc_to_pecan <- function(
    ma_result,
    ma_qc,
    out_dir = NULL,
    n_draws = 5000L,
    seed = 20260828L,
    keep_status = c("PASS", "REVIEW"),
    context = list(),
    models = list(),
    registry = md_to_pecan_bridge_registry(),
    strict = TRUE,
    save_ma_draws_csv = FALSE
) {
  .md_bridge_require_data_table()
  
  selected <- select_ma_traits_for_bridge(
    ma_result = ma_result,
    ma_qc = ma_qc,
    keep_status = keep_status
  )
  
  ma_result_nonfail <- selected$ma_result
  
  ma_draws <- extract_ma_beta_draws(
    ma_result = ma_result_nonfail,
    n_draws = n_draws,
    seed = seed
  )
  
  bridge_result <- bridge_md_posteriors_to_pecan(
    ma_draws = ma_draws,
    context = context,
    models = models,
    registry = registry,
    strict = strict
  )
  
  result <- c(
    list(
      # Filtered standard PEcAn MA contract: PASS + REVIEW only by default.
      trait.mcmc = ma_result_nonfail$trait.mcmc,
      post.distns = ma_result_nonfail$post.distns,
      jagged.data = ma_result_nonfail$jagged.data,
      ma_result_nonfail = ma_result_nonfail,
      ma_draws = ma_draws,
      qc_selection = selected$selection,
      kept_traits = selected$kept_traits,
      passed_traits = selected$passed_traits,
      review_traits = selected$review_traits,
      failed_traits = selected$failed_traits,
      dropped_traits = selected$dropped_traits,
      keep_status = selected$keep_status
    ),
    bridge_result
  )
  
  if (!is.null(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Preserve the full result and the filtered PEcAn objects.
    saveRDS(
      result,
      file.path(out_dir, "ma_qc_nonfail_to_pecan_bridge_result.rds"),
      compress = FALSE
    )
    
    trait.mcmc <- result$trait.mcmc
    post.distns <- result$post.distns
    jagged.data <- result$jagged.data
    
    save(
      trait.mcmc,
      file = file.path(out_dir, "trait.mcmc.QC_NONFAIL.Rdata"),
      compress = FALSE
    )
    save(
      post.distns,
      file = file.path(out_dir, "post.distns.QC_NONFAIL.Rdata"),
      compress = FALSE
    )
    save(
      jagged.data,
      file = file.path(out_dir, "jagged.data.QC_NONFAIL.Rdata"),
      compress = FALSE
    )
    
    data.table::fwrite(
      result$qc_selection,
      file.path(out_dir, "ma_qc_bridge_selection.csv")
    )
    data.table::fwrite(
      result$writer_trait_draws,
      file.path(out_dir, "writer_ready_pecan_trait_draws.csv")
    )
    data.table::fwrite(
      result$candidate_trait_draws,
      file.path(out_dir, "candidate_pecan_trait_draws.csv")
    )
    data.table::fwrite(
      result$rule_attempts,
      file.path(out_dir, "bridge_rule_attempts.csv")
    )
    data.table::fwrite(
      result$md_usage,
      file.path(out_dir, "md_trait_usage.csv")
    )
    data.table::fwrite(
      result$unresolved,
      file.path(out_dir, "md_traits_unresolved.csv")
    )
    
    if (isTRUE(save_ma_draws_csv)) {
      data.table::fwrite(
        result$ma_draws,
        file.path(out_dir, "ma_beta_o_draws_nonfail.csv")
      )
    }
  }
  
  message(
    "Bridge finished. Kept MA traits: ", length(result$kept_traits),
    " (PASS: ", length(result$passed_traits),
    "; REVIEW: ", length(result$review_traits),
    "); removed traits: ", length(result$dropped_traits),
    " (FAIL in QC: ", length(result$failed_traits), ")",
    "; complete writer-ready PEcAn traits: ",
    length(result$complete_writer_traits), "."
  )
  
  result
}


# =============================================================================
# Minimal call template
# =============================================================================
#
# source("md_to_pecan_bridge_v1.R")
#
# bridge_context <- default_md_bridge_context()
#
# bridge_result <- bridge_ma_qc_to_pecan(
#   ma_result = ma_result,
#   ma_qc = ma_qc,
#   out_dir = file.path(
#     "/projectnb/dietzelab/guYANG/SIPNET_Model_Calibration",
#     "TRY_MA_RESULTS",
#     pftname,
#     "03_qc_nonfail_to_pecan_bridge"
#   ),
#   n_draws = 5000L,
#   seed = 20260828L,
#   keep_status = c("PASS", "REVIEW"),
#   context = bridge_context,
#   models = list(),
#   strict = TRUE
# )
#
# bridge_result$kept_traits
# bridge_result$failed_traits
# bridge_result$complete_writer_traits
# bridge_result$writer_trait_draws
# bridge_result$unresolved
#
# The lower-level two-step API remains available:
#
# ma_draws <- extract_ma_beta_draws(
#   ma_result = ma_result,
#   n_draws = 5000L,
#   seed = 20260828L
# )
#
# bridge_context <- list(
#   # Keep every assumption FALSE unless it has been checked for this PFT/data.
#   photosynthesis_area_is_amax = FALSE,
#   photosynthesis_mass_is_amax = FALSE,
#   pnue_conditions_match = FALSE,
#   leaf_respiration_tmeas_c = NA_real_,
#   leaf_respiration_tref_c = NA_real_,
#   leaf_respiration_q10 = NA_real_,
#   rd_vcmax_same_reference = FALSE,
#   leaf_lifespan_inverse_valid = FALSE,
#   wue_definition_matches_sipnet = FALSE,
#   gas_exchange_conditions_match = FALSE,
#   vpd_kpa = NA_real_,
#   generic_root_matches_fine_root = FALSE,
#   fine_root_gas_is_co2 = FALSE,
#   generic_root_gas_is_co2 = FALSE,
#   root_respiration_tmeas_c = NA_real_,
#   fine_root_respiration_tmeas_c = NA_real_,
#   root_respiration_tref_c = 25,
#   root_respiration_q10 = NA_real_,
#   stem_represents_sipnet_wood_pool = FALSE,
#   litter_rate_is_first_order_k = FALSE,
#   litter_rate_matches_sipnet_reference = FALSE,
#   allow_root_depth_soilwhc_approx = FALSE,
#   soil_water_capacity_fraction = NA_real_,
#   allow_leaf_q10_as_veg_proxy = FALSE,
#   allow_leaf_q10_as_fine_root_proxy = FALSE,
#   allow_leaf_q10_as_coarse_root_proxy = FALSE,
#   light_response_model = "",
#   par_basis_compatible = FALSE
# )
#
# bridge_result <- bridge_md_posteriors_to_pecan(
#   ma_draws = ma_draws,
#   context = bridge_context,
#   models = list(),
#   strict = TRUE
# )
#
# print(bridge_result$complete_writer_traits)
# print(bridge_result$unresolved, nrows = Inf)
#
# save_md_pecan_bridge_result(
#   bridge_result,
#   out_dir = file.path(
#     "/projectnb/dietzelab/guYANG/SIPNET_Model_Calibration",
#     "TRY_MA_RESULTS",
#     pftname,
#     "02_md_to_pecan_bridge"
#   )
# )


writer_draws_to_pecan_samples <- function(
    writer_result,
    ensemble_size = 100L,
    sample_method = "uniform"
) {
  library(data.table)
  library(coda)
  
  pftname <- writer_result$pftname
  
  draws <- as.data.table(
    copy(writer_result$writer_trait_draws)
  )
  
  trait_names <- setdiff(
    names(draws),
    "draw_id"
  )
  
  if (ensemble_size > nrow(draws)) {
    stop(
      "当前 joint sampling 要求 ensemble_size <= posterior draws 数量：",
      nrow(draws),
      call. = FALSE
    )
  }
  
  # 每一个最终 PEcAn trait 包装成 PEcAn 要求的 mcmc.list
  # 所有 trait 保持完全相同的行顺序
  final_trait_mcmc <- setNames(
    lapply(
      trait_names,
      function(trait_i) {
        values <- suppressWarnings(
          as.numeric(draws[[trait_i]])
        )
        
        if (any(!is.finite(values))) {
          stop(
            trait_i,
            " 包含非有限 posterior draws。",
            call. = FALSE
          )
        }
        
        chain_matrix <- matrix(
          values,
          ncol = 1L,
          dimnames = list(
            NULL,
            "beta.o"
          )
        )
        
        coda::mcmc.list(
          coda::mcmc(chain_matrix)
        )
      }
    ),
    trait_names
  )
  
  # get_parameter_samples() 要求每个 PFT 一个 list 元素
  trait_mcmc_list <- setNames(
    list(final_trait_mcmc),
    pftname
  )
  
  # 所有19个 trait 都已有 posterior draws，
  # 因此不需要额外的 parametric prior.distns
  prior_distns_list <- setNames(
    list(NULL),
    pftname
  )
  
  pecan_samples <- PEcAn.uncertainty::get_parameter_samples(
    pft_names = pftname,
    prior_distns_list = prior_distns_list,
    trait_mcmc_list = trait_mcmc_list,
    
    ensemble.size = ensemble_size,
    ens.sample.method = sample_method,
    
    sa_quantiles = NULL,
    do_ensemble = TRUE,
    
    # 非常重要：使用相同 posterior 行号抽取所有 traits
    independent = FALSE
  )
  
  list(
    pftname = pftname,
    trait_mcmc = final_trait_mcmc,
    samples = pecan_samples
  )
}

sipnet_writer_traits_v2 <- function() {
  c(
    "leafC", "SLA", "Amax", "AmaxFrac",
    "extinction_coefficient",
    "leaf_respiration_rate_m2",
    "Vm_low_temp", "psnTOpt",
    "growth_resp_factor",
    "half_saturation_PAR",
    "dVPDSlope", "dVpdExp",
    "leaf_turnover_rate", "wueConst",
    
    "veg_respiration_Q10",
    "stem_respiration_rate",
    "root_turnover_rate",
    "fine_root_respiration_Q10",
    "root_respiration_rate",
    "coarse_root_respiration_Q10",
    "root_allocation_fraction",
    "wood_allocation_fraction",
    "leaf_allocation_fraction",
    "wood_turnover_rate",
    
    "soil_respiration_Q10",
    "som_respiration_rate",
    "turn_over_time",
    "fracLitterRespired",
    "frozenSoilEff",
    "frozenSoilFolREff",
    "soilWHC",
    "immedEvapFrac",
    "leafWHC",
    "waterRemoveFrac",
    "fastFlowFrac",
    "rdConst",
    "water_drain_frac",
    
    "GDD",
    "leafOnReallocFrac",
    "fracLeafFall",
    "leafGrowth",
    
    "c2n_leaf",
    "c2n_wood",
    "c2n_fineroot",
    "kCN",
    "n_volatilization_rate",
    "n_leaching_frac",
    "n_fixation_frac_max",
    "n_fix_half_sat",
    "f_anoxia",
    "anaerobic_decomp_rate",
    "anaerobic_trans_exp",
    "soil_methane_rate",
    "litter_methane_rate"
  )
}
