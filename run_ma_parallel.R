# Parallel PEcAn meta-analysis for one PFT.
#
# Each trait is passed independently to
# PEcAn.MA::meta_analysis_standalone() in its own output directory. Results
# are checkpointed on disk and then recombined into the same three-element
# object returned by meta_analysis_standalone():
#   trait.mcmc, post.distns, jagged.data

run_pecan_ma_parallel <- function(
    trait.data,
    prior.distns,
    pft_name,
    outdir,
    iterations = 3000L,
    random = TRUE,
    threshold = 1.2,
    use_ghs = FALSE,
    gamma_tau = 0.01,
    workers = 2L,
    resume = TRUE,
    save_combined = TRUE
) {
  required_packages <- c("PEcAn.MA", "future", "furrr")
  missing_packages <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  
  if (length(missing_packages) > 0L) {
    stop(
      "Missing required packages: ",
      paste(missing_packages, collapse = ", "),
      call. = FALSE
    )
  }
  
  if (!is.list(trait.data) || is.null(names(trait.data))) {
    stop("trait.data must be a named list.", call. = FALSE)
  }
  
  trait_names <- names(trait.data)
  if (
    length(trait_names) == 0L ||
    anyNA(trait_names) ||
    any(!nzchar(trait_names)) ||
    anyDuplicated(trait_names)
  ) {
    stop("trait.data must have unique, non-empty trait names.", call. = FALSE)
  }
  
  empty_traits <- trait_names[
    vapply(trait.data, function(x) is.null(x) || nrow(x) == 0L, logical(1))
  ]
  if (length(empty_traits) > 0L) {
    stop(
      "The following trait.data elements are empty: ",
      paste(empty_traits, collapse = ", "),
      call. = FALSE
    )
  }
  
  prior.distns <- as.data.frame(prior.distns, stringsAsFactors = FALSE)
  
  # Allow a prior table with an explicit `trait` column, but convert it to the
  # row-name format required by PEcAn.MA.
  default_row_names <- identical(
    rownames(prior.distns),
    as.character(seq_len(nrow(prior.distns)))
  )
  if ("trait" %in% names(prior.distns)) {
    if (default_row_names) {
      rownames(prior.distns) <- as.character(prior.distns$trait)
    }
    prior.distns$trait <- NULL
  }
  
  required_prior_columns <- c("distn", "parama", "paramb", "n")
  missing_prior_columns <- setdiff(
    required_prior_columns,
    names(prior.distns)
  )
  if (length(missing_prior_columns) > 0L) {
    stop(
      "prior.distns is missing columns: ",
      paste(missing_prior_columns, collapse = ", "),
      call. = FALSE
    )
  }
  
  missing_priors <- setdiff(trait_names, rownames(prior.distns))
  if (length(missing_priors) > 0L) {
    stop(
      "No prior was found for: ",
      paste(missing_priors, collapse = ", "),
      call. = FALSE
    )
  }
  
  # PEcAn.MA expects exactly these four prior columns and uses trait names as
  # row names.
  prior.distns <- prior.distns[
    trait_names,
    required_prior_columns,
    drop = FALSE
  ]
  
  iterations <- as.integer(iterations)
  workers <- min(
    max(1L, as.integer(workers)),
    length(trait_names)
  )
  
  if (
    length(iterations) != 1L ||
    is.na(iterations) ||
    iterations < 1L
  ) {
    stop("iterations must be a positive integer.", call. = FALSE)
  }
  if (!is.logical(random) || length(random) != 1L || is.na(random)) {
    stop("random must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(use_ghs) || length(use_ghs) != 1L || is.na(use_ghs)) {
    stop("use_ghs must be TRUE or FALSE.", call. = FALSE)
  }
  
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  trait_run_root <- file.path(outdir, "parallel_trait_runs")
  dir.create(trait_run_root, recursive = TRUE, showWarnings = FALSE)
  
  safe_name <- function(x) {
    x <- gsub("[^A-Za-z0-9._-]+", "_", x)
    x <- gsub("_+", "_", x)
    substr(x, 1L, 120L)
  }
  
  config <- list(
    pft_name = as.character(pft_name),
    iterations = iterations,
    random = random,
    threshold = threshold,
    use_ghs = use_ghs,
    gamma_tau = gamma_tau,
    resume = isTRUE(resume)
  )
  
  # Write small, trait-specific inputs before launching workers. Workers only
  # receive file paths, so the complete trait.data object is not copied to
  # every R process.
  tasks <- lapply(seq_along(trait_names), function(i) {
    trait_name <- trait_names[[i]]
    trait_dir <- file.path(
      trait_run_root,
      sprintf("%03d_%s", i, safe_name(trait_name))
    )
    dir.create(trait_dir, recursive = TRUE, showWarnings = FALSE)
    
    input_file <- file.path(trait_dir, "ma_input.rds")
    result_file <- file.path(trait_dir, "ma_result.rds")
    input_hash_file <- file.path(trait_dir, "ma_input.md5")
    
    saveRDS(
      list(
        trait_data = setNames(list(trait.data[[trait_name]]), trait_name),
        priors = prior.distns[trait_name, , drop = FALSE],
        # Include every MA setting that can change the result in the cache
        # fingerprint. Changing random/iterations/etc. therefore forces a
        # fresh run even when trait data and priors are unchanged.
        ma_config = config[setdiff(names(config), "resume")]
      ),
      input_file,
      compress = FALSE
    )
    
    input_md5 <- unname(tools::md5sum(input_file))
    
    list(
      trait = trait_name,
      trait_dir = trait_dir,
      input_file = input_file,
      result_file = result_file,
      input_hash_file = input_hash_file,
      input_md5 = input_md5
    )
  })
  
  run_one_trait <- function(task, config) {
    cached_hash <- if (file.exists(task$input_hash_file)) {
      trimws(readLines(task$input_hash_file, n = 1L, warn = FALSE))
    } else {
      NA_character_
    }
    
    cache_is_current <- (
      config$resume &&
        file.exists(task$result_file) &&
        length(cached_hash) == 1L &&
        !is.na(cached_hash) &&
        identical(cached_hash, task$input_md5)
    )
    
    if (cache_is_current) {
      return(data.frame(
        trait = task$trait,
        status = "CACHED",
        result_file = task$result_file,
        message = NA_character_,
        stringsAsFactors = FALSE
      ))
    }
    
    tryCatch(
      {
        ma_input <- readRDS(task$input_file)
        
        one_result <- PEcAn.MA::meta_analysis_standalone(
          trait_data = ma_input$trait_data,
          priors = ma_input$priors,
          iterations = config$iterations,
          outdir = task$trait_dir,
          pft_name = config$pft_name,
          random = config$random,
          threshold = config$threshold,
          use_ghs = config$use_ghs,
          gamma_tau = config$gamma_tau
        )
        
        # Uncompressed checkpoints are much faster to write and reload for
        # large random-effect MCMC objects.
        saveRDS(one_result, task$result_file, compress = FALSE)
        writeLines(task$input_md5, task$input_hash_file)
        
        data.frame(
          trait = task$trait,
          status = "OK",
          result_file = task$result_file,
          message = NA_character_,
          stringsAsFactors = FALSE
        )
      },
      error = function(e) {
        error_message <- conditionMessage(e)
        writeLines(error_message, file.path(task$trait_dir, "ERROR.txt"))
        
        data.frame(
          trait = task$trait,
          status = "ERROR",
          result_file = task$result_file,
          message = error_message,
          stringsAsFactors = FALSE
        )
      }
    )
  }
  
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  
  if (workers == 1L) {
    future::plan(future::sequential)
  } else {
    # PSOCK/multisession is safer than forking an active RStudio/JAGS process.
    future::plan(future::multisession, workers = workers)
  }
  
  status_list <- furrr::future_map(
    tasks,
    run_one_trait,
    config = config,
    .options = furrr::furrr_options(
      seed = TRUE,
      packages = "PEcAn.MA"
    )
  )
  
  status <- do.call(rbind, status_list)
  rownames(status) <- NULL
  
  status_file <- file.path(outdir, "parallel_ma_status.csv")
  utils::write.csv(status, status_file, row.names = FALSE)
  
  failed <- status$status == "ERROR"
  if (any(failed)) {
    stop(
      "Parallel MA failed for ",
      sum(failed),
      " trait(s): ",
      paste(status$trait[failed], collapse = ", "),
      ". Successful traits were checkpointed. See: ",
      status_file,
      "\nAfter fixing the failed traits, rerun with resume = TRUE.",
      call. = FALSE
    )
  }
  
  # Reconstruct the same three objects returned by
  # meta_analysis_standalone(), preserving the original trait order.
  trait.mcmc <- list()
  jagged.data <- list()
  posterior_rows <- vector("list", length(trait_names))
  names(posterior_rows) <- trait_names
  converged <- logical(length(trait_names))
  
  for (i in seq_along(trait_names)) {
    trait_name <- trait_names[[i]]
    result_file <- status$result_file[match(trait_name, status$trait)]
    one_result <- readRDS(result_file)
    
    if (trait_name %in% names(one_result$trait.mcmc)) {
      trait.mcmc[[trait_name]] <- one_result$trait.mcmc[[trait_name]]
      converged[[i]] <- TRUE
    }
    
    if (trait_name %in% names(one_result$jagged.data)) {
      jagged.data[[trait_name]] <- one_result$jagged.data[[trait_name]]
    }
    
    one_posterior <- one_result$post.distns
    if (trait_name %in% rownames(one_posterior)) {
      one_posterior <- one_posterior[trait_name, , drop = FALSE]
    } else if (nrow(one_posterior) == 1L) {
      rownames(one_posterior) <- trait_name
    } else {
      stop(
        "Could not identify posterior row for trait: ",
        trait_name,
        call. = FALSE
      )
    }
    posterior_rows[[trait_name]] <- one_posterior
    
    rm(one_result)
  }
  
  post.distns <- do.call(rbind, posterior_rows)
  rownames(post.distns) <- trait_names
  status$converged <- converged[match(status$trait, trait_names)]
  utils::write.csv(status, status_file, row.names = FALSE)
  
  ma_result <- list(
    trait.mcmc = trait.mcmc,
    post.distns = post.distns,
    jagged.data = jagged.data
  )
  
  if (isTRUE(save_combined)) {
    save(
      trait.mcmc,
      file = file.path(outdir, "trait.mcmc.Rdata"),
      compress = FALSE
    )
    save(
      post.distns,
      file = file.path(outdir, "post.distns.Rdata"),
      compress = FALSE
    )
    save(
      jagged.data,
      file = file.path(outdir, "jagged.data.Rdata"),
      compress = FALSE
    )
  }
  
  message(
    "Parallel PEcAn MA finished for ", pft_name,
    ". Traits requested: ", length(trait_names),
    "; converged: ", sum(converged),
    "; not retained after convergence screening: ", sum(!converged),
    "."
  )
  
  ma_result
}


# Complete single-PFT TRY -> PEcAn meta-analysis pipeline.
#
# Required helper functions must already have been sourced into the calling R
# session:
#   build_try_unit_map()
#   validate_try_unit_conversion_examples()
#   prepare_single_pft_try_data_for_ma()
#   make_trait_data_from_try_ma_long()
#   make_prior_distns_from_trait_data()
#
# If the three data arguments are NULL, the function uses the objects already
# present in the calling environment:
#   trydat_use_species, trait_map, pftspecies
run_single_pft_try_ma_pipeline_parallel <- function(
    pftname,
    out_dir,
    iterations = 3000L,
    random = TRUE,
    threshold = 1.2,
    use_ghs = FALSE,
    gamma_tau = 0.01,
    workers = 2L,
    sample_fraction = 0.05,
    min_sample_n = 1L,
    max_sample_n = 2L,
    width_multiplier = 10,
    relative_sd_floor = 0.10,
    seed = 20260827L,
    unit_col = "UnitName",
    value_col = "StdValue",
    drop_errorrisk = TRUE,
    unsupported_action = "stop",
    ambiguous_species_action = "warn",
    resume = TRUE,
    save_intermediate = TRUE,
    trydat_use_species = NULL,
    trait_map = NULL,
    pft_species_map = NULL,
    unit_map = NULL
) {
  caller_env <- parent.frame()
  
  get_existing_object <- function(value, object_name) {
    if (!is.null(value)) {
      return(value)
    }
    if (exists(object_name, envir = caller_env, inherits = TRUE)) {
      return(get(object_name, envir = caller_env, inherits = TRUE))
    }
    stop(
      "Object `", object_name, "` was not found. Either create it in the ",
      "calling environment or pass it explicitly to the pipeline function.",
      call. = FALSE
    )
  }
  
  get_existing_function <- function(function_name) {
    if (!exists(
      function_name,
      envir = caller_env,
      mode = "function",
      inherits = TRUE
    )) {
      stop(
        "Required helper function `", function_name,
        "()` is not loaded. Source the script that defines it first.",
        call. = FALSE
      )
    }
    get(
      function_name,
      envir = caller_env,
      mode = "function",
      inherits = TRUE
    )
  }
  
  if (!requireNamespace("PEcAn.data.remote", quietly = TRUE)) {
    stop("Package `PEcAn.data.remote` is required.", call. = FALSE)
  }
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package `data.table` is required.", call. = FALSE)
  }
  
  trydat_use_species <- get_existing_object(
    trydat_use_species,
    "trydat_use_species"
  )
  trait_map <- get_existing_object(trait_map, "trait_map")
  pft_species_map <- get_existing_object(pft_species_map, "pftspecies")
  
  build_unit_map_fn <- get_existing_function("build_try_unit_map")
  validate_units_fn <- get_existing_function(
    "validate_try_unit_conversion_examples"
  )
  prepare_try_fn <- get_existing_function(
    "prepare_single_pft_try_data_for_ma"
  )
  make_trait_data_fn <- get_existing_function(
    "make_trait_data_from_try_ma_long"
  )
  make_priors_fn <- get_existing_function(
    "make_prior_distns_from_trait_data"
  )
  
  if (
    !is.character(pftname) ||
    length(pftname) != 1L ||
    is.na(pftname) ||
    !nzchar(trimws(pftname))
  ) {
    stop("pftname must be one non-empty character value.", call. = FALSE)
  }
  if (
    !is.character(out_dir) ||
    length(out_dir) != 1L ||
    is.na(out_dir) ||
    !nzchar(trimws(out_dir))
  ) {
    stop("out_dir must be one non-empty path.", call. = FALSE)
  }
  
  prep_dir <- file.path(out_dir, "01_preparation")
  ma_dir <- file.path(out_dir, "02_meta_analysis")
  dir.create(prep_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(ma_dir, recursive = TRUE, showWarnings = FALSE)
  
  message("[1/6] Building and validating the TRY unit map: ", pftname)
  
  if (is.null(unit_map)) {
    unit_map <- build_unit_map_fn(trait_map = trait_map)
  }
  
  unit_conversion_test <- validate_units_fn()
  print(unit_conversion_test)
  
  message("[2/6] Selecting the PFT and harmonizing TRY units: ", pftname)
  
  try_data <- prepare_try_fn(
    trydat_use_species = trydat_use_species,
    pftname = pftname,
    trait_map = trait_map,
    unit_map = unit_map,
    pft_species_map = pft_species_map,
    unit_col = unit_col,
    value_col = value_col,
    drop_errorrisk = drop_errorrisk,
    unsupported_action = unsupported_action,
    ambiguous_species_action = ambiguous_species_action
  )
  
  trait_map_for_ma <- attr(try_data, "trait_map_for_ma")
  if (is.null(trait_map_for_ma)) {
    trait_map_for_ma <- trait_map
  }
  
  message("[3/6] Converting TRY data to PEcAn MA long format: ", pftname)
  
  try_ma_long <- PEcAn.data.remote::format_try_for_ma(
    try_data = try_data,
    trait_map = trait_map_for_ma,
    species_map = NULL
  )
  try_ma_long <- data.table::as.data.table(try_ma_long)
  try_ma_long[, final_pft := as.character(pftname)]
  
  required_ma_columns <- c("mean", "vname")
  missing_ma_columns <- setdiff(required_ma_columns, names(try_ma_long))
  if (length(missing_ma_columns) > 0L) {
    stop(
      "format_try_for_ma() output is missing columns: ",
      paste(missing_ma_columns, collapse = ", "),
      call. = FALSE
    )
  }
  if (nrow(try_ma_long) == 0L) {
    stop("try_ma_long contains no records.", call. = FALSE)
  }
  if (any(!is.finite(try_ma_long$mean))) {
    stop("try_ma_long$mean still contains non-finite values.", call. = FALSE)
  }
  if (anyNA(try_ma_long$vname) || any(!nzchar(try_ma_long$vname))) {
    stop("try_ma_long$vname contains missing or empty values.", call. = FALSE)
  }
  
  message("[4/6] Creating trait.data: ", pftname)
  trait.data <- make_trait_data_fn(try_ma_long)
  
  if (!is.list(trait.data) || length(trait.data) == 0L) {
    stop("trait.data is empty or is not a named list.", call. = FALSE)
  }
  
  message("[5/6] Creating prior.distns: ", pftname)
  prior.distns <- make_priors_fn(
    trait.data = trait.data,
    sample_fraction = sample_fraction,
    min_sample_n = min_sample_n,
    max_sample_n = max_sample_n,
    width_multiplier = width_multiplier,
    relative_sd_floor = relative_sd_floor,
    seed = seed
  )
  
  if (isTRUE(save_intermediate)) {
    saveRDS(trait_map, file.path(prep_dir, "trait_map.rds"))
    saveRDS(unit_map, file.path(prep_dir, "unit_map.rds"))
    saveRDS(
      unit_conversion_test,
      file.path(prep_dir, "unit_conversion_test.rds")
    )
    saveRDS(trait_map_for_ma, file.path(prep_dir, "trait_map_for_ma.rds"))
    saveRDS(
      try_data,
      file.path(prep_dir, "try_data.rds"),
      compress = FALSE
    )
    saveRDS(
      try_ma_long,
      file.path(prep_dir, "try_ma_long.rds"),
      compress = FALSE
    )
    save(
      trait.data,
      file = file.path(prep_dir, "trait.data.Rdata"),
      compress = FALSE
    )
    save(
      prior.distns,
      file = file.path(prep_dir, "prior.distns.Rdata"),
      compress = FALSE
    )
    
    pipeline_configuration <- list(
      pftname = pftname,
      out_dir = out_dir,
      iterations = as.integer(iterations),
      random = random,
      threshold = threshold,
      use_ghs = use_ghs,
      gamma_tau = gamma_tau,
      workers = as.integer(workers),
      sample_fraction = sample_fraction,
      min_sample_n = as.integer(min_sample_n),
      max_sample_n = as.integer(max_sample_n),
      width_multiplier = width_multiplier,
      relative_sd_floor = relative_sd_floor,
      seed = seed,
      unit_col = unit_col,
      value_col = value_col,
      drop_errorrisk = drop_errorrisk,
      unsupported_action = unsupported_action,
      ambiguous_species_action = ambiguous_species_action
    )
    saveRDS(
      pipeline_configuration,
      file.path(prep_dir, "pipeline_configuration.rds")
    )
  }
  
  message(
    "[6/6] Running trait-level parallel PEcAn meta-analysis with ",
    workers,
    " worker(s): ",
    pftname
  )
  
  ma_result <- run_pecan_ma_parallel(
    trait.data = trait.data,
    prior.distns = prior.distns,
    pft_name = pftname,
    outdir = ma_dir,
    iterations = iterations,
    random = random,
    threshold = threshold,
    use_ghs = use_ghs,
    gamma_tau = gamma_tau,
    workers = workers,
    resume = resume,
    save_combined = TRUE
  )
  
  # The return contract intentionally remains identical to
  # PEcAn.MA::meta_analysis_standalone().
  ma_result
}


# Keep a selected set of traits while preserving the standard PEcAn MA result
# contract.
subset_pecan_ma_result <- function(ma_result, traits) {
  required_elements <- c("trait.mcmc", "post.distns", "jagged.data")
  missing_elements <- setdiff(required_elements, names(ma_result))
  if (length(missing_elements) > 0L) {
    stop(
      "ma_result is missing: ",
      paste(missing_elements, collapse = ", "),
      call. = FALSE
    )
  }
  
  if (is.null(rownames(ma_result$post.distns))) {
    stop("ma_result$post.distns must use trait names as row names.", call. = FALSE)
  }
  
  requested_traits <- unique(as.character(traits))
  available_traits <- rownames(ma_result$post.distns)
  missing_traits <- setdiff(requested_traits, available_traits)
  if (length(missing_traits) > 0L) {
    warning(
      "Ignoring traits absent from post.distns: ",
      paste(missing_traits, collapse = ", "),
      call. = FALSE
    )
  }
  keep_traits <- requested_traits[requested_traits %in% available_traits]
  
  list(
    trait.mcmc = ma_result$trait.mcmc[
      keep_traits[keep_traits %in% names(ma_result$trait.mcmc)]
    ],
    post.distns = ma_result$post.distns[keep_traits, , drop = FALSE],
    jagged.data = ma_result$jagged.data[
      keep_traits[keep_traits %in% names(ma_result$jagged.data)]
    ]
  )
}


# Post-meta-analysis quality control.
#
# This function does not repair data after MA. It identifies traits that are
# safe to pass forward, traits requiring manual review, and traits that failed
# hard checks. Unit or range problems must be corrected upstream and rerun.
qc_pecan_ma_result <- function(
    ma_result,
    trait.data,
    outdir = NULL,
    range_rules = NULL,
    min_rows = 2L,
    min_unique_values = 2L,
    rhat_threshold = 1.2,
    min_effective_size = 200,
    extreme_positive_span_ratio = 1000,
    min_posterior_se_ratio = 0.05,
    max_posterior_se_ratio = 20,
    require_error_stats = FALSE,
    save_filtered = TRUE
) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package `data.table` is required.", call. = FALSE)
  }
  if (!requireNamespace("coda", quietly = TRUE)) {
    stop("Package `coda` is required.", call. = FALSE)
  }
  
  required_elements <- c("trait.mcmc", "post.distns", "jagged.data")
  missing_elements <- setdiff(required_elements, names(ma_result))
  if (length(missing_elements) > 0L) {
    stop(
      "ma_result is missing: ",
      paste(missing_elements, collapse = ", "),
      call. = FALSE
    )
  }
  if (!is.list(trait.data) || is.null(names(trait.data))) {
    stop("trait.data must be a named list.", call. = FALSE)
  }
  if (is.null(rownames(ma_result$post.distns))) {
    stop("ma_result$post.distns must use trait names as row names.", call. = FALSE)
  }
  
  min_rows <- as.integer(min_rows)
  min_unique_values <- as.integer(min_unique_values)
  
  # Optional strict physical-range table:
  # trait | lower | upper | action (FAIL or REVIEW) | expected_unit (optional)
  if (!is.null(range_rules)) {
    range_rules <- as.data.frame(range_rules, stringsAsFactors = FALSE)
    required_range_columns <- c("trait", "lower", "upper")
    missing_range_columns <- setdiff(
      required_range_columns,
      names(range_rules)
    )
    if (length(missing_range_columns) > 0L) {
      stop(
        "range_rules is missing: ",
        paste(missing_range_columns, collapse = ", "),
        call. = FALSE
      )
    }
    if (!"action" %in% names(range_rules)) {
      range_rules$action <- "FAIL"
    }
    if (!"expected_unit" %in% names(range_rules)) {
      range_rules$expected_unit <- NA_character_
    }
    range_rules$action <- toupper(as.character(range_rules$action))
    if (any(!range_rules$action %in% c("FAIL", "REVIEW"))) {
      stop("range_rules$action must be FAIL or REVIEW.", call. = FALSE)
    }
    if (anyDuplicated(range_rules$trait)) {
      stop("range_rules contains duplicated trait names.", call. = FALSE)
    }
  }
  
  to_numeric <- function(x) {
    suppressWarnings(as.numeric(as.character(x)))
  }
  
  count_unique <- function(data, column) {
    if (is.null(data) || !column %in% names(data)) {
      return(NA_integer_)
    }
    values <- data[[column]]
    values <- values[!is.na(values)]
    length(unique(values))
  }
  
  trait_names <- rownames(ma_result$post.distns)
  
  qc_rows <- lapply(trait_names, function(trait_name) {
    input_data <- trait.data[[trait_name]]
    analyzed_data <- ma_result$jagged.data[[trait_name]]
    
    if (is.null(input_data)) {
      input_data <- data.frame()
    }
    if (is.null(analyzed_data)) {
      analyzed_data <- data.frame()
    }
    
    input_values <- if ("mean" %in% names(input_data)) {
      to_numeric(input_data$mean)
    } else {
      numeric()
    }
    analyzed_values <- if ("Y" %in% names(analyzed_data)) {
      to_numeric(analyzed_data$Y)
    } else {
      input_values
    }
    analyzed_values <- analyzed_values[is.finite(analyzed_values)]
    
    input_stat <- if ("stat" %in% names(input_data)) {
      to_numeric(input_data$stat)
    } else {
      rep(NA_real_, nrow(input_data))
    }
    input_n <- if ("n" %in% names(input_data)) {
      to_numeric(input_data$n)
    } else {
      rep(NA_real_, nrow(input_data))
    }
    
    n_input <- nrow(input_data)
    n_analyzed <- length(analyzed_values)
    n_unique <- length(unique(analyzed_values))
    n_valid_stat <- sum(is.finite(input_stat) & input_stat > 0)
    n_invalid_n <- sum(!is.finite(input_n) | input_n <= 0)
    n_greenhouse_input <- if ("greenhouse" %in% names(input_data)) {
      sum(to_numeric(input_data$greenhouse) == 1, na.rm = TRUE)
    } else {
      NA_integer_
    }
    
    data_min <- if (n_analyzed > 0L) min(analyzed_values) else NA_real_
    data_q25 <- if (n_analyzed > 0L) {
      unname(stats::quantile(analyzed_values, 0.25, names = FALSE))
    } else {
      NA_real_
    }
    data_median <- if (n_analyzed > 0L) {
      stats::median(analyzed_values)
    } else {
      NA_real_
    }
    data_q75 <- if (n_analyzed > 0L) {
      unname(stats::quantile(analyzed_values, 0.75, names = FALSE))
    } else {
      NA_real_
    }
    data_max <- if (n_analyzed > 0L) max(analyzed_values) else NA_real_
    data_mean <- if (n_analyzed > 0L) mean(analyzed_values) else NA_real_
    data_sd <- if (n_analyzed > 1L) stats::sd(analyzed_values) else NA_real_
    
    positive_span_ratio <- if (
      is.finite(data_min) &&
      is.finite(data_max) &&
      data_min > 0
    ) {
      data_max / data_min
    } else {
      NA_real_
    }
    
    mcmc_object <- ma_result$trait.mcmc[[trait_name]]
    mcmc_retained <- !is.null(mcmc_object)
    has_beta_o <- FALSE
    posterior_values <- numeric()
    posterior_median <- NA_real_
    posterior_sd <- NA_real_
    posterior_q025 <- NA_real_
    posterior_q975 <- NA_real_
    rhat_beta_o <- NA_real_
    effective_size_beta_o <- NA_real_
    
    if (mcmc_retained && length(mcmc_object) > 0L) {
      first_chain_names <- colnames(mcmc_object[[1L]])
      has_beta_o <- "beta.o" %in% first_chain_names
      
      if (has_beta_o) {
        beta_chains <- lapply(mcmc_object, function(chain) {
          beta_matrix <- as.matrix(chain)[, "beta.o", drop = FALSE]
          coda::mcmc(beta_matrix)
        })
        beta_mcmc <- do.call(coda::mcmc.list, beta_chains)
        posterior_values <- unlist(
          lapply(beta_chains, as.numeric),
          use.names = FALSE
        )
        posterior_values <- posterior_values[
          is.finite(posterior_values)
        ]
        
        if (length(posterior_values) > 0L) {
          posterior_median <- stats::median(posterior_values)
          posterior_sd <- stats::sd(posterior_values)
          posterior_q025 <- unname(stats::quantile(
            posterior_values,
            0.025,
            names = FALSE
          ))
          posterior_q975 <- unname(stats::quantile(
            posterior_values,
            0.975,
            names = FALSE
          ))
        }
        
        effective_size_beta_o <- tryCatch(
          as.numeric(coda::effectiveSize(beta_mcmc)[[1L]]),
          error = function(e) NA_real_
        )
        if (length(beta_chains) > 1L) {
          rhat_beta_o <- tryCatch(
            as.numeric(
              coda::gelman.diag(
                beta_mcmc,
                autoburnin = FALSE,
                multivariate = FALSE
              )$psrf[1L, "Point est."]
            ),
            error = function(e) NA_real_
          )
        }
      }
    }
    
    expected_sampling_se <- if (
      is.finite(data_sd) &&
      data_sd > 0 &&
      n_analyzed > 1L
    ) {
      data_sd / sqrt(n_analyzed)
    } else {
      NA_real_
    }
    posterior_se_ratio <- if (
      is.finite(posterior_sd) &&
      is.finite(expected_sampling_se) &&
      expected_sampling_se > 0
    ) {
      posterior_sd / expected_sampling_se
    } else {
      NA_real_
    }
    
    range_lower <- NA_real_
    range_upper <- NA_real_
    range_action <- NA_character_
    expected_unit <- NA_character_
    n_outside_range <- NA_integer_
    
    if (!is.null(range_rules) && trait_name %in% range_rules$trait) {
      rule <- range_rules[range_rules$trait == trait_name, , drop = FALSE]
      range_lower <- to_numeric(rule$lower[[1L]])
      range_upper <- to_numeric(rule$upper[[1L]])
      range_action <- rule$action[[1L]]
      expected_unit <- as.character(rule$expected_unit[[1L]])
      n_outside_range <- sum(
        analyzed_values < range_lower |
          analyzed_values > range_upper,
        na.rm = TRUE
      )
    }
    
    fail_flags <- character()
    review_flags <- character()
    
    if (n_analyzed < min_rows) {
      fail_flags <- c(fail_flags, "too_few_analyzed_rows")
    }
    if (n_unique < min_unique_values) {
      fail_flags <- c(fail_flags, "too_few_unique_values")
    }
    if (!mcmc_retained || !has_beta_o) {
      fail_flags <- c(fail_flags, "mcmc_not_retained_or_beta_o_missing")
    }
    if (has_beta_o && length(posterior_values) == 0L) {
      fail_flags <- c(fail_flags, "nonfinite_beta_o_posterior")
    }
    if (is.finite(rhat_beta_o) && rhat_beta_o > rhat_threshold) {
      fail_flags <- c(fail_flags, "rhat_above_threshold")
    }
    if (
      !is.na(n_outside_range) &&
      n_outside_range > 0L &&
      identical(range_action, "FAIL")
    ) {
      fail_flags <- c(fail_flags, "outside_physical_range")
    }
    
    if (
      !is.na(n_outside_range) &&
      n_outside_range > 0L &&
      identical(range_action, "REVIEW")
    ) {
      review_flags <- c(review_flags, "outside_review_range")
    }
    if (
      is.finite(positive_span_ratio) &&
      positive_span_ratio > extreme_positive_span_ratio
    ) {
      review_flags <- c(review_flags, "extreme_positive_span_ratio")
    }
    if (
      is.finite(posterior_se_ratio) &&
      posterior_se_ratio < min_posterior_se_ratio
    ) {
      review_flags <- c(review_flags, "posterior_unusually_narrow")
    }
    if (
      is.finite(posterior_se_ratio) &&
      posterior_se_ratio > max_posterior_se_ratio
    ) {
      review_flags <- c(review_flags, "posterior_unusually_wide")
    }
    if (
      is.finite(posterior_median) &&
      is.finite(data_min) &&
      is.finite(data_max) &&
      (posterior_median < data_min || posterior_median > data_max)
    ) {
      review_flags <- c(review_flags, "posterior_median_outside_data_range")
    }
    if (
      is.finite(effective_size_beta_o) &&
      effective_size_beta_o < min_effective_size
    ) {
      review_flags <- c(review_flags, "low_effective_sample_size")
    }
    if (n_invalid_n > 0L) {
      review_flags <- c(review_flags, "invalid_sample_size_n")
    }
    if (isTRUE(require_error_stats) && n_valid_stat == 0L) {
      review_flags <- c(review_flags, "no_valid_error_statistics")
    }
    
    qc_status <- if (length(fail_flags) > 0L) {
      "FAIL"
    } else if (length(review_flags) > 0L) {
      "REVIEW"
    } else {
      "PASS"
    }
    
    all_flags <- c(
      paste0("FAIL:", fail_flags),
      paste0("REVIEW:", review_flags)
    )
    
    data.frame(
      trait = trait_name,
      status = qc_status,
      use_for_bridge = identical(qc_status, "PASS"),
      flags = if (length(all_flags) == 0L) "" else paste(
        all_flags,
        collapse = ";"
      ),
      n_input = n_input,
      n_analyzed = n_analyzed,
      n_unique_values = n_unique,
      n_species = count_unique(input_data, "specie_id"),
      n_citations = count_unique(input_data, "citation_id"),
      n_sites_input = count_unique(input_data, "site_id"),
      n_sites_analyzed = count_unique(analyzed_data, "site"),
      n_greenhouse_input = n_greenhouse_input,
      n_valid_stat = n_valid_stat,
      n_invalid_n = n_invalid_n,
      data_min = data_min,
      data_q25 = data_q25,
      data_median = data_median,
      data_q75 = data_q75,
      data_max = data_max,
      data_mean = data_mean,
      data_sd = data_sd,
      positive_span_ratio = positive_span_ratio,
      posterior_median = posterior_median,
      posterior_sd = posterior_sd,
      posterior_q025 = posterior_q025,
      posterior_q975 = posterior_q975,
      expected_sampling_se = expected_sampling_se,
      posterior_se_ratio = posterior_se_ratio,
      rhat_beta_o = rhat_beta_o,
      effective_size_beta_o = effective_size_beta_o,
      mcmc_retained = mcmc_retained,
      range_lower = range_lower,
      range_upper = range_upper,
      expected_unit = expected_unit,
      n_outside_range = n_outside_range,
      stringsAsFactors = FALSE
    )
  })
  
  qc_summary <- data.table::rbindlist(qc_rows, fill = TRUE)
  qc_summary[, status_order := match(status, c("FAIL", "REVIEW", "PASS"))]
  data.table::setorder(qc_summary, status_order, trait)
  qc_summary[, status_order := NULL]
  
  passed_traits <- qc_summary[status == "PASS", trait]
  review_traits <- qc_summary[status == "REVIEW", trait]
  failed_traits <- qc_summary[status == "FAIL", trait]
  
  ma_result_qc <- subset_pecan_ma_result(ma_result, passed_traits)
  
  if (!is.null(outdir)) {
    dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
    data.table::fwrite(
      qc_summary,
      file.path(outdir, "ma_qc_summary.csv")
    )
    
    qc_decisions <- list(
      passed_traits = passed_traits,
      review_traits = review_traits,
      failed_traits = failed_traits,
      thresholds = list(
        min_rows = min_rows,
        min_unique_values = min_unique_values,
        rhat_threshold = rhat_threshold,
        min_effective_size = min_effective_size,
        extreme_positive_span_ratio = extreme_positive_span_ratio,
        min_posterior_se_ratio = min_posterior_se_ratio,
        max_posterior_se_ratio = max_posterior_se_ratio,
        require_error_stats = require_error_stats
      ),
      range_rules = range_rules
    )
    saveRDS(qc_decisions, file.path(outdir, "ma_qc_decisions.rds"))
    
    if (isTRUE(save_filtered)) {
      trait.mcmc <- ma_result_qc$trait.mcmc
      post.distns <- ma_result_qc$post.distns
      jagged.data <- ma_result_qc$jagged.data
      
      save(
        trait.mcmc,
        file = file.path(outdir, "trait.mcmc.QC_PASS.Rdata"),
        compress = FALSE
      )
      save(
        post.distns,
        file = file.path(outdir, "post.distns.QC_PASS.Rdata"),
        compress = FALSE
      )
      save(
        jagged.data,
        file = file.path(outdir, "jagged.data.QC_PASS.Rdata"),
        compress = FALSE
      )
    }
  }
  
  message(
    "MA QC complete. PASS: ", length(passed_traits),
    "; REVIEW: ", length(review_traits),
    "; FAIL: ", length(failed_traits), "."
  )
  
  list(
    summary = qc_summary,
    passed_traits = passed_traits,
    review_traits = review_traits,
    failed_traits = failed_traits,
    ma_result_qc = ma_result_qc
  )
}


# ============================================================
# Example calls
# ============================================================

# source("/path/to/run_pecan_ma_parallel.R")
#
# pftname <- "CroplandPool__Cereal_Croplands"
#
# ma_out_dir <- file.path(
#   "/projectnb/dietzelab/guYANG",
#   "SIPNET_Model_Calibration",
#   "TRY_MA_RESULTS_PARALLEL",
#   pftname
# )
#
# ma_result <- run_pecan_ma_parallel(
#   trait.data = trait.data,
#   prior.distns = prior.distns,
#   pft_name = pftname,
#   outdir = ma_out_dir,
#   iterations = 3000L,
#   random = TRUE,
#   threshold = 1.2,
#   use_ghs = FALSE,
#   gamma_tau = 0.01,
#   workers = 2L,
#   resume = TRUE,
#   save_combined = TRUE
# )
#
# trait.mcmc <- ma_result$trait.mcmc
# post.distns <- ma_result$post.distns
# jagged.data <- ma_result$jagged.data
#
# Complete TRY -> MA pipeline using the three objects already in the R session:
# trydat_use_species, trait_map, and pftspecies.
#
# pipeline_result <- run_single_pft_try_ma_pipeline_parallel(
#   pftname = "CroplandPool__Cereal_Croplands",
#   out_dir = file.path(
#     "/projectnb/dietzelab/guYANG",
#     "SIPNET_Model_Calibration",
#     "TRY_MA_PIPELINE_PARALLEL",
#     "CroplandPool__Cereal_Croplands"
#   ),
#   iterations = 3000L,
#   random = TRUE,
#   threshold = 1.2,
#   use_ghs = FALSE,
#   gamma_tau = 0.01,
#   workers = 2L,
#   sample_fraction = 0.05,
#   min_sample_n = 1L,
#   max_sample_n = 2L,
#   width_multiplier = 10,
#   relative_sd_floor = 0.10,
#   seed = 20260827L,
#   resume = TRUE
# )
#
# trait.mcmc <- pipeline_result$trait.mcmc
# post.distns <- pipeline_result$post.distns
# jagged.data <- pipeline_result$jagged.data
