# =============================================================================
# Build PEcAn prior.distns from a small random sample of trait.data
#
# Default rule for each trait:
#   sample_n = clamp(ceiling(5% of finite observations), min = 1, max = 2)
#   distn    = "norm"
#   parama   = median(sampled values)
#   paramb   = 5 * max(sample SD, 10% of sampled-value magnitude)
#   n        = number of sampled observations
#
# The selected row indices and values are attached to prior.distns as:
#   attr(prior.distns, "prior_sample_rows")
#   attr(prior.distns, "prior_sample_values")
# =============================================================================

make_prior_distns_from_trait_data <- function(
    trait.data,
    sample_fraction = 0.05,
    min_sample_n = 1L,
    max_sample_n = 2L,
    width_multiplier = 5,
    relative_sd_floor = 0.10,
    seed = 20260827L
) {
  if (!is.list(trait.data) || length(trait.data) == 0L) {
    stop("trait.data 必须是非空的 named list。", call. = FALSE)
  }
  
  trait_names <- names(trait.data)
  if (
    is.null(trait_names) ||
    anyNA(trait_names) ||
    any(!nzchar(trimws(trait_names)))
  ) {
    stop("trait.data 的每个元素都必须有 trait 名称。", call. = FALSE)
  }
  if (anyDuplicated(trait_names)) {
    stop("trait.data 中存在重复的 trait 名称。", call. = FALSE)
  }
  
  if (
    length(sample_fraction) != 1L ||
    !is.finite(sample_fraction) ||
    sample_fraction <= 0 ||
    sample_fraction > 1
  ) {
    stop("sample_fraction 必须位于 (0, 1]。", call. = FALSE)
  }
  
  min_sample_n <- as.integer(min_sample_n)
  max_sample_n <- as.integer(max_sample_n)
  if (
    length(min_sample_n) != 1L ||
    length(max_sample_n) != 1L ||
    is.na(min_sample_n) ||
    is.na(max_sample_n) ||
    min_sample_n < 1L ||
    max_sample_n < min_sample_n
  ) {
    stop(
      "min_sample_n 和 max_sample_n 必须是正整数，且 max >= min。",
      call. = FALSE
    )
  }
  
  if (
    length(width_multiplier) != 1L ||
    !is.finite(width_multiplier) ||
    width_multiplier <= 0
  ) {
    stop("width_multiplier 必须是正的有限数值。", call. = FALSE)
  }
  
  if (
    length(relative_sd_floor) != 1L ||
    !is.finite(relative_sd_floor) ||
    relative_sd_floor < 0
  ) {
    stop("relative_sd_floor 必须是非负有限数值。", call. = FALSE)
  }
  
  if (
    length(seed) != 1L ||
    is.na(seed) ||
    !is.finite(seed)
  ) {
    stop("seed 必须是单个有限整数。", call. = FALSE)
  }
  
  # Sort names before sampling so the result is reproducible even if list order changes.
  trait_names <- sort(trait_names)
  set.seed(as.integer(seed))
  
  prior_rows <- vector("list", length(trait_names))
  names(prior_rows) <- trait_names
  
  sampled_row_indices <- vector("list", length(trait_names))
  names(sampled_row_indices) <- trait_names
  
  sampled_value_rows <- vector("list", length(trait_names))
  names(sampled_value_rows) <- trait_names
  
  for (trait_index in seq_along(trait_names)) {
    trait_name <- trait_names[[trait_index]]
    one_trait <- trait.data[[trait_name]]
    
    if (!is.data.frame(one_trait)) {
      stop(
        "trait.data[['", trait_name, "']] 不是 data.frame。",
        call. = FALSE
      )
    }
    if (!"mean" %in% names(one_trait)) {
      stop(
        "trait.data[['", trait_name, "']] 缺少 mean 列。",
        call. = FALSE
      )
    }
    
    values <- suppressWarnings(
      as.numeric(as.character(one_trait$mean))
    )
    finite_rows <- which(is.finite(values))
    n_available <- length(finite_rows)
    
    if (n_available == 0L) {
      stop(
        "trait ", trait_name, " 没有有限的 mean 值。",
        call. = FALSE
      )
    }
    
    requested_n <- ceiling(sample_fraction * n_available)
    sample_n <- max(min_sample_n, requested_n)
    sample_n <- min(max_sample_n, sample_n)
    sample_n <- min(n_available, sample_n)
    
    # sample.int avoids base::sample()'s special behavior when finite_rows
    # contains exactly one numeric row index larger than one.
    chosen_rows <- finite_rows[
      sample.int(
        n = n_available,
        size = sample_n,
        replace = FALSE
      )
    ]
    chosen_values <- values[chosen_rows]
    
    prior_center <- stats::median(chosen_values)
    
    sample_sd <- if (sample_n >= 2L) {
      stats::sd(chosen_values)
    } else {
      NA_real_
    }
    
    sampled_magnitude <- max(
      abs(c(chosen_values, prior_center)),
      na.rm = TRUE
    )
    sd_floor <- relative_sd_floor * sampled_magnitude
    
    base_sd_candidates <- c(sample_sd, sd_floor)
    base_sd_candidates <- base_sd_candidates[
      is.finite(base_sd_candidates) & base_sd_candidates > 0
    ]
    
    # If all sampled values are exactly zero, use 1 before widening.
    base_sd <- if (length(base_sd_candidates) > 0L) {
      max(base_sd_candidates)
    } else {
      1
    }
    
    prior_sd <- width_multiplier * base_sd
    
    prior_rows[[trait_index]] <- data.frame(
      distn = "norm",
      parama = as.numeric(prior_center),
      paramb = as.numeric(prior_sd),
      n = as.integer(sample_n),
      stringsAsFactors = FALSE
    )
    
    sampled_row_indices[[trait_index]] <- chosen_rows
    sampled_value_rows[[trait_index]] <- data.frame(
      trait = trait_name,
      trait_data_row = chosen_rows,
      sampled_mean = chosen_values,
      stringsAsFactors = FALSE
    )
  }
  
  prior.distns <- do.call(rbind, prior_rows)
  rownames(prior.distns) <- trait_names
  
  prior_sample_values <- do.call(
    rbind,
    sampled_value_rows
  )
  rownames(prior_sample_values) <- NULL
  
  attr(prior.distns, "prior_sample_rows") <- sampled_row_indices
  attr(prior.distns, "prior_sample_values") <- prior_sample_values
  attr(prior.distns, "prior_sampling_configuration") <- list(
    sample_fraction = sample_fraction,
    min_sample_n = min_sample_n,
    max_sample_n = max_sample_n,
    width_multiplier = width_multiplier,
    relative_sd_floor = relative_sd_floor,
    seed = as.integer(seed)
  )
  
  message(
    "prior.distns generated.",
    "\nTraits: ", nrow(prior.distns),
    "\nSampled observations: ", sum(prior.distns$n),
    "\nSample size range: ",
    min(prior.distns$n), " to ", max(prior.distns$n)
  )
  
  prior.distns
}


# =============================================================================
# Example
# =============================================================================

# prior.distns <- make_prior_distns_from_trait_data(trait.data)
# print(prior.distns)
# attr(prior.distns, "prior_sample_values")
# attr(prior.distns, "prior_sampling_configuration")
#
# save(
#   prior.distns,
#   file = file.path(out_dir, "prior.distns.Rdata")
# )
