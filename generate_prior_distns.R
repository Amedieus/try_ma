# =============================================================================
# Build domain-aware PEcAn prior.distns from trait.data.
#
# The current project still uses a small sample of the likelihood data to set
# prior location. This file only fixes the prior support/distribution bug:
#   * fractions and dry-matter fractions: beta on [0, 1]
#   * percentages: uniform on [0, 100]
#   * signed water potentials: normal
#   * other positive traits: lognormal (or gamma when requested)
#
# PEcAn expects R distribution parameterizations in columns:
#   distn, parama, paramb, n
# =============================================================================


build_trait_prior_registry <- function(
    unit_map,
    trait_names = NULL,
    positive_distribution = c("lnorm", "gamma")
) {
  positive_distribution <- match.arg(positive_distribution)

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package `data.table` is required.", call. = FALSE)
  }

  unit_dt <- data.table::copy(data.table::as.data.table(unit_map))
  required_columns <- c(
    "pecan_vname",
    "canonical_unit",
    "conversion_id"
  )
  missing_columns <- setdiff(required_columns, names(unit_dt))
  if (length(missing_columns) > 0L) {
    stop(
      "unit_map is missing columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  registry <- unique(
    unit_dt[
      !is.na(pecan_vname) & nzchar(trimws(as.character(pecan_vname))),
      .(
        trait = trimws(as.character(pecan_vname)),
        canonical_unit = trimws(as.character(canonical_unit)),
        conversion_id = trimws(as.character(conversion_id))
      )
    ]
  )

  conflicts <- registry[
    ,
    .(
      n_units = data.table::uniqueN(canonical_unit),
      n_conversion_ids = data.table::uniqueN(conversion_id)
    ),
    by = trait
  ][n_units != 1L | n_conversion_ids != 1L]

  if (nrow(conflicts) > 0L) {
    stop(
      "The following MA traits have conflicting unit-map definitions: ",
      paste(conflicts$trait, collapse = ", "),
      call. = FALSE
    )
  }

  registry <- registry[
    ,
    .(
      canonical_unit = canonical_unit[[1L]],
      conversion_id = conversion_id[[1L]]
    ),
    by = trait
  ]

  registry[
    ,
    domain := data.table::fcase(
      conversion_id %in% c(
        "fraction",
        "dry_matter_fraction",
        "quantum_yield"
      ),
      "unit_interval",
      conversion_id == "percent_mass",
      "percent_interval",
      conversion_id == "pressure_mpa",
      "signed",
      default = "positive"
    )
  ]

  registry[
    ,
    distribution := data.table::fcase(
      domain == "unit_interval",
      "beta",
      domain == "percent_interval",
      "unif",
      domain == "signed",
      "norm",
      default = positive_distribution
    )
  ]

  registry[
    ,
    `:=`(
      lower = data.table::fcase(
        domain == "unit_interval", 0,
        domain == "percent_interval", 0,
        default = NA_real_
      ),
      upper = data.table::fcase(
        domain == "unit_interval", 1,
        domain == "percent_interval", 100,
        default = NA_real_
      )
    )
  ]

  if (!is.null(trait_names)) {
    trait_names <- unique(trimws(as.character(trait_names)))
    missing_traits <- setdiff(trait_names, registry$trait)
    if (length(missing_traits) > 0L) {
      stop(
        "unit_map has no prior-domain definition for: ",
        paste(missing_traits, collapse = ", "),
        call. = FALSE
      )
    }
    registry <- registry[trait %in% trait_names]
  }

  data.table::setorder(registry, trait)
  registry[]
}


make_prior_distns_from_trait_data <- function(
    trait.data,
    sample_fraction = 0.05,
    min_sample_n = 1L,
    max_sample_n = 2L,
    width_multiplier = 5,
    relative_sd_floor = 0.10,
    seed = 20260827L,
    unit_map,
    prior_registry = NULL,
    positive_distribution = c("lnorm", "gamma"),
    domain_action = c("stop", "warn"),
    beta_concentration = 2,
    min_log_sd = 0.35,
    max_log_sd = 2
) {
  positive_distribution <- match.arg(positive_distribution)
  domain_action <- match.arg(domain_action)

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

  numeric_controls <- c(
    width_multiplier = width_multiplier,
    relative_sd_floor = relative_sd_floor,
    beta_concentration = beta_concentration,
    min_log_sd = min_log_sd,
    max_log_sd = max_log_sd
  )
  if (any(!is.finite(numeric_controls))) {
    stop("Prior control values must be finite.", call. = FALSE)
  }
  if (
    width_multiplier <= 0 ||
    relative_sd_floor < 0 ||
    beta_concentration <= 0 ||
    min_log_sd <= 0 ||
    max_log_sd < min_log_sd
  ) {
    stop("Invalid prior width/domain control values.", call. = FALSE)
  }

  if (length(seed) != 1L || is.na(seed) || !is.finite(seed)) {
    stop("seed 必须是单个有限整数。", call. = FALSE)
  }

  trait_names <- sort(trait_names)

  if (is.null(prior_registry)) {
    prior_registry <- build_trait_prior_registry(
      unit_map = unit_map,
      trait_names = trait_names,
      positive_distribution = positive_distribution
    )
  } else {
    prior_registry <- data.table::copy(
      data.table::as.data.table(prior_registry)
    )
  }

  required_registry_columns <- c(
    "trait",
    "canonical_unit",
    "conversion_id",
    "domain",
    "distribution",
    "lower",
    "upper"
  )
  missing_registry_columns <- setdiff(
    required_registry_columns,
    names(prior_registry)
  )
  if (length(missing_registry_columns) > 0L) {
    stop(
      "prior_registry is missing columns: ",
      paste(missing_registry_columns, collapse = ", "),
      call. = FALSE
    )
  }
  if (anyDuplicated(prior_registry$trait)) {
    stop("prior_registry$trait must be unique.", call. = FALSE)
  }

  missing_registry_traits <- setdiff(trait_names, prior_registry$trait)
  if (length(missing_registry_traits) > 0L) {
    stop(
      "prior_registry has no row for: ",
      paste(missing_registry_traits, collapse = ", "),
      call. = FALSE
    )
  }
  prior_registry <- prior_registry[
    match(trait_names, prior_registry$trait)
  ]

  set.seed(as.integer(seed))

  prior_rows <- vector("list", length(trait_names))
  sampled_row_indices <- vector("list", length(trait_names))
  sampled_value_rows <- vector("list", length(trait_names))
  audit_rows <- vector("list", length(trait_names))
  names(prior_rows) <- trait_names
  names(sampled_row_indices) <- trait_names
  names(sampled_value_rows) <- trait_names
  names(audit_rows) <- trait_names

  for (trait_index in seq_along(trait_names)) {
    trait_name <- trait_names[[trait_index]]
    one_trait <- trait.data[[trait_name]]
    rule <- prior_registry[trait == trait_name][1L]
    rule_domain <- as.character(rule$domain)

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

    values <- suppressWarnings(as.numeric(as.character(one_trait$mean)))
    finite_rows <- which(is.finite(values))
    if (length(finite_rows) == 0L) {
      stop("trait ", trait_name, " 没有有限的 mean 值。", call. = FALSE)
    }

    domain_valid <- switch(
      rule_domain,
      unit_interval = is.finite(values) & values >= 0 & values <= 1,
      percent_interval = is.finite(values) & values >= 0 & values <= 100,
      positive = is.finite(values) & values > 0,
      signed = is.finite(values),
      stop(
        "Unsupported prior domain for ", trait_name, ": ", rule_domain,
        call. = FALSE
      )
    )

    invalid_domain_rows <- which(is.finite(values) & !domain_valid)
    if (length(invalid_domain_rows) > 0L) {
      domain_message <- paste0(
        "trait ", trait_name, " has ", length(invalid_domain_rows),
        " finite observation(s) outside prior domain `", rule_domain, "`."
      )
      if (domain_action == "stop") {
        stop(domain_message, call. = FALSE)
      }
      warning(domain_message, call. = FALSE)
    }

    usable_rows <- which(domain_valid)
    n_available <- length(usable_rows)
    if (n_available == 0L) {
      stop(
        "trait ", trait_name, " has no observation inside prior domain `",
        rule_domain, "`.",
        call. = FALSE
      )
    }

    requested_n <- ceiling(sample_fraction * n_available)
    sample_n <- max(min_sample_n, requested_n)
    sample_n <- min(max_sample_n, sample_n, n_available)

    chosen_rows <- usable_rows[
      sample.int(n = n_available, size = sample_n, replace = FALSE)
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
    base_sd <- if (length(base_sd_candidates) > 0L) {
      max(base_sd_candidates)
    } else {
      1
    }
    prior_sd <- width_multiplier * base_sd

    distribution <- as.character(rule$distribution)
    prior_parameters <- switch(
      distribution,
      norm = c(parama = prior_center, paramb = prior_sd),
      unif = {
        if (!is.finite(rule$lower) || !is.finite(rule$upper)) {
          stop("Uniform prior requires finite bounds for ", trait_name, call. = FALSE)
        }
        c(parama = rule$lower, paramb = rule$upper)
      },
      beta = {
        beta_mean <- pmax(1e-4, pmin(1 - 1e-4, prior_center))
        c(
          parama = beta_mean * beta_concentration,
          paramb = (1 - beta_mean) * beta_concentration
        )
      },
      lnorm = {
        if (!is.finite(prior_center) || prior_center <= 0) {
          stop("Lognormal prior requires a positive center for ", trait_name, call. = FALSE)
        }
        relative_prior_sd <- prior_sd / prior_center
        log_sd <- sqrt(log1p(relative_prior_sd^2))
        log_sd <- max(min_log_sd, min(max_log_sd, log_sd))
        c(parama = log(prior_center), paramb = log_sd)
      },
      gamma = {
        if (!is.finite(prior_center) || prior_center <= 0) {
          stop("Gamma prior requires a positive center for ", trait_name, call. = FALSE)
        }
        gamma_sd <- max(prior_sd, .Machine$double.eps)
        gamma_shape <- (prior_center / gamma_sd)^2
        gamma_rate <- prior_center / gamma_sd^2
        c(
          parama = max(gamma_shape, 1e-4),
          paramb = max(gamma_rate, 1e-8)
        )
      },
      stop(
        "Unsupported prior distribution for ", trait_name, ": ",
        distribution,
        call. = FALSE
      )
    )

    prior_rows[[trait_index]] <- data.frame(
      distn = distribution,
      parama = as.numeric(prior_parameters[["parama"]]),
      paramb = as.numeric(prior_parameters[["paramb"]]),
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
    audit_rows[[trait_index]] <- data.frame(
      trait = trait_name,
      canonical_unit = rule$canonical_unit,
      conversion_id = rule$conversion_id,
      domain = rule_domain,
      distribution = distribution,
      lower = rule$lower,
      upper = rule$upper,
      n_finite = length(finite_rows),
      n_domain_valid = n_available,
      n_domain_invalid = length(invalid_domain_rows),
      sampled_center = prior_center,
      sampled_sd = sample_sd,
      widened_prior_sd = prior_sd,
      parama = as.numeric(prior_parameters[["parama"]]),
      paramb = as.numeric(prior_parameters[["paramb"]]),
      stringsAsFactors = FALSE
    )
  }

  prior.distns <- do.call(rbind, prior_rows)
  rownames(prior.distns) <- trait_names

  prior_sample_values <- do.call(rbind, sampled_value_rows)
  rownames(prior_sample_values) <- NULL
  prior_parameter_audit <- data.table::rbindlist(
    audit_rows,
    use.names = TRUE,
    fill = TRUE
  )

  attr(prior.distns, "prior_sample_rows") <- sampled_row_indices
  attr(prior.distns, "prior_sample_values") <- prior_sample_values
  attr(prior.distns, "prior_registry") <- prior_registry
  attr(prior.distns, "prior_parameter_audit") <- prior_parameter_audit
  attr(prior.distns, "prior_sampling_configuration") <- list(
    sample_fraction = sample_fraction,
    min_sample_n = min_sample_n,
    max_sample_n = max_sample_n,
    width_multiplier = width_multiplier,
    relative_sd_floor = relative_sd_floor,
    seed = as.integer(seed),
    positive_distribution = positive_distribution,
    domain_action = domain_action,
    beta_concentration = beta_concentration,
    min_log_sd = min_log_sd,
    max_log_sd = max_log_sd
  )

  distribution_counts <- table(prior.distns$distn)
  message(
    "Domain-aware prior.distns generated.",
    "\nTraits: ", nrow(prior.distns),
    "\nDistribution counts: ",
    paste(
      paste(names(distribution_counts), distribution_counts, sep = "="),
      collapse = "; "
    ),
    "\nSampled observations: ", sum(prior.distns$n),
    "\nSample size range: ",
    min(prior.distns$n), " to ", max(prior.distns$n)
  )

  prior.distns
}


# Example:
# unit_map <- build_try_unit_map(trait_map)
# prior.distns <- make_prior_distns_from_trait_data(
#   trait.data = trait.data,
#   unit_map = unit_map,
#   positive_distribution = "lnorm"
# )
