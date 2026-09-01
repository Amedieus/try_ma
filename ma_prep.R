library(PEcAn.all)
library(PEcAn.SIPNET)
library(PEcAn.uncertainty)
library(PEcAn.settings)
library(future)
library(furrr)
library(data.table)
library(neonSoilFlux)

load("/projectnb/dietzelab/guYANG/TRY_meta_analysis/trydat_use_species.RData")
load("/projectnb/dietzelab/guYANG/TRY_meta_analysis/pftspecies.RData")
source("/projectnb/dietzelab/guYANG/TRY_meta_analysis/try_ma/pecan_to_sipnet.R")
source("/projectnb/dietzelab/guYANG/TRY_meta_analysis/try_ma/md_pecan_bridge.R")
source("/projectnb/dietzelab/guYANG/TRY_meta_analysis/try_ma/run_ma_parallel.R")
source("/projectnb/dietzelab/guYANG/TRY_meta_analysis/try_ma/generate_prior_distns.R")
source("/projectnb/dietzelab/guYANG/TRY_meta_analysis/try_ma/ma_functions.R")
source("/projectnb/dietzelab/guYANG/TRY_meta_analysis/try_ma/pecan_traits.R")
source("/projectnb/dietzelab/guYANG/TRY_meta_analysis/try_ma/functions_for_try_data.R")


##### Generate try_data (trait_map is generated manually)
library(data.table)

# ============================================================
# 0. PFT及统一输出目录
# ============================================================

pftname <- "Permanent_Wetlands"

pipeline_out_dir <- file.path(
  "/projectnb/dietzelab/guYANG",
  "TRY_meta_analysis",
  pftname
)

prep_dir <- file.path(
  pipeline_out_dir,
  "01_preparation"
)

ma_dir <- file.path(
  pipeline_out_dir,
  "02_meta_analysis"
)

qc_dir <- file.path(
  pipeline_out_dir,
  "03_ma_qc"
)

writer_dir <- file.path(
  pipeline_out_dir,
  "04_writer_ready_pecan_all_enabled"
)

for (dir_i in c(
  pipeline_out_dir,
  prep_dir,
  ma_dir,
  qc_dir,
  writer_dir
)) {
  dir.create(
    dir_i,
    recursive = TRUE,
    showWarnings = FALSE
  )
}


# ============================================================
# 1. 生成并检查 unit map
# ============================================================

unit_map <- build_try_unit_map(
  trait_map = trait_map
)

unit_conversion_test <-
  validate_try_unit_conversion_examples()

print(
  unit_conversion_test,
  nrows = Inf
)


# ============================================================
# 2. 提取单PFT TRY数据并统一单位
# ============================================================

try_data <- prepare_single_pft_try_data_for_ma(
  trydat_use_species = trydat_use_species,
  pftname = pftname,
  trait_map = trait_map,
  unit_map = unit_map,
  pft_species_map = pftspecies,
  
  unit_col = "UnitName",
  value_col = "StdValue",
  
  drop_errorrisk = TRUE,
  unsupported_action = "stop",
  ambiguous_species_action = "warn"
)

setDT(try_data)


# ============================================================
# 3. 检查并修复错误的 Replicates
#
# 无效Replicates不删除trait value；
# 设为NA后，format_try_for_ma()会保守生成n=1。
# ============================================================

invalid_replicates <- rep(
  FALSE,
  nrow(try_data)
)

if ("Replicates" %in% names(try_data)) {
  
  replicates_text <- trimws(
    as.character(
      try_data$Replicates
    )
  )
  
  replicates_supplied <- (
    !is.na(replicates_text) &
      nzchar(replicates_text) &
      !tolower(replicates_text) %in%
      c(
        "na",
        "n/a",
        "nan",
        "null"
      )
  )
  
  replicates_numeric <- suppressWarnings(
    as.numeric(replicates_text)
  )
  
  invalid_replicates <- replicates_supplied & (
    is.na(replicates_numeric) |
      !is.finite(replicates_numeric) |
      replicates_numeric <= 0 |
      abs(
        replicates_numeric -
          round(replicates_numeric)
      ) >
      sqrt(.Machine$double.eps)
  )
}


# ============================================================
# 4. 保存无效Replicates审核表
# ============================================================

audit_columns <- intersect(
  c(
    "DatasetID",
    "Dataset",
    "ObsDataID",
    "ObservationID",
    "AccSpeciesID",
    "SpeciesName",
    "TraitName",
    "DataName",
    "OriglName",
    "OrigValueStr",
    "OrigUnitStr",
    "ValueKindName",
    "OrigUncertaintyStr",
    "UncertaintyName",
    "Replicates",
    "StdValue",
    "UnitName",
    "Reference",
    "Comment"
  ),
  names(try_data)
)

invalid_replicates_audit <- try_data[
  invalid_replicates,
  ..audit_columns
]

fwrite(
  invalid_replicates_audit,
  file.path(
    prep_dir,
    "invalid_replicates_audit.csv"
  )
)

if (any(invalid_replicates)) {
  
  warning(
    sum(invalid_replicates),
    " TRY records have invalid Replicates. ",
    "Their trait values are retained, but Replicates are set to NA; ",
    "PEcAn will conservatively assign n = 1.",
    call. = FALSE
  )
  
  # 保留trait value，只删除错误的样本量信息
  try_data[
    invalid_replicates,
    Replicates := NA_character_
  ]
}


# ============================================================
# 5. 获取最终 trait mapping
# ============================================================

trait_map_for_ma <- attr(
  try_data,
  "trait_map_for_ma"
)

if (is.null(trait_map_for_ma)) {
  trait_map_for_ma <- trait_map
}


# ============================================================
# 6. TRY data → PEcAn MA long format
# ============================================================

try_ma_long <-
  PEcAn.data.remote::format_try_for_ma(
    try_data = try_data,
    trait_map = trait_map_for_ma,
    species_map = NULL
  )

setDT(try_ma_long)

try_ma_long[
  ,
  final_pft := pftname
]


# ============================================================
# 7. 检查 try_ma_long
# ============================================================

if (nrow(try_ma_long) == 0L) {
  stop("try_ma_long 没有记录。")
}

if (any(!is.finite(try_ma_long$mean))) {
  stop(
    "try_ma_long$mean 仍然包含非有限值。"
  )
}

if (
  anyNA(try_ma_long$vname) ||
  any(!nzchar(try_ma_long$vname))
) {
  stop(
    "try_ma_long$vname 存在缺失或空字符串。"
  )
}


# ============================================================
# 8. 检查format之后的n
# ============================================================

n_numeric <- suppressWarnings(
  as.numeric(
    as.character(try_ma_long$n)
  )
)

invalid_n_after_format <- (
  is.na(n_numeric) |
    !is.finite(n_numeric) |
    n_numeric <= 0 |
    abs(
      n_numeric -
        round(n_numeric)
    ) >
    sqrt(.Machine$double.eps)
)

if (any(invalid_n_after_format)) {
  
  fwrite(
    try_ma_long[
      invalid_n_after_format
    ],
    file.path(
      prep_dir,
      "invalid_n_after_format.csv"
    )
  )
  
  stop(
    sum(invalid_n_after_format),
    " rows still contain invalid n after format_try_for_ma()."
  )
}

try_ma_long[
  ,
  n := as.integer(n_numeric)
]

print(
  try_ma_long,
  nrows = 20
)


# ============================================================
# 9. 保存准备阶段结果
# ============================================================

saveRDS(
  trait_map,
  file.path(
    prep_dir,
    "trait_map.rds"
  )
)

saveRDS(
  trait_map_for_ma,
  file.path(
    prep_dir,
    "trait_map_for_ma.rds"
  )
)

saveRDS(
  unit_map,
  file.path(
    prep_dir,
    "unit_map.rds"
  )
)

saveRDS(
  unit_conversion_test,
  file.path(
    prep_dir,
    "unit_conversion_test.rds"
  )
)

saveRDS(
  try_data,
  file.path(
    prep_dir,
    "try_data.rds"
  ),
  compress = FALSE
)

saveRDS(
  try_ma_long,
  file.path(
    prep_dir,
    "try_ma_long.rds"
  ),
  compress = FALSE
)


# ============================================================
# 10. 生成 trait.data
# ============================================================

trait.data <-
  make_trait_data_from_try_ma_long(
    try_ma_long
  )

save(
  trait.data,
  file = file.path(
    prep_dir,
    "trait.data.Rdata"
  ),
  compress = FALSE
)


# ============================================================
# 11. 生成 prior.distns
# ============================================================

prior.distns <-
  make_prior_distns_from_trait_data(
    trait.data = trait.data,
    sample_fraction = 0.05,
    min_sample_n = 1L,
    max_sample_n = 2L,
    width_multiplier = 10,
    relative_sd_floor = 0.10,
    seed = 20260827L
  )

save(
  prior.distns,
  file = file.path(
    prep_dir,
    "prior.distns.Rdata"
  ),
  compress = FALSE
)


# ============================================================
# 12. 并行 Meta-analysis
#
# 不再调用meta_analysis_standalone()；
# run_pecan_ma_parallel()内部会逐trait调用它。
# ============================================================

ma_result <-
  run_pecan_ma_parallel(
    trait.data = trait.data,
    prior.distns = prior.distns,
    
    pft_name = pftname,
    outdir = ma_dir,
    
    iterations = 3000L,
    random = FALSE,
    threshold = 1.2,
    use_ghs = FALSE,
    gamma_tau = 0.01,
    
    workers = 24L,
    resume = TRUE,
    save_combined = TRUE
  )

saveRDS(
  ma_result,
  file.path(
    ma_dir,
    "ma_result.rds"
  ),
  compress = FALSE
)


# ============================================================
# 13. MA QC
# ============================================================

ma_qc <-
  qc_pecan_ma_result(
    ma_result = ma_result,
    trait.data = trait.data,
    outdir = qc_dir
  )

saveRDS(
  ma_qc,
  file.path(
    qc_dir,
    "ma_qc_result.rds"
  ),
  compress = FALSE
)

if (length(ma_qc$passed_traits) == 0L) {
  stop(
    "没有任何通过QC的MA trait：",
    pftname
  )
}


# ============================================================
# 14. PASS + REVIEW保守bridge（审核用途）
# ============================================================

nonfail_bridge_dir <- file.path(
  qc_dir,
  "nonfail_bridge_PASS_REVIEW"
)

bridge_context <-
  default_md_bridge_context()

bridge_result <- tryCatch({
  
  bridge_ma_qc_to_pecan(
    ma_result = ma_result,
    ma_qc = ma_qc,
    out_dir = nonfail_bridge_dir,
    
    n_draws = 5000L,
    seed = 20260828L,
    
    keep_status = c(
      "PASS",
      "REVIEW"
    ),
    
    context = bridge_context,
    models = list(),
    
    strict = TRUE,
    save_ma_draws_csv = FALSE
  )
  
}, error = function(e) {
  
  warning(
    "PASS+REVIEW audit bridge failed: ",
    conditionMessage(e),
    call. = FALSE
  )
  
  writeLines(
    conditionMessage(e),
    file.path(
      qc_dir,
      "nonfail_bridge_error.txt"
    )
  )
  
  NULL
})


# ============================================================
# 15. 确保正确加载 writer bridge
# ============================================================

bridge_file <- file.path(
  script_dir,
  "md_to_pecan_bridge_v1.R"
)

if (!file.exists(bridge_file)) {
  stop(
    "找不到bridge文件：",
    bridge_file
  )
}

sys.source(
  bridge_file,
  envir = .GlobalEnv
)

stopifnot(
  is.function(sipnet_writer_traits_v2),
  length(sipnet_writer_traits_v2()) == 54L
)


# ============================================================
# 16. 生成最终writer-ready PEcAn traits
# ============================================================

writer_res_all <-
  run_pass_ma_to_writer_ready_pecan_all_enabled(
    pftname = pftname,
    
    # 使用你修改后的接口：
    # ma_root直接指向PFT文件夹
    ma_root = pipeline_out_dir,
    
    bridge_file = bridge_file,
    qc_subdir = "03_ma_qc",
    
    out_subdir =
      "04_writer_ready_pecan_all_enabled",
    
    n_draws = 6000L,
    seed = 20260828L,
    
    context_overrides = list(),
    model_overrides = list(),
    
    save_ma_draws_csv = FALSE
  )

saveRDS(
  writer_res_all,
  file.path(
    writer_res_all$out_dir,
    "writer_res_all.rds"
  ),
  compress = FALSE
)


# ============================================================
# 17. 最终检查
# ============================================================

cat(
  "\nWorkflow finished for:",
  pftname,
  "\n"
)

cat(
  "MA traits:",
  length(ma_result$trait.mcmc),
  "\n"
)

cat(
  "QC PASS/REVIEW/FAIL:",
  length(ma_qc$passed_traits),
  "/",
  length(ma_qc$review_traits),
  "/",
  length(ma_qc$failed_traits),
  "\n"
)

cat(
  "Writer-ready PEcAn traits:",
  length(writer_res_all$complete_writer_traits),
  "\n"
)

cat(
  "Writer posterior draws:",
  nrow(writer_res_all$writer_trait_draws),
  "\n"
)

cat(
  "Final output:",
  writer_res_all$out_dir,
  "\n"
)

print(
  writer_res_all$complete_writer_traits
)

print(
  writer_res_all$writer_trait_summary,
  nrows = Inf
)

##### Generate SIPNET priors
# pecan_sample_res <- writer_draws_to_pecan_samples(
#   writer_result = writer_res_all,
#   ensemble_size = 100L,
#   sample_method = "uniform"
# )