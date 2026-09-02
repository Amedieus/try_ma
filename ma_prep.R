options(stringsAsFactors = FALSE)


# ============================================================
# 0. RStudio 用户配置：通常只需要修改 PFT_NAME
#
# 使用方法：
#   1. 修改 PFT_NAME；
#   2. 如有需要，修改三个目录；
#   3. 在 RStudio 中点击 Source。
#
# 本脚本只运行：TRY preparation -> prior -> MA -> QC。
# 不运行 bridge，也不运行 SIPNET writer。
# ============================================================

PFT_NAME <- "Permanent_Wetlands"

CODE_DIR <- paste0(
  "/projectnb/dietzelab/guYANG/",
  "TRY_meta_analysis/try_ma"
)

DATA_ROOT <- paste0(
  "/projectnb/dietzelab/guYANG/",
  "TRY_meta_analysis"
)

OUTPUT_ROOT <- paste0(
  "/projectnb/dietzelab/guYANG/",
  "TRY_meta_analysis"
)

MA_ITERATIONS <- 3000L
MA_WORKERS <- 2L


# ============================================================
# 1. 基础检查和辅助函数
# ============================================================

MA_PREP_VERSION <- "MA_PREP_RSTUDIO_V1_2026-09-01"

load_rdata_object <- function(path, object_name) {
  if (!file.exists(path)) {
    stop("找不到输入文件：", path, call. = FALSE)
  }
  
  input_env <- new.env(parent = emptyenv())
  loaded_names <- load(path, envir = input_env)
  
  if (!object_name %in% loaded_names) {
    stop(
      path,
      " 中没有对象 `",
      object_name,
      "`；实际包含：",
      paste(loaded_names, collapse = ", "),
      call. = FALSE
    )
  }
  
  get(object_name, envir = input_env, inherits = FALSE)
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
    stop(name, " 必须是正整数。", call. = FALSE)
  }
  
  as.integer(value_numeric)
}

pftname <- trimws(as.character(PFT_NAME))

if (length(pftname) != 1L || is.na(pftname) || !nzchar(pftname)) {
  stop("请在脚本顶部填写一个有效的 PFT_NAME。", call. = FALSE)
}

if (grepl("[/\\\\]", pftname)) {
  stop("PFT_NAME 不能包含 / 或 \\\\。", call. = FALSE)
}

ma_iterations <- check_positive_integer(
  MA_ITERATIONS,
  "MA_ITERATIONS"
)

ma_workers <- check_positive_integer(
  MA_WORKERS,
  "MA_WORKERS"
)

if (!dir.exists(CODE_DIR)) {
  stop("CODE_DIR 不存在：", CODE_DIR, call. = FALSE)
}

if (!dir.exists(DATA_ROOT)) {
  stop("DATA_ROOT 不存在：", DATA_ROOT, call. = FALSE)
}

code_dir <- normalizePath(CODE_DIR, mustWork = TRUE)
data_root <- normalizePath(DATA_ROOT, mustWork = TRUE)

dir.create(
  OUTPUT_ROOT,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!dir.exists(OUTPUT_ROOT)) {
  stop("无法创建 OUTPUT_ROOT：", OUTPUT_ROOT, call. = FALSE)
}

output_root <- normalizePath(OUTPUT_ROOT, mustWork = TRUE)


# ============================================================
# 2. 检查 R 包并加载本地源码
# ============================================================

required_packages <- c(
  "data.table",
  "future",
  "furrr",
  "coda",
  "PEcAn.MA",
  "PEcAn.data.remote"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0L) {
  stop(
    "缺少以下 R 包：",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

suppressPackageStartupMessages(
  library(data.table)
)

source_files <- c(
  "ma_functions.R",
  "functions_for_try_data.R",
  "generate_prior_distns.R",
  "run_ma_parallel.R"
)

missing_source_files <- source_files[
  !file.exists(file.path(code_dir, source_files))
]

if (length(missing_source_files) > 0L) {
  stop(
    "CODE_DIR 中找不到以下代码文件：",
    paste(missing_source_files, collapse = ", "),
    call. = FALSE
  )
}

for (source_file in source_files) {
  sys.source(
    file.path(code_dir, source_file),
    envir = .GlobalEnv
  )
}


# ============================================================
# 3. 读取输入数据
# ============================================================

trydat_use_species <- load_rdata_object(
  file.path(data_root, "trydat_use_species.RData"),
  "trydat_use_species"
)

pftspecies <- load_rdata_object(
  file.path(data_root, "pftspecies.RData"),
  "pftspecies"
)

pft_name_column <- intersect(
  c(
    "final_pft",
    "pftname",
    "pft_name",
    "pft",
    "PFT"
  ),
  names(pftspecies)
)

if (length(pft_name_column) > 0L) {
  pft_name_column <- pft_name_column[[1L]]
  
  available_pfts <- sort(unique(
    trimws(as.character(
      pftspecies[[pft_name_column]]
    ))
  ))
  
  available_pfts <- available_pfts[
    !is.na(available_pfts) & nzchar(available_pfts)
  ]
  
  if (!pftname %in% available_pfts) {
    stop(
      "PFT_NAME `",
      pftname,
      "` 不在 pftspecies$",
      pft_name_column,
      " 中。\n可用的 PFT 名称为：\n",
      paste(available_pfts, collapse = "\n"),
      call. = FALSE
    )
  }
}

message("============================================================")
message("Running ", MA_PREP_VERSION)
message("PFT: ", pftname)
message("CODE_DIR: ", code_dir)
message("DATA_ROOT: ", data_root)
message("OUTPUT_ROOT: ", output_root)
message("MA iterations: ", ma_iterations)
message("MA workers: ", ma_workers)
message("Bridge and SIPNET writer are disabled in this script.")
message("============================================================")

# ============================================================
# 4. PFT及统一输出目录
# ============================================================

pipeline_out_dir <- file.path(
  output_root,
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

for (dir_i in c(
  pipeline_out_dir,
  prep_dir,
  ma_dir,
  qc_dir
)) {
  dir.create(
    dir_i,
    recursive = TRUE,
    showWarnings = FALSE
  )
}


# ============================================================
# 5. 生成并检查 unit map
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
# 6. 提取单PFT TRY数据并统一单位
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
# 7. 检查并修复错误的 Replicates
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
# 8. 保存无效Replicates审核表
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
# 9. 获取最终 trait mapping
# ============================================================

trait_map_for_ma <- attr(
  try_data,
  "trait_map_for_ma"
)

if (is.null(trait_map_for_ma)) {
  trait_map_for_ma <- trait_map
}


# ============================================================
# 10. TRY data → PEcAn MA long format
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
# 11. 检查 try_ma_long
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
# 12. 检查format之后的n
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
# 13. 保存准备阶段结果
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
# 14. 生成 trait.data
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
# 15. 生成 prior.distns
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
# 16. 并行 Meta-analysis
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
    
    iterations = ma_iterations,
    random = FALSE,
    threshold = 1.2,
    use_ghs = FALSE,
    gamma_tau = 0.01,
    
    workers = ma_workers,
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
# 17. MA QC
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

nonfail_traits <- unique(c(
  ma_qc$passed_traits,
  ma_qc$review_traits
))

if (length(nonfail_traits) == 0L) {
  warning(
    "MA 已完成，但没有 PASS 或 REVIEW trait：",
    pftname,
    call. = FALSE
  )
}


# ============================================================
# 18. 第一阶段完成
#
# bridge / writer 故意不在本脚本中执行，避免 bridge 错误被误认为
# MA 错误。确认 01_preparation、02_meta_analysis 和 03_ma_qc 后，
# 再从保存的 ma_result.rds 和 ma_qc_result.rds 开始下一阶段。
# ============================================================

cat(
  "\n", MA_PREP_VERSION, " completed successfully",
  "\nMA + QC finished for: ", pftname,
  "\nMA traits: ", length(ma_result$trait.mcmc),
  "\nQC PASS: ", length(ma_qc$passed_traits),
  "\nQC REVIEW: ", length(ma_qc$review_traits),
  "\nQC FAIL: ", length(ma_qc$failed_traits),
  "\nOutput: ", pipeline_out_dir,
  "\n",
  sep = ""
)