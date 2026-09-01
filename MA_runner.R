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
source("/projectnb/dietzelab/guYANG/TRY_meta_analysis/pecan_to_sipnet.R")
source("/projectnb/dietzelab/guYANG/TRY_meta_analysis/md_pecan_bridge.R")
source("/projectnb/dietzelab/guYANG/TRY_meta_analysis/run_ma_parallel.R")
source("/projectnb/dietzelab/guYANG/TRY_meta_analysis/generate_prior_distns.R")
source("/projectnb/dietzelab/guYANG/TRY_meta_analysis/ma_functions.R")
source("/projectnb/dietzelab/guYANG/TRY_meta_analysis/pecan_traits.R")
source("/projectnb/dietzelab/guYANG/SIPNET_Model_Calibration/functions_for_try_data.R")

library(data.table)

# ============================================================
# 0. 总输出根目录
# ============================================================

ma_root_parent <- file.path(
  "/projectnb/dietzelab/guYANG",
  "TRY_meta_analysis"
)

dir.create(
  ma_root_parent,
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# 1. 从 pftspecies 提取需要运行的全部 PFT
# ============================================================

pft_dt <- copy(
  as.data.table(pftspecies)
)

if (!"final_pft" %in% names(pft_dt)) {
  stop("pftspecies 缺少 final_pft 列。")
}

if ("include" %in% names(pft_dt)) {
  
  pft_dt[
    ,
    include_use :=
      tolower(trimws(as.character(include))) %in%
      c("true", "t", "1", "yes")
  ]
  
} else {
  
  pft_dt[
    ,
    include_use := TRUE
  ]
}

pftnames <- sort(
  unique(
    as.character(
      pft_dt[
        include_use == TRUE &
          !is.na(final_pft) &
          nzchar(trimws(as.character(final_pft))),
        final_pft
      ]
    )
  )
)

cat(
  "PFTs to run:",
  length(pftnames),
  "\n"
)

# ============================================================
# 2. Trait unit map 只需要生成和验证一次
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
# 3. 准备循环状态表
# ============================================================

pipeline_status_list <- vector(
  mode = "list",
  length = length(pftnames)
)

names(pipeline_status_list) <- pftnames


# ============================================================
# 4. 按 PFT 顺序循环
#
# PFT之间顺序运行；
# 每个PFT内部使用24个workers并行trait MA。
# ============================================================

for (pft_i in pftnames) {
  
  message(
    "\n",
    paste(rep("=", 70), collapse = ""),
    "\nSTARTING PFT: ",
    pft_i,
    "\n",
    paste(rep("=", 70), collapse = "")
  )
  
  pft_start_time <- Sys.time()
  
  pft_dir <- file.path(
    ma_root_parent,
    pft_i
  )
  
  dir.create(
    pft_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  pipeline_status_list[[pft_i]] <- tryCatch({
    
    # ========================================================
    # A. TRY → try_ma_long → trait.data → prior.distns
    #    → parallel MA
    # ========================================================
    
    ma_result <-
      run_single_pft_try_ma_pipeline_parallel(
        pftname = pft_i,
        out_dir = pft_dir,
        
        # MA 设置
        iterations = 3000L,
        random = FALSE,
        threshold = 1.2,
        use_ghs = FALSE,
        gamma_tau = 0.01,
        
        # 每个PFT内部并行
        workers = 24L,
        
        # Prior 设置
        sample_fraction = 0.05,
        min_sample_n = 1L,
        max_sample_n = 2L,
        width_multiplier = 10,
        relative_sd_floor = 0.10,
        seed = 20260827L,
        
        # TRY 数据设置
        unit_col = "UnitName",
        value_col = "StdValue",
        drop_errorrisk = TRUE,
        unsupported_action = "stop",
        ambiguous_species_action = "warn",
        
        # 继续已经成功完成的trait
        resume = TRUE,
        save_intermediate = TRUE,
        
        # 显式输入数据，不依赖 GlobalEnv 自动查找
        trydat_use_species =
          trydat_use_species,
        
        trait_map =
          trait_map,
        
        pft_species_map =
          pftspecies,
        
        unit_map =
          unit_map
      )
    
    ma_dir <- file.path(
      pft_dir,
      "02_meta_analysis"
    )
    
    dir.create(
      ma_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )
    
    saveRDS(
      ma_result,
      file.path(
        ma_dir,
        "ma_result.rds"
      ),
      compress = FALSE
    )
    
    
    # ========================================================
    # B. 读取该PFT的 trait.data
    # ========================================================
    
    trait_file <- file.path(
      pft_dir,
      "01_preparation",
      "trait.data.Rdata"
    )
    
    if (!file.exists(trait_file)) {
      stop(
        "没有找到 trait.data：",
        trait_file
      )
    }
    
    trait_env <- new.env()
    
    load(
      trait_file,
      envir = trait_env
    )
    
    if (!exists(
      "trait.data",
      envir = trait_env,
      inherits = FALSE
    )) {
      stop(
        "trait.data.Rdata 中没有 trait.data 对象。"
      )
    }
    
    trait.data_i <- get(
      "trait.data",
      envir = trait_env,
      inherits = FALSE
    )
    
    
    # ========================================================
    # C. MA QC
    # ========================================================
    
    qc_dir <- file.path(
      pft_dir,
      "03_ma_qc"
    )
    
    ma_qc <-
      qc_pecan_ma_result(
        ma_result = ma_result,
        trait.data = trait.data_i,
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
        "该PFT没有任何通过QC的MA trait：",
        pft_i
      )
    }
    
    
    # ========================================================
    # D. PASS + REVIEW 保守 bridge
    #
    # 这是审核结果，不作为最终 all-enabled writer 输入。
    # 即使这个审计 bridge 失败，也继续运行最终 PASS bridge。
    # ========================================================
    
    bridge_out_dir <- file.path(
      qc_dir,
      "nonfail_bridge_PASS_REVIEW"
    )
    
    bridge_context <-
      default_md_bridge_context()
    
    bridge_attempt <- tryCatch({
      
      bridge_result <-
        bridge_ma_qc_to_pecan(
          ma_result = ma_result,
          ma_qc = ma_qc,
          out_dir = bridge_out_dir,
          
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
      
      list(
        status = "SUCCESS",
        result = bridge_result,
        error = NA_character_
      )
      
    }, error = function(e) {
      
      warning(
        "Non-FAIL audit bridge failed for ",
        pft_i,
        ": ",
        conditionMessage(e),
        call. = FALSE
      )
      
      list(
        status = "FAILED",
        result = NULL,
        error = conditionMessage(e)
      )
    })
    
    
    # ========================================================
    # E. PASS-only、全部开启的 writer-ready PEcAn traits
    #
    # 这里使用你刚修改的函数：
    # ma_root 直接指向 PFT 文件夹。
    # ========================================================
    
    writer_res_all <-
      run_pass_ma_to_writer_ready_pecan_all_enabled(
        pftname = pft_i,
        
        ma_root = pft_dir,
        
        bridge_file = file.path(
          script_dir,
          "md_to_pecan_bridge_v1.R"
        ),
        
        qc_subdir = "03_ma_qc",
        
        out_subdir =
          "04_writer_ready_pecan_all_enabled",
        
        n_draws = 6000L,
        seed = 20260828L,
        
        context_overrides = list(),
        model_overrides = list(),
        
        save_ma_draws_csv = FALSE
      )
    
    
    # ========================================================
    # F. 保存该PFT最终 writer_res_all
    # ========================================================
    
    writer_rds <- file.path(
      writer_res_all$out_dir,
      "writer_res_all.rds"
    )
    
    saveRDS(
      writer_res_all,
      writer_rds,
      compress = FALSE
    )
    
    pft_end_time <- Sys.time()
    
    data.table(
      pftname = pft_i,
      status = "SUCCESS",
      
      n_ma_traits =
        length(ma_result$trait.mcmc),
      
      n_qc_pass =
        length(ma_qc$passed_traits),
      
      n_qc_review =
        length(ma_qc$review_traits),
      
      n_qc_fail =
        length(ma_qc$failed_traits),
      
      nonfail_bridge_status =
        bridge_attempt$status,
      
      nonfail_bridge_error =
        bridge_attempt$error,
      
      n_writer_traits =
        length(
          writer_res_all$complete_writer_traits
        ),
      
      n_writer_draws =
        nrow(
          writer_res_all$writer_trait_draws
        ),
      
      elapsed_minutes =
        as.numeric(
          difftime(
            pft_end_time,
            pft_start_time,
            units = "mins"
          )
        ),
      
      pft_dir = pft_dir,
      writer_result_file = writer_rds,
      error_message = NA_character_
    )
    
  }, error = function(e) {
    
    error_message <- conditionMessage(e)
    
    warning(
      "PFT FAILED: ",
      pft_i,
      "\n",
      error_message,
      call. = FALSE
    )
    
    writeLines(
      error_message,
      con = file.path(
        pft_dir,
        "pipeline_error.txt"
      )
    )
    
    pft_end_time <- Sys.time()
    
    data.table(
      pftname = pft_i,
      status = "FAILED",
      
      n_ma_traits = NA_integer_,
      n_qc_pass = NA_integer_,
      n_qc_review = NA_integer_,
      n_qc_fail = NA_integer_,
      
      nonfail_bridge_status = NA_character_,
      nonfail_bridge_error = NA_character_,
      
      n_writer_traits = NA_integer_,
      n_writer_draws = NA_integer_,
      
      elapsed_minutes =
        as.numeric(
          difftime(
            pft_end_time,
            pft_start_time,
            units = "mins"
          )
        ),
      
      pft_dir = pft_dir,
      writer_result_file = NA_character_,
      error_message = error_message
    )
  })
  
  
  # 每个PFT完成后清理大对象，防止内存持续累积
  objects_to_remove <- intersect(
    c(
      "ma_result",
      "ma_qc",
      "trait.data_i",
      "trait_env",
      "bridge_result",
      "bridge_attempt",
      "writer_res_all"
    ),
    ls(
      envir = .GlobalEnv,
      all.names = TRUE
    )
  )
  
  if (length(objects_to_remove) > 0L) {
    rm(
      list = objects_to_remove,
      envir = .GlobalEnv
    )
  }
  
  invisible(gc())
}

# job_lines <- c(
#   "#!/bin/bash",
#   "module load R/4.4.0",
#   "Rscript /projectnb/dietzelab/guYANG/TRY_meta_analysis/MA_runner.R"
# )
# writeLines(job_lines, "/projectnb/dietzelab/guYANG/TRY_meta_analysis/MA_runner.sh")

# qsub -l h_rt=8:00:00 \
# -l buyin \
# -l mem_per_core=10G \
# -pe omp 28 \
# -V \
# -N MA_runner \
# -o /projectnb/dietzelab/guYANG/TRY_meta_analysis/MA_runner.out \
# -e /projectnb/dietzelab/guYANG/TRY_meta_analysis/MA_runner.err \
# -M yanggu@bu.edu \
# -m abe \
# -S /bin/bash \
# /projectnb/dietzelab/guYANG/TRY_meta_analysis/MA_runner.sh


