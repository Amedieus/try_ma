library(data.table)


##### Trait_map
trait_map <- c(
  # 1. DIRECT：可直接作为 PEcAn observed/target trait
  "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole excluded" =
    "SLA",
  "Leaf carbon (C) content per leaf dry mass" =
    "leafC",
  "Leaf carbon/nitrogen (C/N) ratio" =
    "c2n_leaf",
  "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole included" =
    "SLA",
  "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or excluded" =
    "SLA",
  "Leaf carbon (C) content per leaf area" =
    "leafC_area",
  "Photosynthesis: intercellular CO2 concentration to ambient CO2 (ci/ca)" =
    "ci_ca",
  "Fine root carbon/nitrogen (C/N) ratio" =
    "c2n_fineroot",
  "Fine root dry mass turnover rate" =
    "root_turnover_rate",
  
  # 2. INTERMEDIATE：Observed 中间性状 → bridge
  # 2.1 木密度和叶结构
   "Stem specific density (SSD, stem dry mass per stem fresh volume) or wood density" =
    "wood_density_md",
  "Leaf thickness" =
    "leaf_thickness_md",
  "Leaf nitrogen (N) content per leaf dry mass" =
    "leaf_N_mass_md",
  "Leaf phosphorus (P) content per leaf dry mass" =
    "leaf_P_mass_md",
  "Leaf dry mass per leaf fresh mass (leaf dry matter content, LDMC)" =
    "LDMC_md",
  "Leaf density (leaf tissue density, leaf dry mass per leaf volume)" =
    "leaf_tissue_density_md",
  
  "Leaf lignin content per leaf dry mass" =
    "leaf_lignin_md",
  
  "Leaf cellulose content per leaf dry mass" =
    "leaf_cellulose_md",
  
  "Leaf hemi-cellulose content per leaf dry mass" =
    "leaf_hemicellulose_md",
  
  "Leaf nitrogen/phosphorus (N/P) ratio" =
    "leaf_NP_ratio_md",
  
  "Leaf nitrogen (N) content per leaf area" =
    "leaf_N_area_md",
  
  "Leaf phosphorus (P) content per leaf area" =
    "leaf_P_area_md",
  
  "Leaf construction cost per leaf dry mass" =
    "leaf_construction_cost_mass_md",
  
  "Leaf construction cost per leaf area" =
    "leaf_construction_cost_area_md",

    # 2.2 光合作用、气体交换和叶呼吸
  "Photosynthesis rate per leaf area" =
    "photosynthesis_rate_area_observed_md",
  
  "Photosynthesis rate per leaf dry mass" =
    "photosynthesis_rate_mass_observed_md",
  
  "Photosynthesis: intercellular CO2 concentration" =
    "ci_md",
  
  "Leaf transpiration rate per leaf area" =
    "leaf_transpiration_area_md",
  
  "Leaf respiration rate in the dark per leaf area" =
    "leaf_dark_respiration_area_Tmeas_md",
  
  "Leaf respiration rate in the dark per leaf dry mass" =
    "leaf_dark_respiration_mass_Tmeas_md",
  
  "Photosynthesis carboxylation capacity (Vcmax) per leaf dry mass (Farquhar model)" =
    "Vcmax_mass_Tref_md",
  
  "Photosynthesis carboxylation capacity (Vcmax) per leaf area (Farquhar model)" =
    "Vcmax_area_Tref_md",
  
  "Photosynthesis light use efficiency (LUE)" =
    "LUE_md",
  
  "Photosynthesis rate per leaf nitrogen (N) content (photosynthetic nitrogen use efficiency, PNUE)" =
    "PNUE_md",
  
  "Photosynthesis electron transport capacity (Jmax) per leaf area (Farquhar model)" =
    "Jmax_area_Tref_md",
  
  "Leaf respiration rate in the dark temperature dependence" =
    "leaf_respiration_Q10_md",
  
  "Photosynthesis rate per leaf transpiration (photosynthetic water use effinciency: WUE)" =
    "WUE_md",
  
  "Photosynthesis electron transport capacity (Jmax) per leaf dry mass (Farquhar model)" =
    "Jmax_mass_Tref_md",
  
  "Leaf respiration rate in the dark as fraction of photosynthetic carboxylation capacity (Vcmax)" =
    "Rd_Vcmax_fraction_md",
  
  "Leaf mesophyll conductance" =
    "mesophyll_conductance_md",
  
  "Photosynthesis quantum yield (QY; corresponding to photosynthetic efficiency)" =
    "quantum_yield_md",
  
  # 2.3 根系结构、化学、呼吸和周转
  "Root tissue density (root dry mass per root volume)" =
    "root_tissue_density_md",
  
  "Root diameter" =
    "root_diameter_md",
  
  "Root nitrogen (N) content per root dry mass" =
    "root_N_mass_md",
  
  "Root carbon (C) content per root dry mass" =
    "root_C_md",
  
  "Root dry mass per root fresh mass (root dry matter content; RDMC)" =
    "root_RDMC_md",
  
  "Fine root carbon (C) content per fine root dry mass" =
    "fine_root_C_md",
  
  "Fine root nitrogen (N) content per fine root dry mass" =
    "fine_root_N_mass_md",
  
  "Root respiration rate per root dry mass" =
    "root_respiration_mass_Tmeas_generic_md",
  
  "Fine root length per fine root dry mass (specific fine root length, SRL)" =
    "fine_root_SRL_md",
  
  "Fine root dry mass per fine root fresh mass (fine root dry matter content; RDMC)" =
    "fine_root_RDMC_md",
  
  "Fine root phosphorus (P) content per fine root dry mass" =
    "fine_root_P_mass_md",
  
  "Root phosphorus (P) content per root dry mass" =
    "root_P_mass_md",
  
  "Root length per root dry mass (specific root length, SRL)" =
    "root_SRL_md",
  
  "Root carbon/nitrogen (C/N) ratio" =
    "c2n_root_generic_md",
  
  "Fine root diameter" =
    "fine_root_diameter_md",
  
  "Fine root tissue density (fine root dry mass per fine root volume)" =
    "fine_root_tissue_density_md",
  
  "Fine root respiration rate per fine root dry mass" =
    "fine_root_respiration_mass_Tmeas_md",
  
  "Root dry mass turnover rate" =
    "root_turnover_rate_generic_md",
  
  # 2.4 凋落物和粗木质残体分解
  "Litter decomposition rate" =
    "litter_decomposition_rate_observed_md",
  
  "Litter nitrogen (N) content per litter dry mass" =
    "litter_N_mass_md",
  
  "Litter carbon (C) content per litter dry mass" =
    "litter_C_mass_md",
  
  "Litter lignin content per litter dry mass" =
    "litter_lignin_md",
  
  "Litter tannin content per litter dry mass" =
    "litter_tannin_md",
  
  "Litter cellulose content per litter dry mass" =
    "litter_cellulose_md",
  
  "Litter phosphorus (P) content per litter dry mass" =
    "litter_P_mass_md",
  
  "Litter SLA (leaf area per leaf dry mass, specific leaf area, 1/LMA of leaf litter)" =
    "litter_SLA_md",
  
  "Litter carbon/nitrogen (C/N) ratio" =
    "litter_CN_md",
  
  "Litter decomposition rate constant" =
    "litter_decomposition_k_observed_md",
  
  "Coarse woody debris (CWD) stem: decomposition rate constant" =
    "cwd_decomposition_k_md",
  
  "Coarse woody debris (CWD) stem: decomposition rate" =
    "cwd_decomposition_rate_md",
  
  "Coarse woody debris (CWD) stem: nitrogen (N) content per stem CWD" =
    "cwd_N_mass_md",
  
  "Coarse woody debris (CWD) stem: carbon (C) content per stem CWD" =
    "cwd_C_mass_md",
  
  "Coarse woody debris (CWD) stem: carbon/nitrogen (C/N) ratio" =
    "cwd_CN_md",
  
  
  # ----------------------------------------------------------
  # 2.5 茎、木质组织和水力
  # ----------------------------------------------------------
  
  "Stem carbon/nitrogen (C/N) ratio" =
    "stem_CN_md",
  
  "Stem specific density (SSD, stem dry mass per stem fresh volume) or wood density: sapwood" =
    "sapwood_density_md",
  
  "Stem carbon (C) content per stem dry mass" =
    "stem_C_md",
  
  "Stem nitrogen (N) content per stem dry mass" =
    "stem_N_mass_md",
  
  "Stem specific density (SSD, stem dry mass per stem fresh volume) or wood density: branch" =
    "branch_density_md",
  
  "Bark thickness" =
    "bark_thickness_md",
  
  "Coarse root to fine root mass ratio" =
    "coarse_fine_root_mass_ratio_md",
  
  "Wood (sapwood) specific conductivity (stem specific conductivity)" =
    "sapwood_specific_conductivity_md",
  
  "Wood nitrogen (N) content per wood dry mass" =
    "wood_N_mass_md",
  
  "Stem dry mass per stem fresh mass (stem dry matter content, StDMC)" =
    "stem_DMC_md",
  
  # 2.6 叶寿命、根深和植物水力
  "Leaf lifespan (longevity)" =
    "leaf_lifespan_md",
  
  "Leaf water & osmotic potential: leaf osmotic potential at full turgor" =
    "osmotic_potential_full_turgor_md",
  
  "Leaf water & osmotic potential: leaf osmotic potential at turgor loss" =
    "osmotic_potential_tlp_md",
  
  "Leaf water storage capacity (WSC)" =
    "leaf_WSC_md",
  
  "Root rooting depth" =
    "rooting_depth_md",
  
  "Leaf hydraulic conductance" =
    "leaf_hydraulic_conductance_md",
  
  "Fine root rooting depth" =
    "fine_root_rooting_depth_md",
  
  "Leaf water potential midday" =
    "midday_leaf_water_potential_md",
  
  "Leaf water potential predawn" =
    "predawn_leaf_water_potential_md",
  
  "Leaf water & osmotic potential: leaf water potential at turgor loss point" =
    "leaf_turgor_loss_point_md"
)

##### Generate try_data
library(data.table)

prepare_single_pft_try_data <- function(
    trydat_use_species,
    pftname,
    trait_map,
    pft_species_map = NULL,
    
    registry =
      try_phase2_relationship_registry(),
    
    unit_col = "UnitName",
    
    out_dir = NULL,
    
    strict_units = TRUE,
    strict_ranges = FALSE,
    range_action = c(
      "drop",
      "stop",
      "keep"
    ),
    
    sipnet_days_per_year = 365,
    unit_overrides = NULL,
    
    drop_errorrisk = TRUE,
    
    species_ambiguity_action = c(
      "warn",
      "stop",
      "allow"
    ),
    
    save_ready_csv = FALSE
) {
  
  range_action <- match.arg(range_action)
  
  species_ambiguity_action <-
    match.arg(species_ambiguity_action)
  
  
  # ==========================================================
  # 0. 检查已有 unit-registry 函数
  # ==========================================================
  
  required_functions <- c(
    "try_phase2_relationship_registry",
    "validate_try_phase2_relationship_registry",
    "harmonize_try_phase2_observed_units"
  )
  
  missing_functions <- required_functions[
    !vapply(
      required_functions,
      exists,
      logical(1),
      mode = "function",
      inherits = TRUE
    )
  ]
  
  if (length(missing_functions) > 0L) {
    stop(
      paste0(
        "请先 source 包含 Phase 2 unit registry ",
        "和 converter 的脚本。缺少函数：",
        paste(
          missing_functions,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  
  # ==========================================================
  # 1. 检查并清理 trait_map
  # ==========================================================
  
  if (
    !is.character(trait_map) ||
    is.null(names(trait_map))
  ) {
    stop(
      "trait_map 必须是 named character vector，例如：\n",
      'c("TRY TraitName" = "PEcAn_vname")',
      call. = FALSE
    )
  }
  
  map_names <- trimws(
    as.character(names(trait_map))
  )
  
  map_values <- trimws(
    as.character(unname(trait_map))
  )
  
  valid_map <- (
    !is.na(map_names) &
      nzchar(map_names) &
      !is.na(map_values) &
      nzchar(map_values)
  )
  
  trait_map <- setNames(
    map_values[valid_map],
    map_names[valid_map]
  )
  
  if (length(trait_map) == 0L) {
    stop(
      "trait_map 中没有有效映射。",
      call. = FALSE
    )
  }
  
  if (anyDuplicated(names(trait_map))) {
    
    duplicated_traits <- unique(
      names(trait_map)[
        duplicated(names(trait_map))
      ]
    )
    
    stop(
      "以下 TRY TraitName 在 trait_map 中重复：",
      paste(
        duplicated_traits,
        collapse = "; "
      ),
      call. = FALSE
    )
  }
  
  
  # ==========================================================
  # 2. 为当前 trait_map 准备 canonical-unit registry
  # ==========================================================
  
  registry_dt <- copy(
    as.data.table(registry)
  )
  
  validate_try_phase2_relationship_registry(
    registry_dt
  )
  
  missing_registry_traits <- setdiff(
    names(trait_map),
    registry_dt$TraitName
  )
  
  if (length(missing_registry_traits) > 0L) {
    stop(
      "以下 trait_map 名称不在 canonical-unit registry 中：\n",
      paste(
        missing_registry_traits,
        collapse = "\n"
      ),
      call. = FALSE
    )
  }
  
  registry_use <- copy(
    registry_dt[
      TraitName %chin% names(trait_map)
    ]
  )
  
  # 当前版本只接受数值型 DIRECT 和 INTERMEDIATE
  nonnumeric_traits <- registry_use[
    value_kind != "NUMERIC",
    TraitName
  ]
  
  if (length(nonnumeric_traits) > 0L) {
    stop(
      paste0(
        "当前 trait_map 包含非数值性状，",
        "不能进入连续变量 MA：\n",
        paste(
          nonnumeric_traits,
          collapse = "\n"
        )
      ),
      call. = FALSE
    )
  }
  
  # 使用你自己定义的 DIRECT / *_md vname
  registry_use[
    ,
    pecan_observed_vname :=
      unname(
        trait_map[
          as.character(TraitName)
        ]
      )
  ]
  
  # 同一个输出 vname 必须只有一个 canonical unit
  registry_unit_check <- registry_use[
    ,
    .(
      n_canonical_units =
        uniqueN(canonical_unit),
      
      canonical_units =
        paste(
          sort(unique(canonical_unit)),
          collapse = " | "
        )
    ),
    by = pecan_observed_vname
  ]
  
  bad_registry_units <- registry_unit_check[
    n_canonical_units != 1L
  ]
  
  if (nrow(bad_registry_units) > 0L) {
    stop(
      "以下 vname 在 registry 中对应多个 canonical unit：",
      paste(
        bad_registry_units$pecan_observed_vname,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  
  # ==========================================================
  # 3. 读取和检查 PFT-species mapping
  # ==========================================================
  
  if (is.null(pft_species_map)) {
    
    if (
      !exists(
        "pftspecies",
        envir = .GlobalEnv,
        inherits = FALSE
      )
    ) {
      stop(
        "没有提供 pft_species_map，",
        "全局环境中也没有 pftspecies。",
        call. = FALSE
      )
    }
    
    pft_species_map <- get(
      "pftspecies",
      envir = .GlobalEnv
    )
  }
  
  pft_dt <- copy(
    as.data.table(pft_species_map)
  )
  
  required_pft_columns <- c(
    "final_pft",
    "try_species_id"
  )
  
  missing_pft_columns <- setdiff(
    required_pft_columns,
    names(pft_dt)
  )
  
  if (length(missing_pft_columns) > 0L) {
    stop(
      "PFT-species mapping 缺少列：",
      paste(
        missing_pft_columns,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  
  # ==========================================================
  # 4. 处理 include 标记
  # ==========================================================
  
  if ("include" %in% names(pft_dt)) {
    
    if (is.logical(pft_dt$include)) {
      
      pft_dt[
        ,
        include_internal :=
          fifelse(
            is.na(include),
            FALSE,
            include
          )
      ]
      
    } else {
      
      pft_dt[
        ,
        include_internal :=
          tolower(
            trimws(
              as.character(include)
            )
          ) %chin%
          c(
            "true",
            "t",
            "1",
            "yes",
            "y"
          )
      ]
    }
    
  } else {
    
    pft_dt[
      ,
      include_internal := TRUE
    ]
  }
  
  pft_use <- pft_dt[
    include_internal == TRUE &
      !is.na(try_species_id)
  ]
  
  
  # ==========================================================
  # 5. 检查同一 species 是否进入多个 PFT
  # ==========================================================
  
  species_ambiguity <- pft_use[
    ,
    .(
      n_pfts =
        uniqueN(final_pft),
      
      pfts =
        paste(
          sort(unique(final_pft)),
          collapse = ";"
        )
    ),
    by = .(
      try_species_id =
        as.character(try_species_id)
    )
  ][
    n_pfts > 1L
  ]
  
  target_species_ids <- unique(
    as.character(
      pft_use[
        as.character(final_pft) ==
          as.character(pftname),
        try_species_id
      ]
    )
  )
  
  target_species_ids <- target_species_ids[
    !is.na(target_species_ids) &
      nzchar(target_species_ids)
  ]
  
  if (length(target_species_ids) == 0L) {
    stop(
      "没有找到目标 PFT 的 included species：",
      pftname,
      call. = FALSE
    )
  }
  
  target_species_ambiguity <- species_ambiguity[
    try_species_id %chin%
      target_species_ids
  ]
  
  if (nrow(target_species_ambiguity) > 0L) {
    
    ambiguity_message <- paste0(
      "目标 PFT 有 ",
      nrow(target_species_ambiguity),
      " 个 species 同时属于多个 PFT。 ",
      "正式分析建议改用 ObservationID-level PFT assignment。"
    )
    
    if (species_ambiguity_action == "stop") {
      stop(
        ambiguity_message,
        call. = FALSE
      )
    }
    
    if (species_ambiguity_action == "warn") {
      warning(
        ambiguity_message,
        call. = FALSE
      )
    }
  }
  
  
  # ==========================================================
  # 6. 复制并检查 TRY 数据
  # ==========================================================
  
  try_dt <- copy(
    as.data.table(trydat_use_species)
  )
  
  required_try_columns <- c(
    "AccSpeciesID",
    "TraitName",
    "StdValue",
    unit_col
  )
  
  missing_try_columns <- setdiff(
    required_try_columns,
    names(try_dt)
  )
  
  if (length(missing_try_columns) > 0L) {
    stop(
      "trydat_use_species 缺少列：",
      paste(
        missing_try_columns,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  # 将用户指定单位列统一复制为 UnitName
  if (unit_col != "UnitName") {
    
    try_dt[
      ,
      UnitName :=
        as.character(
          get(unit_col)
        )
    ]
    
  } else {
    
    try_dt[
      ,
      UnitName :=
        as.character(UnitName)
    ]
  }
  
  n_input_rows <- nrow(try_dt)
  
  
  # ==========================================================
  # 7. 按 PFT species 和 trait_map 筛选
  # ==========================================================
  
  try_data <- try_dt[
    as.character(AccSpeciesID) %chin%
      target_species_ids &
      as.character(TraitName) %chin%
      names(trait_map)
  ]
  
  n_rows_before_basic_filter <- nrow(
    try_data
  )
  
  if (n_rows_before_basic_filter == 0L) {
    stop(
      "目标 PFT 中没有匹配 trait_map 的 TRY records。",
      call. = FALSE
    )
  }
  
  
  # ==========================================================
  # 8. 保存原始值和单位
  # ==========================================================
  
  try_data[
    ,
    `:=`(
      StdValue_original_prepare =
        StdValue,
      
      UnitName_original_prepare =
        as.character(UnitName)
    )
  ]
  
  
  # ==========================================================
  # 9. 删除缺失单位和非有限数值
  # ==========================================================
  
  unit_text <- trimws(
    as.character(try_data$UnitName)
  )
  
  missing_unit <- (
    is.na(unit_text) |
      !nzchar(unit_text) |
      tolower(unit_text) %chin%
      c(
        "na",
        "n/a",
        "n.a.",
        "nan",
        "null",
        "none",
        "unknown",
        "not available",
        "not applicable",
        "-9999",
        "?"
      )
  )
  
  numeric_value <- suppressWarnings(
    as.numeric(
      as.character(
        try_data$StdValue
      )
    )
  )
  
  nonfinite_value <- !is.finite(
    numeric_value
  )
  
  missing_unit_records <- copy(
    try_data[
      missing_unit
    ]
  )
  
  nonfinite_value_records <- copy(
    try_data[
      !missing_unit &
        nonfinite_value
    ]
  )
  
  n_dropped_missing_unit <- sum(
    missing_unit
  )
  
  n_dropped_nonfinite <- sum(
    !missing_unit &
      nonfinite_value
  )
  
  keep_basic <- (
    !missing_unit &
      !nonfinite_value
  )
  
  try_data <- try_data[
    keep_basic
  ]
  
  try_data[
    ,
    `:=`(
      UnitName =
        unit_text[keep_basic],
      
      StdValue =
        numeric_value[keep_basic]
    )
  ]
  
  if (nrow(try_data) == 0L) {
    stop(
      "删除缺失单位和非有限值后没有剩余记录。",
      call. = FALSE
    )
  }
  
  
  # ==========================================================
  # 10. 防止 ErrorRisk 被 formatter 误认为 SE
  # ==========================================================
  
  if (
    isTRUE(drop_errorrisk) &&
    "ErrorRisk" %in% names(try_data)
  ) {
    
    try_data[
      ,
      ErrorRisk_original_prepare :=
        ErrorRisk
    ]
    
    try_data[
      ,
      ErrorRisk := NULL
    ]
  }
  
  try_data[
    ,
    final_pft :=
      as.character(pftname)
  ]
  
  
  # ==========================================================
  # 11. 转换到 canonical units
  # ==========================================================
  
  if (!is.null(out_dir)) {
    dir.create(
      out_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )
  }
  
  harmonized <-
    harmonize_try_phase2_observed_units(
      try_data = try_data,
      
      registry = registry_use,
      
      out_dir = out_dir,
      
      strict_units =
        strict_units,
      
      strict_ranges =
        strict_ranges,
      
      range_action =
        range_action,
      
      sipnet_days_per_year =
        sipnet_days_per_year,
      
      unit_overrides =
        unit_overrides,
      
      save_harmonized_csv =
        FALSE
    )
  
  try_data_ready <- copy(
    harmonized$data
  )
  
  
  # ==========================================================
  # 12. 恢复用户定义的 DIRECT / *_md vname
  # ==========================================================
  
  if (
    "TraitName_original_phase2" %in%
    names(try_data_ready)
  ) {
    
    original_trait_name <-
      as.character(
        try_data_ready$TraitName_original_phase2
      )
    
  } else {
    
    original_trait_name <-
      as.character(
        try_data_ready$TraitName
      )
  }
  
  requested_vname <- unname(
    trait_map[
      original_trait_name
    ]
  )
  
  if (any(is.na(requested_vname))) {
    stop(
      "单位处理后有 TraitName 无法回接到 trait_map。",
      call. = FALSE
    )
  }
  
  final_vname <- requested_vname
  
  # unit harmonizer 会把根呼吸的 CO2、O2 和未知气体拆开。
  # 对这些 synthetic TraitName 继续保留 _md 后缀。
  synthetic_trait <- (
    as.character(
      try_data_ready$TraitName
    ) != original_trait_name
  )
  
  add_md_suffix <- (
    synthetic_trait &
      endsWith(
        requested_vname,
        "_md"
      ) &
      !endsWith(
        as.character(
          try_data_ready$pecan_observed_vname
        ),
        "_md"
      )
  )
  
  final_vname[add_md_suffix] <- paste0(
    try_data_ready$pecan_observed_vname[
      add_md_suffix
    ],
    "_md"
  )
  
  try_data_ready[
    ,
    `:=`(
      vname =
        final_vname,
      
      pecan_observed_vname =
        final_vname,
      
      StdValue =
        canonical_value,
      
      UnitName =
        canonical_unit,
      
      final_pft =
        as.character(pftname)
    )
  ]
  
  
  # ==========================================================
  # 13. 验证 canonical values 和 units
  # ==========================================================
  
  if (
    any(
      !is.finite(
        try_data_ready$StdValue
      )
    )
  ) {
    stop(
      "canonical StdValue 中仍然存在非有限数值。",
      call. = FALSE
    )
  }
  
  vname_unit_check <- try_data_ready[
    ,
    .(
      n_canonical_units =
        uniqueN(canonical_unit),
      
      canonical_units =
        paste(
          sort(
            unique(canonical_unit)
          ),
          collapse = " | "
        )
    ),
    by = vname
  ]
  
  bad_vname_units <- vname_unit_check[
    n_canonical_units != 1L
  ]
  
  if (nrow(bad_vname_units) > 0L) {
    stop(
      "单位处理后仍有 vname 对应多个 canonical unit：",
      paste(
        bad_vname_units$vname,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  
  # ==========================================================
  # 14. 为 formatter 生成唯一 id
  # ==========================================================
  
  if (
    "ObsDataID" %in%
    names(try_data_ready)
  ) {
    try_data_ready[
      ,
      TRY_ObsDataID_original_prepare :=
        ObsDataID
    ]
  }
  
  try_data_ready[
    ,
    ObsDataID := .I
  ]
  
  
  # ==========================================================
  # 15. 生成真正传给 format_try_for_ma() 的 trait_map
  # ==========================================================
  
  format_map_table <- unique(
    try_data_ready[
      ,
      .(
        TraitName =
          as.character(TraitName),
        
        vname =
          as.character(vname)
      )
    ]
  )
  
  bad_format_map <- format_map_table[
    ,
    .(
      n_vnames =
        uniqueN(vname)
    ),
    by = TraitName
  ][
    n_vnames != 1L
  ]
  
  if (nrow(bad_format_map) > 0L) {
    stop(
      "formatter TraitName 对应多个 vname：",
      paste(
        bad_format_map$TraitName,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  format_trait_map <- setNames(
    format_map_table$vname,
    format_map_table$TraitName
  )
  
  
  # ==========================================================
  # 16. 生成审核结果
  # ==========================================================
  
  preparation_summary <- data.table(
    final_pft =
      as.character(pftname),
    
    n_species_in_mapping =
      length(target_species_ids),
    
    n_rows_before_basic_filter =
      n_rows_before_basic_filter,
    
    n_dropped_missing_unit =
      n_dropped_missing_unit,
    
    n_dropped_nonfinite_value =
      n_dropped_nonfinite,
    
    n_unsupported_unit_records =
      nrow(
        harmonized$unit_failures
      ),
    
    n_range_check_records =
      nrow(
        harmonized$range_failures
      ),
    
    n_final_rows =
      nrow(try_data_ready),
    
    n_final_species =
      uniqueN(
        try_data_ready$AccSpeciesID
      ),
    
    n_final_try_traits =
      uniqueN(
        try_data_ready$TraitName
      ),
    
    n_final_vnames =
      uniqueN(
        try_data_ready$vname
      ),
    
    n_direct_vnames =
      uniqueN(
        try_data_ready[
          !endsWith(vname, "_md"),
          vname
        ]
      ),
    
    n_intermediate_vnames =
      uniqueN(
        try_data_ready[
          endsWith(vname, "_md"),
          vname
        ]
      )
  )
  
  unit_audit <- try_data_ready[
    ,
    .(
      n_rows = .N,
      
      n_species =
        uniqueN(AccSpeciesID),
      
      canonical_min =
        min(
          StdValue,
          na.rm = TRUE
        ),
      
      canonical_median =
        median(
          StdValue,
          na.rm = TRUE
        ),
      
      canonical_max =
        max(
          StdValue,
          na.rm = TRUE
        )
    ),
    by = .(
      vname,
      TraitName,
      UnitName_original,
      canonical_unit,
      conversion_rule,
      unit_status,
      range_status
    )
  ][
    order(
      vname,
      TraitName,
      UnitName_original
    )
  ]
  
  
  # ==========================================================
  # 17. 保存审核结果
  # ==========================================================
  
  if (!is.null(out_dir)) {
    
    fwrite(
      preparation_summary,
      file.path(
        out_dir,
        "single_pft_try_preparation_summary.csv"
      )
    )
    
    fwrite(
      unit_audit,
      file.path(
        out_dir,
        "single_pft_try_unit_audit.csv"
      )
    )
    
    fwrite(
      missing_unit_records,
      file.path(
        out_dir,
        "single_pft_try_missing_units.csv"
      )
    )
    
    fwrite(
      nonfinite_value_records,
      file.path(
        out_dir,
        "single_pft_try_nonfinite_values.csv"
      )
    )
    
    if (
      nrow(species_ambiguity) > 0L
    ) {
      fwrite(
        species_ambiguity,
        file.path(
          out_dir,
          "single_pft_species_ambiguity.csv"
        )
      )
    }
    
    if (isTRUE(save_ready_csv)) {
      fwrite(
        try_data_ready,
        file.path(
          out_dir,
          "single_pft_try_data_ready.csv"
        )
      )
    }
  }
  
  
  # ==========================================================
  # 18. 返回结果
  # ==========================================================
  
  message(
    "Finished preparing canonical TRY data for: ",
    pftname,
    
    "\nFinal rows: ",
    format(
      nrow(try_data_ready),
      big.mark = ","
    ),
    
    "\nSpecies: ",
    uniqueN(
      try_data_ready$AccSpeciesID
    ),
    
    "\nDIRECT vnames: ",
    preparation_summary$n_direct_vnames,
    
    "\nINTERMEDIATE vnames: ",
    preparation_summary$n_intermediate_vnames
  )
  
  list(
    data =
      try_data_ready[],
    
    # 这个才传给 format_try_for_ma()
    trait_map =
      format_trait_map,
    
    # 你原来定义的93条 mapping
    requested_trait_map =
      trait_map,
    
    registry =
      registry_use,
    
    preparation_summary =
      preparation_summary,
    
    unit_audit =
      unit_audit,
    
    missing_unit_records =
      missing_unit_records,
    
    nonfinite_value_records =
      nonfinite_value_records,
    
    species_ambiguity =
      species_ambiguity,
    
    harmonization =
      harmonized
  )
}
