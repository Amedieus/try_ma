# ============================================================================
# Unit map for the 93 numeric TRY traits in the user-supplied trait_map
#
# This file only defines target units and dimensional-conversion families.
# It does not define ecological bridges, SIPNET targets, or outlier thresholds.
# ============================================================================

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("需要先安装 data.table。", call. = FALSE)
}


try_canonical_unit_lookup <- c(
  "Stem specific density (SSD, stem dry mass per stem fresh volume) or wood density" =
    "kg dry mass m-3 fresh volume",
  "Leaf thickness" =
    "mm",
  "Leaf nitrogen (N) content per leaf dry mass" =
    "g N kg-1 leaf dry mass",
  "Leaf phosphorus (P) content per leaf dry mass" =
    "g P kg-1 leaf dry mass",
  "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole excluded" =
    "m2 leaf kg-1 dry mass",
  "Leaf dry mass per leaf fresh mass (leaf dry matter content, LDMC)" =
    "g dry mass g-1 fresh mass",
  "Leaf density (leaf tissue density, leaf dry mass per leaf volume)" =
    "kg dry mass m-3 leaf volume",
  "Leaf lignin content per leaf dry mass" =
    "percent leaf dry mass",
  "Leaf cellulose content per leaf dry mass" =
    "percent leaf dry mass",
  "Leaf hemi-cellulose content per leaf dry mass" =
    "percent leaf dry mass",
  "Leaf carbon (C) content per leaf dry mass" =
    "percent leaf dry mass",
  "Leaf nitrogen/phosphorus (N/P) ratio" =
    "g N g-1 P",
  "Leaf nitrogen (N) content per leaf area" =
    "g N m-2 leaf",
  "Leaf carbon/nitrogen (C/N) ratio" =
    "g C g-1 N",
  "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole included" =
    "m2 leaf kg-1 dry mass",
  "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or excluded" =
    "m2 leaf kg-1 dry mass",
  "Leaf carbon (C) content per leaf area" =
    "g C m-2 leaf",
  "Leaf phosphorus (P) content per leaf area" =
    "g P m-2 leaf",
  "Leaf construction cost per leaf dry mass" =
    "g glucose g-1 leaf dry mass",
  "Leaf construction cost per leaf area" =
    "g glucose m-2 leaf",
  "Photosynthesis rate per leaf area" =
    "umol CO2 m-2 leaf s-1",
  "Photosynthesis rate per leaf dry mass" =
    "nmol CO2 g-1 leaf dry mass s-1",
  "Photosynthesis: intercellular CO2 concentration" =
    "umol CO2 mol-1 air",
  "Leaf transpiration rate per leaf area" =
    "mmol H2O m-2 leaf s-1",
  "Leaf respiration rate in the dark per leaf area" =
    "umol CO2 m-2 leaf s-1",
  "Leaf respiration rate in the dark per leaf dry mass" =
    "nmol CO2 g-1 leaf dry mass s-1",
  "Photosynthesis carboxylation capacity (Vcmax) per leaf dry mass (Farquhar model)" =
    "nmol CO2 g-1 leaf dry mass s-1",
  "Photosynthesis carboxylation capacity (Vcmax) per leaf area (Farquhar model)" =
    "umol CO2 m-2 leaf s-1",
  "Photosynthesis light use efficiency (LUE)" =
    "mol C mol-1 photons",
  "Photosynthesis rate per leaf nitrogen (N) content (photosynthetic nitrogen use efficiency, PNUE)" =
    "umol CO2 g-1 N s-1",
  "Photosynthesis electron transport capacity (Jmax) per leaf area (Farquhar model)" =
    "umol electrons m-2 leaf s-1",
  "Leaf respiration rate in the dark temperature dependence" =
    "unitless Q10",
  "Photosynthesis rate per leaf transpiration (photosynthetic water use effinciency: WUE)" =
    "umol CO2 mmol-1 H2O",
  "Photosynthesis electron transport capacity (Jmax) per leaf dry mass (Farquhar model)" =
    "nmol electrons g-1 leaf dry mass s-1",
  "Leaf respiration rate in the dark as fraction of photosynthetic carboxylation capacity (Vcmax)" =
    "fraction",
  "Photosynthesis: intercellular CO2 concentration to ambient CO2 (ci/ca)" =
    "fraction",
  "Leaf mesophyll conductance" =
    "mol CO2 m-2 leaf s-1 TRY-reported-gm-convention",
  "Photosynthesis quantum yield (QY; corresponding to photosynthetic efficiency)" =
    "mol CO2 mol-1 photons",
  "Root tissue density (root dry mass per root volume)" =
    "g dry mass cm-3 root volume",
  "Root diameter" =
    "mm",
  "Root nitrogen (N) content per root dry mass" =
    "g N kg-1 root dry mass",
  "Root carbon (C) content per root dry mass" =
    "percent root dry mass",
  "Root dry mass per root fresh mass (root dry matter content; RDMC)" =
    "g dry mass g-1 fresh mass",
  "Fine root carbon/nitrogen (C/N) ratio" =
    "g C g-1 N",
  "Fine root carbon (C) content per fine root dry mass" =
    "percent fine-root dry mass",
  "Fine root dry mass turnover rate" =
    "year-1",
  "Fine root nitrogen (N) content per fine root dry mass" =
    "g N kg-1 fine-root dry mass",
  "Root respiration rate per root dry mass" =
    "nmol respiratory gas g-1 root dry mass s-1",
  "Fine root length per fine root dry mass (specific fine root length, SRL)" =
    "m root g-1 dry mass",
  "Fine root dry mass per fine root fresh mass (fine root dry matter content; RDMC)" =
    "g dry mass g-1 fresh mass",
  "Fine root phosphorus (P) content per fine root dry mass" =
    "g P kg-1 fine-root dry mass",
  "Root phosphorus (P) content per root dry mass" =
    "g P kg-1 root dry mass",
  "Root length per root dry mass (specific root length, SRL)" =
    "m root g-1 dry mass",
  "Root carbon/nitrogen (C/N) ratio" =
    "g C g-1 N",
  "Fine root diameter" =
    "mm",
  "Fine root tissue density (fine root dry mass per fine root volume)" =
    "g dry mass cm-3 fine-root volume",
  "Fine root respiration rate per fine root dry mass" =
    "nmol respiratory gas g-1 fine-root dry mass s-1",
  "Root dry mass turnover rate" =
    "year-1",
  "Litter decomposition rate" =
    "year-1",
  "Litter nitrogen (N) content per litter dry mass" =
    "g N kg-1 litter dry mass",
  "Litter carbon (C) content per litter dry mass" =
    "percent litter dry mass",
  "Litter lignin content per litter dry mass" =
    "percent litter dry mass",
  "Litter tannin content per litter dry mass" =
    "percent litter dry mass",
  "Litter cellulose content per litter dry mass" =
    "percent litter dry mass",
  "Litter phosphorus (P) content per litter dry mass" =
    "g P kg-1 litter dry mass",
  "Litter SLA (leaf area per leaf dry mass, specific leaf area, 1/LMA of leaf litter)" =
    "m2 litter kg-1 dry mass",
  "Litter carbon/nitrogen (C/N) ratio" =
    "g C g-1 N",
  "Litter decomposition rate constant" =
    "year-1",
  "Coarse woody debris (CWD) stem: decomposition rate constant" =
    "year-1",
  "Coarse woody debris (CWD) stem: decomposition rate" =
    "year-1",
  "Coarse woody debris (CWD) stem: nitrogen (N) content per stem CWD" =
    "g N kg-1 CWD dry mass",
  "Coarse woody debris (CWD) stem: carbon (C) content per stem CWD" =
    "percent CWD dry mass",
  "Coarse woody debris (CWD) stem: carbon/nitrogen (C/N) ratio" =
    "g C g-1 N",
  "Stem carbon/nitrogen (C/N) ratio" =
    "g C g-1 N",
  "Stem specific density (SSD, stem dry mass per stem fresh volume) or wood density: sapwood" =
    "kg dry mass m-3 sapwood",
  "Stem carbon (C) content per stem dry mass" =
    "percent stem dry mass",
  "Stem nitrogen (N) content per stem dry mass" =
    "g N kg-1 stem dry mass",
  "Stem specific density (SSD, stem dry mass per stem fresh volume) or wood density: branch" =
    "kg dry mass m-3 branch volume",
  "Bark thickness" =
    "mm",
  "Coarse root to fine root mass ratio" =
    "ratio dry mass",
  "Wood (sapwood) specific conductivity (stem specific conductivity)" =
    "kg H2O m-1 s-1 MPa-1",
  "Wood nitrogen (N) content per wood dry mass" =
    "g N kg-1 wood dry mass",
  "Stem dry mass per stem fresh mass (stem dry matter content, StDMC)" =
    "g dry mass g-1 fresh mass",
  "Leaf lifespan (longevity)" =
    "year",
  "Leaf water & osmotic potential: leaf osmotic potential at full turgor" =
    "MPa",
  "Leaf water & osmotic potential: leaf osmotic potential at turgor loss" =
    "MPa",
  "Leaf water storage capacity (WSC)" =
    "kg H2O m-2 leaf MPa-1",
  "Root rooting depth" =
    "m",
  "Leaf hydraulic conductance" =
    "mmol H2O m-2 leaf s-1 MPa-1",
  "Fine root rooting depth" =
    "m",
  "Leaf water potential midday" =
    "MPa",
  "Leaf water potential predawn" =
    "MPa",
  "Leaf water & osmotic potential: leaf water potential at turgor loss point" =
    "MPa"
)


canonical_unit_to_conversion_id <- function(canonical_unit) {
  canonical_unit <- as.character(canonical_unit)
  
  data.table::fcase(
    canonical_unit == "MPa",
    "pressure_mpa",
    canonical_unit == "cm",
    "length_cm",
    canonical_unit == "mm",
    "length_mm",
    canonical_unit == "m",
    "length_m",
    canonical_unit %in% c("fraction", "fraction dry mass"),
    "fraction",
    startsWith(canonical_unit, "percent "),
    "percent_mass",
    startsWith(canonical_unit, "g C g-1 N"),
    "cn_ratio",
    canonical_unit == "g N g-1 P",
    "ratio",
    startsWith(canonical_unit, "ratio "),
    "ratio",
    grepl("^g [NP] kg-1 ", canonical_unit),
    "element_g_kg",
    grepl("^g [NP] m-2 ", canonical_unit),
    "element_area_g_m2",
    startsWith(canonical_unit, "kg dry mass m-3 "),
    "density_kg_m3",
    startsWith(canonical_unit, "g dry mass cm-3 "),
    "tissue_density_g_cm3",
    canonical_unit == "g dry mass g-1 fresh mass",
    "dry_matter_fraction",
    startsWith(canonical_unit, "m2 ") &
      grepl("kg-1 dry mass$", canonical_unit),
    "sla_m2_kg",
    canonical_unit == "g C m-2 leaf",
    "carbon_area_g_m2",
    canonical_unit == "year",
    "lifespan_year",
    canonical_unit == "year-1",
    "rate_year",
    canonical_unit == "unitless Q10",
    "q10",
    canonical_unit == "umol CO2 m-2 leaf s-1",
    "co2_flux_area_umol",
    grepl("^nmol CO2 g-1 .*dry mass s-1$", canonical_unit),
    "co2_flux_mass_nmol",
    grepl(
      "^nmol respiratory gas g-1 .*dry mass s-1$",
      canonical_unit
    ),
    "respiratory_gas_flux_mass_nmol",
    canonical_unit == "umol electrons m-2 leaf s-1",
    "electron_flux_area_umol",
    canonical_unit == "nmol electrons g-1 leaf dry mass s-1",
    "electron_flux_mass_nmol",
    canonical_unit == "mmol H2O m-2 leaf s-1",
    "water_flux_area_mmol",
    canonical_unit == "mmol H2O m-2 leaf s-1 MPa-1",
    "leaf_hydraulic_conductance",
    canonical_unit == "umol CO2 mol-1 air",
    "co2_concentration_umol_mol",
    canonical_unit == "kg H2O m-2 leaf MPa-1",
    "water_storage_capacity",
    canonical_unit %in% c(
      "mol C mol-1 photons",
      "mol CO2 mol-1 photons"
    ),
    "quantum_yield",
    canonical_unit == "umol CO2 g-1 N s-1",
    "pnue_umol_gn_s",
    canonical_unit == "umol CO2 mmol-1 H2O",
    "wue_umol_mmol",
    canonical_unit == "m root g-1 dry mass",
    "srl_m_g",
    canonical_unit == "g glucose g-1 leaf dry mass",
    "construction_cost_mass",
    canonical_unit == "g glucose m-2 leaf",
    "construction_cost_area",
    canonical_unit == "kg H2O m-1 s-1 MPa-1",
    "sapwood_conductivity",
    canonical_unit == paste(
      "mol CO2 m-2 leaf s-1",
      "TRY-reported-gm-convention"
    ),
    "mesophyll_conductance_reported",
    default = NA_character_
  )
}


build_try_unit_map <- function(trait_map) {
  if (!is.character(trait_map) || is.null(names(trait_map))) {
    stop(
      "trait_map 必须是 named character vector。",
      call. = FALSE
    )
  }
  
  trait_names <- as.character(names(trait_map))
  canonical_units <- unname(try_canonical_unit_lookup[trait_names])
  
  missing_traits <- trait_names[is.na(canonical_units)]
  if (length(missing_traits) > 0L) {
    stop(
      "unit map 缺少以下 TRY TraitName：\n",
      paste(missing_traits, collapse = "\n"),
      call. = FALSE
    )
  }
  
  conversion_ids <- canonical_unit_to_conversion_id(canonical_units)
  missing_conversion <- trait_names[is.na(conversion_ids)]
  if (length(missing_conversion) > 0L) {
    stop(
      "以下 traits 尚未分配纯单位换算类型：\n",
      paste(missing_conversion, collapse = "\n"),
      call. = FALSE
    )
  }
  
  out <- data.table::data.table(
    TraitName = trait_names,
    pecan_vname = unname(trait_map),
    canonical_unit = canonical_units,
    conversion_id = conversion_ids
  )
  
  if (anyDuplicated(out$TraitName)) {
    stop("unit_map 中 TraitName 重复。", call. = FALSE)
  }
  
  vname_unit_check <- out[
    ,
    .(
      n_units = data.table::uniqueN(canonical_unit),
      units = paste(sort(unique(canonical_unit)), collapse = " | ")
    ),
    by = pecan_vname
  ][n_units != 1L]
  
  if (nrow(vname_unit_check) > 0L) {
    stop(
      "同一个 PEcAn vname 对应多个 canonical unit：\n",
      paste(
        paste0(
          vname_unit_check$pecan_vname,
          " = ",
          vname_unit_check$units
        ),
        collapse = "\n"
      ),
      call. = FALSE
    )
  }
  
  out[]
}


# ============================================================================
# Actual dimensional conversion
# ============================================================================

normalize_try_unit_key <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("µ|μ", "u", x)
  x <- gsub("micro", "u", x, fixed = TRUE)
  x <- gsub("nano", "n", x, fixed = TRUE)
  x <- gsub("milli", "m", x, fixed = TRUE)
  x <- gsub("moles", "mol", x, fixed = TRUE)
  x <- gsub("mole", "mol", x, fixed = TRUE)
  x <- gsub("²", "2", x, fixed = TRUE)
  x <- gsub("¹", "1", x, fixed = TRUE)
  x <- gsub("⁻", "-", x, fixed = TRUE)
  x <- gsub("−|–", "-", x)
  x <- gsub(
    "(^|[[:space:]])per([[:space:]]|$)",
    "\\1/\\2",
    x,
    perl = TRUE
  )
  x <- gsub("co₂", "co2", x, fixed = TRUE)
  x <- gsub("\\^", "", x)
  x <- gsub("[[:space:]_·*]", "", x)
  x <- gsub("\\.", "", x)
  x
}


convert_try_unit_group <- function(
    value,
    unit,
    conversion_id,
    canonical_unit,
    days_per_year = 365
) {
  if (length(conversion_id) != 1L || is.na(conversion_id)) {
    stop("conversion_id 必须是单个非空值。", call. = FALSE)
  }
  if (length(canonical_unit) != 1L || is.na(canonical_unit)) {
    stop("canonical_unit 必须是单个非空值。", call. = FALSE)
  }
  if (
    length(days_per_year) != 1L ||
    !is.finite(days_per_year) ||
    days_per_year <= 0
  ) {
    stop("days_per_year 必须是正的有限标量。", call. = FALSE)
  }
  
  value <- suppressWarnings(as.numeric(as.character(value)))
  unit_character <- as.character(unit)
  key <- normalize_try_unit_key(unit_character)
  canonical_key <- normalize_try_unit_key(canonical_unit)
  
  # Remove labels that do not change dimensions. They remain present in the
  # final canonical unit; this is only for matching TRY spelling variants.
  strip_labels <- function(z) {
    gsub(
      paste0(
        "leaf|fineroot|root|stem|wood|branch|sapwood|litter|cwd|",
        "drymass|freshmass|freshvolume|volume|plant|photons|photon|",
        "air|electrons|electron|glucose"
      ),
      "",
      z
    )
  }
  
  key_core <- strip_labels(key)
  canonical_core <- strip_labels(canonical_key)
  key_no_co2 <- gsub("co2", "", key, fixed = TRUE)
  key_core_no_co2 <- gsub("co2", "", key_core, fixed = TRUE)
  
  converted <- rep(NA_real_, length(value))
  multiplier_used <- rep(NA_real_, length(value))
  conversion_rule <- rep(NA_character_, length(value))
  matched <- rep(FALSE, length(value))
  finite_value <- is.finite(value)
  has_unit <- !is.na(unit_character) & nzchar(trimws(unit_character))
  
  add_rule <- function(index, multiplier, rule_name) {
    index <- which(index & !matched)
    if (length(index) == 0L) return(invisible(NULL))
    converted[index] <<- value[index] * multiplier
    multiplier_used[index] <<- multiplier
    conversion_rule[index] <<- rule_name
    matched[index] <<- TRUE
    invisible(NULL)
  }
  
  # Exact target unit, including harmless tissue-label differences.
  add_rule(
    finite_value & has_unit &
      (key == canonical_key | key_core == canonical_core),
    1,
    "source unit equals canonical unit"
  )
  
  if (conversion_id == "pressure_mpa") {
    add_rule(
      finite_value & key_core == "-mpa" & value >= 0,
      -1,
      "positive magnitude in -MPa -> signed MPa"
    )
    add_rule(
      finite_value & key_core == "-mpa" & value < 0,
      1,
      "already signed value labelled -MPa -> MPa"
    )
    add_rule(finite_value & key_core == "mpa", 1, "MPa -> MPa")
    add_rule(finite_value & key_core == "kpa", 0.001, "kPa -> MPa")
    add_rule(finite_value & key_core == "pa", 1e-6, "Pa -> MPa")
    add_rule(finite_value & key_core == "bar", 0.1, "bar -> MPa")
  }
  
  if (conversion_id == "length_mm") {
    add_rule(finite_value & key_core == "mm", 1, "mm -> mm")
    add_rule(finite_value & key_core == "cm", 10, "cm -> mm")
    add_rule(finite_value & key_core == "m", 1000, "m -> mm")
    add_rule(finite_value & key_core == "um", 0.001, "um -> mm")
  }
  
  if (conversion_id == "length_cm") {
    add_rule(finite_value & key_core == "cm", 1, "cm -> cm")
    add_rule(finite_value & key_core == "mm", 0.1, "mm -> cm")
    add_rule(finite_value & key_core == "m", 100, "m -> cm")
  }
  
  if (conversion_id == "length_m") {
    add_rule(finite_value & key_core == "m", 1, "m -> m")
    add_rule(finite_value & key_core == "cm", 0.01, "cm -> m")
    add_rule(finite_value & key_core == "mm", 0.001, "mm -> m")
    add_rule(finite_value & key_core == "km", 1000, "km -> m")
  }
  
  if (conversion_id %in% c("fraction", "dry_matter_fraction")) {
    add_rule(
      finite_value & key_core %in% c(
        "1", "fraction", "proportion", "ratio", "g/g", "gg-1", "gg1"
      ),
      1,
      "fraction -> fraction"
    )
    add_rule(
      finite_value & key_core %in% c("%", "percent", "percentage", "pct"),
      0.01,
      "percent -> fraction"
    )
    add_rule(
      finite_value & key_core %in% c(
        "mg/g", "mgg-1", "mgg1", "g/kg", "gkg-1", "gkg1"
      ),
      0.001,
      "mg/g or g/kg -> fraction"
    )
  }
  
  if (conversion_id == "percent_mass") {
    add_rule(
      finite_value & key_core %in% c("%", "percent", "percentage", "pct"),
      1,
      "percent -> percent"
    )
    add_rule(
      finite_value & key_core %in% c(
        "mg/g", "mgg-1", "mgg1", "g/kg", "gkg-1", "gkg1"
      ),
      0.1,
      "mg/g or g/kg -> percent"
    )
    add_rule(
      finite_value & key_core %in% c("g/g", "gg-1", "gg1", "fraction", "1"),
      100,
      "fraction -> percent"
    )
  }
  
  if (conversion_id == "ratio") {
    add_rule(
      finite_value & key_core %in% c(
        "1", "ratio", "unitless", "dimensionless", "g/g", "gg-1", "gg1",
        "gn/gp", "gng-1p"
      ),
      1,
      "explicit ratio -> ratio"
    )
  }
  
  if (conversion_id == "cn_ratio") {
    add_rule(
      finite_value & key_core %in% c(
        "g/g", "gg-1", "gc/gn", "gcgn-1", "gcg-1n",
        "mg/mg", "mgmg-1", "mgcmg-1n",
        "kg/kg", "kgkg-1", "kgckg-1n"
      ),
      1,
      "explicit C:N mass ratio -> g C g-1 N"
    )
    add_rule(
      finite_value & key_core %in% c(
        "mol/mol", "molmol-1", "molc/moln", "molcmol-1n"
      ),
      12.0107 / 14.0067,
      "molar C:N -> mass C:N"
    )
  }
  
  if (conversion_id == "element_g_kg") {
    add_rule(
      finite_value & key_core %in% c(
        "g/kg", "gkg-1", "gkg1", "gnkg-1", "gpkg-1", "gckg-1",
        "mg/g", "mgg-1", "mgg1", "mgng-1", "mgpg-1", "mgcg-1"
      ),
      1,
      "g/kg or mg/g -> g/kg"
    )
    add_rule(
      finite_value & key_core %in% c("%", "percent", "percentage", "pct"),
      10,
      "percent -> g/kg"
    )
    add_rule(
      finite_value & key_core %in% c("g/g", "gg-1", "gg1"),
      1000,
      "g/g -> g/kg"
    )
    add_rule(
      finite_value & key_core %in% c(
        "mg/kg", "mgkg-1", "mgkg1", "mgnkg-1", "mgpkg-1", "mgckg-1"
      ),
      0.001,
      "mg/kg -> g/kg"
    )
  }
  
  if (conversion_id == "element_area_g_m2") {
    add_rule(
      finite_value & key_core %in% c(
        "g/m2", "gm-2", "gm2-1", "gnm-2", "gpm-2", "gcm-2"
      ),
      1,
      "g/m2 -> g/m2"
    )
    add_rule(
      finite_value & key_core %in% c(
        "mg/m2", "mgm-2", "mgm2-1", "mgnm-2", "mgpm-2", "mgcm-2"
      ),
      0.001,
      "mg/m2 -> g/m2"
    )
  }
  
  if (conversion_id == "density_kg_m3") {
    add_rule(
      finite_value & key_core %in% c("kg/m3", "kgm-3", "kgm3-1"),
      1,
      "kg/m3 -> kg/m3"
    )
    add_rule(
      finite_value & key_core %in% c("g/cm3", "gcm-3", "gcm3-1"),
      1000,
      "g/cm3 -> kg/m3"
    )
    add_rule(
      finite_value & key_core %in% c("mg/mm3", "mgmm-3", "mgmm3-1"),
      1000,
      "mg/mm3 -> kg/m3"
    )
  }
  
  if (conversion_id == "tissue_density_g_cm3") {
    add_rule(
      finite_value & key_core %in% c("g/cm3", "gcm-3", "gcm3-1"),
      1,
      "g/cm3 -> g/cm3"
    )
    add_rule(
      finite_value & key_core %in% c("kg/m3", "kgm-3", "kgm3-1"),
      0.001,
      "kg/m3 -> g/cm3"
    )
    add_rule(
      finite_value & key_core %in% c("mg/mm3", "mgmm-3", "mgmm3-1"),
      1,
      "mg/mm3 -> g/cm3"
    )
  }
  
  if (conversion_id == "sla_m2_kg") {
    add_rule(
      finite_value & key_core %in% c(
        "m2/kg", "m2kg-1", "m2kg1", "mm2/mg", "mm2mg-1", "mm2mg1"
      ),
      1,
      "m2/kg or mm2/mg -> m2/kg"
    )
    add_rule(
      finite_value & key_core %in% c("cm2/g", "cm2g-1", "cm2g1"),
      0.1,
      "cm2/g -> m2/kg"
    )
    add_rule(
      finite_value & key_core %in% c("m2/g", "m2g-1", "m2g1"),
      1000,
      "m2/g -> m2/kg"
    )
    add_rule(
      finite_value & key_core %in% c("mm2/g", "mm2g-1", "mm2g1"),
      0.001,
      "mm2/g -> m2/kg"
    )
    add_rule(
      finite_value & key_core %in% c("cm2/mg", "cm2mg-1", "cm2mg1"),
      100,
      "cm2/mg -> m2/kg"
    )
  }
  
  if (conversion_id == "carbon_area_g_m2") {
    add_rule(
      finite_value & key_core %in% c(
        "g/m2", "gm-2", "gm2-1", "gc/m2", "gcarbon/m2"
      ),
      1,
      "g C/m2 -> g C/m2"
    )
    add_rule(
      finite_value & key_core %in% c(
        "mg/m2", "mgm-2", "mgm2-1", "mgc/m2", "mgcarbon/m2"
      ),
      0.001,
      "mg C/m2 -> g C/m2"
    )
    add_rule(
      finite_value & key_core %in% c("mg/cm2", "mgcm2-1"),
      10,
      "mg C/cm2 -> g C/m2"
    )
    add_rule(
      finite_value & key_core %in% c("mg/mm2", "mgmm-2", "mgmm2-1"),
      1000,
      "mg C/mm2 -> g C/m2"
    )
  }
  
  if (conversion_id == "lifespan_year") {
    add_rule(
      finite_value & key_core %in% c("year", "years", "yr", "yrs", "a", "annum"),
      1,
      "year -> year"
    )
    add_rule(
      finite_value & key_core %in% c("month", "months", "mo"),
      1 / 12,
      "month -> year"
    )
    add_rule(
      finite_value & key_core %in% c("day", "days", "d"),
      1 / days_per_year,
      "day -> year"
    )
    add_rule(
      finite_value & key_core %in% c("week", "weeks", "wk"),
      7 / days_per_year,
      "week -> year"
    )
  }
  
  if (conversion_id == "rate_year") {
    add_rule(
      finite_value & key_core %in% c(
        "1/year", "year-1", "yr-1", "a-1", "/year", "/yr"
      ),
      1,
      "year-1 -> year-1"
    )
    add_rule(
      finite_value & key_core %in% c(
        "%/year", "%year-1", "%/yr", "%yr-1",
        "percent/year", "percentyear-1", "percentyr-1"
      ),
      0.01,
      "percent/year -> fraction/year"
    )
    add_rule(
      finite_value & key_core %in% c("1/day", "day-1", "d-1", "/day", "/d"),
      days_per_year,
      "day-1 -> year-1"
    )
    add_rule(
      finite_value & key_core %in% c(
        "%/day", "%day-1", "%/d", "%d-1",
        "percent/day", "percentday-1", "percentd-1"
      ),
      0.01 * days_per_year,
      "percent/day -> fraction/year"
    )
    add_rule(
      finite_value & key_core %in% c("1/month", "month-1", "mo-1", "/month"),
      12,
      "month-1 -> year-1"
    )
  }
  
  if (conversion_id == "q10") {
    add_rule(
      finite_value & key_core %in% c(
        "1", "unitless", "dimensionless", "q10", "ratio"
      ),
      1,
      "Q10 -> unitless Q10"
    )
  }
  
  if (conversion_id == "co2_flux_area_umol") {
    add_rule(
      finite_value & key_no_co2 %in% c(
        "umol/m2/s", "umolm-2s-1", "umolm2-1s-1", "umolm-2/sec"
      ),
      1,
      "umol CO2 m-2 s-1 -> canonical"
    )
    add_rule(
      finite_value & key_no_co2 %in% c(
        "nmol/m2/s", "nmolm-2s-1", "nmolm2-1s-1"
      ),
      0.001,
      "nmol CO2 m-2 s-1 -> umol CO2 m-2 s-1"
    )
    add_rule(
      finite_value & key_no_co2 %in% c(
        "mmol/m2/s", "mmolm-2s-1", "mmolm2-1s-1"
      ),
      1000,
      "mmol CO2 m-2 s-1 -> umol CO2 m-2 s-1"
    )
  }
  
  if (conversion_id == "co2_flux_mass_nmol") {
    add_rule(
      finite_value & key_core_no_co2 %in% c(
        "nmol/g/s", "nmolg-1s-1", "nmolgdm-1s-1",
        "umol/kg/s", "umolkg-1s-1", "umolkgdm-1s-1"
      ),
      1,
      "nmol/g or umol/kg -> nmol/g"
    )
    add_rule(
      finite_value & key_core_no_co2 %in% c(
        "umol/g/s", "umolg-1s-1", "umolgdm-1s-1"
      ),
      1000,
      "umol/g -> nmol/g"
    )
    add_rule(
      finite_value & key_core_no_co2 %in% c(
        "nmol/kg/s", "nmolkg-1s-1", "nmolkgdm-1s-1"
      ),
      0.001,
      "nmol/kg -> nmol/g"
    )
  }
  
  if (conversion_id == "respiratory_gas_flux_mass_nmol") {
    add_rule(
      finite_value & key_core_no_co2 %in% c(
        "nmol/g/s", "nmolg-1s-1", "umol/kg/s", "umolkg-1s-1"
      ),
      1,
      "nmol/g or umol/kg -> nmol/g; gas identity retained"
    )
    add_rule(
      finite_value & key_core_no_co2 %in% c("umol/g/s", "umolg-1s-1"),
      1000,
      "umol/g -> nmol/g; gas identity retained"
    )
  }
  
  if (conversion_id == "electron_flux_area_umol") {
    add_rule(
      finite_value & key_core %in% c("umol/m2/s", "umolm-2s-1", "umolm2-1s-1"),
      1,
      "umol electrons m-2 s-1 -> canonical"
    )
    add_rule(
      finite_value & key_core %in% c("nmol/m2/s", "nmolm-2s-1", "nmolm2-1s-1"),
      0.001,
      "nmol electrons m-2 s-1 -> umol m-2 s-1"
    )
  }
  
  if (conversion_id == "electron_flux_mass_nmol") {
    add_rule(
      finite_value & key_core %in% c(
        "nmol/g/s", "nmolg-1s-1", "umol/kg/s", "umolkg-1s-1"
      ),
      1,
      "nmol/g or umol/kg -> nmol/g"
    )
    add_rule(
      finite_value & key_core %in% c("umol/g/s", "umolg-1s-1"),
      1000,
      "umol/g -> nmol/g"
    )
  }
  
  if (conversion_id == "water_flux_area_mmol") {
    add_rule(
      finite_value & key_core %in% c(
        "mmolh2o/m2/s", "mmolh2om-2s-1", "mmol/m2/s", "mmolm-2s-1"
      ),
      1,
      "mmol H2O m-2 s-1 -> canonical"
    )
    add_rule(
      finite_value & key_core %in% c(
        "umolh2o/m2/s", "umolh2om-2s-1", "umol/m2/s", "umolm-2s-1"
      ),
      0.001,
      "umol H2O m-2 s-1 -> mmol m-2 s-1"
    )
    add_rule(
      finite_value & key_core %in% c(
        "molh2o/m2/s", "molh2om-2s-1", "mol/m2/s", "molm-2s-1"
      ),
      1000,
      "mol H2O m-2 s-1 -> mmol m-2 s-1"
    )
  }
  
  if (conversion_id == "co2_concentration_umol_mol") {
    add_rule(
      finite_value & key_core %in% c(
        "umolco2/mol", "umol/mol", "umolmol-1", "ppm"
      ),
      1,
      "umol/mol or ppm -> umol/mol"
    )
    add_rule(
      finite_value & key_core %in% c("mmol/mol", "mmolmol-1"),
      1000,
      "mmol/mol -> umol/mol"
    )
  }
  
  if (conversion_id == "quantum_yield") {
    add_rule(
      finite_value & key_core %in% c(
        "molc/mol", "molco2/mol", "mol/mol", "molmol-1",
        "umolc/umol", "umolco2/umol", "umol/umol"
      ),
      1,
      "molar yield -> mol/mol"
    )
  }
  
  if (conversion_id == "pnue_umol_gn_s") {
    add_rule(
      finite_value & key_core %in% c(
        "umolco2/gn/s", "umol/gn/s", "umolg-1ns-1",
        "nmolco2/mgn/s", "nmol/mgn/s"
      ),
      1,
      "umol/g N or nmol/mg N -> canonical"
    )
  }
  
  if (conversion_id == "wue_umol_mmol") {
    add_rule(
      finite_value & key_core %in% c(
        "umolco2/mmolh2o", "umol/mmol", "mmolco2/molh2o", "mmol/mol"
      ),
      1,
      "umol/mmol or mmol/mol -> canonical"
    )
    add_rule(
      finite_value & key_core %in% c("molco2/molh2o", "mol/mol"),
      1000,
      "mol/mol -> umol/mmol"
    )
  }
  
  if (conversion_id == "srl_m_g") {
    add_rule(
      finite_value & key_core %in% c("m/g", "mg-1", "mg1"),
      1,
      "m/g -> m/g"
    )
    add_rule(
      finite_value & key_core %in% c("cm/g", "cmg-1", "cmg1"),
      0.01,
      "cm/g -> m/g"
    )
    add_rule(
      finite_value & key_core %in% c("m/kg", "mkg-1", "mkg1"),
      0.001,
      "m/kg -> m/g"
    )
  }
  
  if (conversion_id == "construction_cost_mass") {
    add_rule(
      finite_value & key_core %in% c("g/g", "gg-1", "gg1"),
      1,
      "g glucose/g dry mass -> canonical"
    )
    add_rule(
      finite_value & key_core %in% c("mg/g", "mgg-1", "mgg1"),
      0.001,
      "mg glucose/g -> g/g"
    )
  }
  
  if (conversion_id == "construction_cost_area") {
    add_rule(
      finite_value & key_core %in% c("g/m2", "gm-2", "gm2-1"),
      1,
      "g glucose/m2 -> canonical"
    )
    add_rule(
      finite_value & key_core %in% c("mg/m2", "mgm-2", "mgm2-1"),
      0.001,
      "mg glucose/m2 -> g/m2"
    )
  }
  
  # Complex hydraulic dimensions are accepted only when explicitly equivalent.
  if (conversion_id == "leaf_hydraulic_conductance") {
    add_rule(
      finite_value & key %in% c(
        canonical_key, "mmolh2om-2s-1mpa-1", "mmol/m2/s/mpa"
      ),
      1,
      "leaf hydraulic conductance canonical unit"
    )
  }
  
  if (conversion_id == "water_storage_capacity") {
    add_rule(
      finite_value & key %in% c(canonical_key, "kgh2om-2mpa-1", "kg/m2/mpa"),
      1,
      "leaf water storage canonical unit"
    )
  }
  
  if (conversion_id == "sapwood_conductivity") {
    add_rule(
      finite_value & key %in% c(canonical_key, "kgh2om-1s-1mpa-1", "kg/m/s/mpa"),
      1,
      "sapwood conductivity canonical unit"
    )
  }
  
  if (conversion_id == "mesophyll_conductance_reported") {
    add_rule(
      finite_value & key %in% c(canonical_key, "molco2m-2s-1", "molm-2s-1", "mol/m2/s"),
      1,
      "TRY-reported gm retained without inventing a pressure denominator"
    )
  }
  
  status <- ifelse(
    !finite_value,
    "FAIL_nonfinite_value",
    ifelse(
      !has_unit,
      "FAIL_missing_unit",
      ifelse(matched, "PASS", "FAIL_unsupported_unit")
    )
  )
  
  data.table::data.table(
    canonical_value = converted,
    conversion_multiplier = multiplier_used,
    conversion_rule = conversion_rule,
    unit_status = status,
    source_unit_key = key
  )
}


convert_try_units_with_unit_map <- function(
    try_data,
    unit_map,
    unit_col = "UnitName",
    value_col = "StdValue",
    days_per_year = 365,
    unsupported_action = c("stop", "drop")
) {
  unsupported_action <- match.arg(unsupported_action)
  
  x_source <- data.table::as.data.table(try_data)
  map <- data.table::copy(data.table::as.data.table(unit_map))
  
  required_data <- c("TraitName", unit_col, value_col)
  missing_data <- setdiff(required_data, names(x_source))
  if (length(missing_data) > 0L) {
    stop(
      "try_data 缺少列：",
      paste(missing_data, collapse = ", "),
      call. = FALSE
    )
  }
  
  required_map <- c(
    "TraitName", "pecan_vname", "canonical_unit", "conversion_id"
  )
  missing_map <- setdiff(required_map, names(map))
  if (length(missing_map) > 0L) {
    stop(
      "unit_map 缺少列：",
      paste(missing_map, collapse = ", "),
      call. = FALSE
    )
  }
  if (anyDuplicated(map$TraitName)) {
    stop("unit_map$TraitName 必须唯一。", call. = FALSE)
  }
  
  # Copy only mapped records; do not duplicate an entire large TRY table first.
  mapped_names <- as.character(map$TraitName)
  x <- data.table::copy(
    x_source[as.character(TraitName) %in% mapped_names]
  )
  if (nrow(x) == 0L) {
    stop("try_data 中没有命中 unit_map 的记录。", call. = FALSE)
  }
  
  x[, unit_conversion_row_id__ := .I]
  x[, StdValue_before_unit_conversion := get(value_col)]
  x[, UnitName_before_unit_conversion := as.character(get(unit_col))]
  
  x <- merge(
    x,
    map,
    by = "TraitName",
    all.x = TRUE,
    sort = FALSE
  )
  data.table::setorder(x, unit_conversion_row_id__)
  
  groups <- unique(x[, .(conversion_id, canonical_unit)])
  converted_list <- vector("list", nrow(groups))
  
  for (group_index in seq_len(nrow(groups))) {
    conversion_i <- groups$conversion_id[group_index]
    canonical_i <- groups$canonical_unit[group_index]
    row_index <- which(
      x$conversion_id == conversion_i &
        x$canonical_unit == canonical_i
    )
    
    converted_i <- convert_try_unit_group(
      value = x$StdValue_before_unit_conversion[row_index],
      unit = x$UnitName_before_unit_conversion[row_index],
      conversion_id = conversion_i,
      canonical_unit = canonical_i,
      days_per_year = days_per_year
    )
    converted_i[, unit_conversion_row_id__ := x$unit_conversion_row_id__[row_index]]
    converted_list[[group_index]] <- converted_i
  }
  
  converted <- data.table::rbindlist(
    converted_list,
    use.names = TRUE,
    fill = TRUE
  )
  x <- merge(
    x,
    converted,
    by = "unit_conversion_row_id__",
    all.x = TRUE,
    sort = FALSE
  )
  data.table::setorder(x, unit_conversion_row_id__)
  
  missing_unit_records <- data.table::copy(
    x[unit_status == "FAIL_missing_unit"]
  )
  nonfinite_value_records <- data.table::copy(
    x[unit_status == "FAIL_nonfinite_value"]
  )
  unsupported_unit_records <- data.table::copy(
    x[unit_status == "FAIL_unsupported_unit"]
  )
  
  if (
    unsupported_action == "stop" &&
    nrow(unsupported_unit_records) > 0L
  ) {
    unsupported_summary <- unsupported_unit_records[
      ,
      .(n_records = .N),
      by = .(
        TraitName,
        source_unit = UnitName_before_unit_conversion,
        canonical_unit,
        conversion_id
      )
    ][order(TraitName, -n_records)]
    
    print(unsupported_summary, nrows = Inf)
    stop(
      "发现 ",
      nrow(unsupported_unit_records),
      " 条无法安全换算的单位。上表已列出；",
      "如决定暂时删除这些记录，可设置 unsupported_action = 'drop'。",
      call. = FALSE
    )
  }
  
  use <- data.table::copy(
    x[unit_status == "PASS" & is.finite(canonical_value)]
  )
  if (nrow(use) == 0L) {
    stop("单位换算后没有可用记录。", call. = FALSE)
  }
  
  use[, (value_col) := canonical_value]
  use[, (unit_col) := canonical_unit]
  
  conversion_audit <- x[
    ,
    .(
      n_records = .N,
      n_pass = sum(unit_status == "PASS"),
      n_missing_unit = sum(unit_status == "FAIL_missing_unit"),
      n_nonfinite_value = sum(unit_status == "FAIL_nonfinite_value"),
      n_unsupported_unit = sum(unit_status == "FAIL_unsupported_unit")
    ),
    by = .(
      TraitName,
      pecan_vname,
      source_unit = UnitName_before_unit_conversion,
      canonical_unit,
      conversion_id,
      conversion_multiplier,
      conversion_rule,
      unit_status
    )
  ][order(TraitName, source_unit)]
  
  attr(use, "unit_conversion_audit") <- conversion_audit
  attr(use, "missing_unit_records") <- missing_unit_records
  attr(use, "nonfinite_value_records") <- nonfinite_value_records
  attr(use, "unsupported_unit_records") <- unsupported_unit_records
  
  message(
    "TRY unit conversion finished.",
    "\nInput mapped rows: ", format(nrow(x), big.mark = ","),
    "\nConverted rows: ", format(nrow(use), big.mark = ","),
    "\nMissing units removed: ", nrow(missing_unit_records),
    "\nNon-finite values removed: ", nrow(nonfinite_value_records),
    "\nUnsupported units: ", nrow(unsupported_unit_records),
    "\nNo physical-range filtering was performed."
  )
  
  use[]
}


validate_try_unit_conversion_examples <- function(
    tolerance = 1e-10
) {
  tests <- data.table::data.table(
    test_name = c(
      "SLA_cm2_g_to_m2_kg",
      "leafC_mg_g_to_percent",
      "wood_density_g_cm3_to_kg_m3",
      "lifespan_day_to_year",
      "root_depth_cm_to_m",
      "leaf_N_mg_g_to_g_kg",
      "fraction_percent_to_fraction",
      "negative_MPa_magnitude",
      "area_CO2_flux_identity",
      "root_respiration_nmol_identity"
    ),
    value = c(20, 476, 0.6, 365, 100, 20, 50, 1.5, 10, 7.5),
    unit = c(
      "cm2/g",
      "mg/g",
      "g/cm3",
      "day",
      "cm",
      "mg/g",
      "%",
      "-MPa",
      "umol CO2 m-2 s-1",
      "nano moles g-1 s-1"
    ),
    conversion_id = c(
      "sla_m2_kg",
      "percent_mass",
      "density_kg_m3",
      "lifespan_year",
      "length_m",
      "element_g_kg",
      "fraction",
      "pressure_mpa",
      "co2_flux_area_umol",
      "respiratory_gas_flux_mass_nmol"
    ),
    canonical_unit = c(
      "m2 leaf kg-1 dry mass",
      "percent leaf dry mass",
      "kg dry mass m-3 fresh volume",
      "year",
      "m",
      "g N kg-1 leaf dry mass",
      "fraction",
      "MPa",
      "umol CO2 m-2 leaf s-1",
      "nmol respiratory gas g-1 root dry mass s-1"
    ),
    expected = c(2, 47.6, 600, 1, 1, 20, 0.5, -1.5, 10, 7.5)
  )
  
  calculated <- data.table::rbindlist(
    lapply(seq_len(nrow(tests)), function(index) {
      convert_try_unit_group(
        value = tests$value[index],
        unit = tests$unit[index],
        conversion_id = tests$conversion_id[index],
        canonical_unit = tests$canonical_unit[index],
        days_per_year = 365
      )
    }),
    use.names = TRUE,
    fill = TRUE
  )
  
  result <- cbind(tests, calculated)
  result[, absolute_error := abs(canonical_value - expected)]
  result[, passed :=
           unit_status == "PASS" &
           is.finite(absolute_error) &
           absolute_error <= tolerance * pmax(1, abs(expected))
  ]
  
  if (any(!result$passed)) {
    stop(
      "单位换算自检失败：",
      paste(result[passed == FALSE, test_name], collapse = ", "),
      call. = FALSE
    )
  }
  
  result[]
}


# ============================================================================
# Complete wrapper: one PFT + trait selection + unit conversion -> try_data
# ============================================================================

# Join a separately stored observation-coordinate table back to the TRY data.
# TRY trait exports do not necessarily contain Latitude/Longitude, while the
# earlier PFT-mapping workflow stores them in
# na_species_res$observation_coordinates. Never join coordinates by species:
# use the most specific common observation identifier.
attach_try_observation_coordinates <- function(
    try_data,
    observation_coordinate_data,
    observation_lat_col = "Latitude",
    observation_lon_col = "Longitude",
    key_candidates = c(
      "ObsDataID",
      "ObservationID",
      "OrigObsDataID",
      "observation_id",
      "obs_id"
    )
) {
  try_dt <- data.table::copy(data.table::as.data.table(try_data))
  coordinate_dt <- data.table::copy(
    data.table::as.data.table(observation_coordinate_data)
  )

  if (nrow(coordinate_dt) == 0L) {
    stop("observation_coordinate_data has no rows.", call. = FALSE)
  }

  normalize_identifier_name <- function(x) {
    tolower(gsub("[^[:alnum:]]", "", x))
  }
  try_normalized_names <- normalize_identifier_name(names(try_dt))
  coordinate_normalized_names <- normalize_identifier_name(
    names(coordinate_dt)
  )

  try_join_key <- NULL
  lookup_join_key <- NULL
  for (candidate in key_candidates) {
    candidate_normalized <- normalize_identifier_name(candidate)
    try_match <- which(try_normalized_names == candidate_normalized)
    lookup_match <- which(
      coordinate_normalized_names == candidate_normalized
    )
    if (length(try_match) > 0L && length(lookup_match) > 0L) {
      try_join_key <- names(try_dt)[try_match[[1L]]]
      lookup_join_key <- names(coordinate_dt)[lookup_match[[1L]]]
      break
    }
  }
  if (is.null(try_join_key) || is.null(lookup_join_key)) {
    stop(
      "TRY data and observation_coordinate_data have no common observation ",
      "identifier. Expected one of: ",
      paste(key_candidates, collapse = ", "),
      call. = FALSE
    )
  }

  resolve_coordinate_column <- function(x, requested, candidates, label) {
    if (requested %in% names(x)) {
      return(requested)
    }
    found <- intersect(candidates, names(x))
    if (length(found) == 0L) {
      stop(
        "observation_coordinate_data has no ", label, " column. Tried: ",
        paste(unique(c(requested, candidates)), collapse = ", "),
        call. = FALSE
      )
    }
    found[[1L]]
  }

  lookup_lat_col <- resolve_coordinate_column(
    coordinate_dt,
    observation_lat_col,
    c("lat", "latitude", "Latitude", "LAT", "obs_lat", "try_lat"),
    "latitude"
  )
  lookup_lon_col <- resolve_coordinate_column(
    coordinate_dt,
    observation_lon_col,
    c(
      "lon", "longitude", "Longitude", "LON", "long",
      "obs_lon", "try_lon"
    ),
    "longitude"
  )

  coordinate_lookup <- coordinate_dt[
    ,
    .(
      coordinate_join_key__ = trimws(as.character(get(lookup_join_key))),
      lookup_latitude__ = suppressWarnings(
        as.numeric(as.character(get(lookup_lat_col)))
      ),
      lookup_longitude__ = suppressWarnings(
        as.numeric(as.character(get(lookup_lon_col)))
      )
    )
  ][
    !is.na(coordinate_join_key__) & nzchar(coordinate_join_key__)
  ]

  coordinate_conflicts <- coordinate_lookup[
    is.finite(lookup_latitude__) & is.finite(lookup_longitude__),
    .(
      n_coordinate_pairs = data.table::uniqueN(
        paste(lookup_latitude__, lookup_longitude__, sep = "|")
      )
    ),
    by = coordinate_join_key__
  ][n_coordinate_pairs > 1L]

  if (nrow(coordinate_conflicts) > 0L) {
    stop(
      nrow(coordinate_conflicts),
      " observation identifiers have conflicting coordinate pairs in ",
      "observation_coordinate_data. The coordinate join was not performed.",
      call. = FALSE
    )
  }

  coordinate_lookup <- coordinate_lookup[
    order(
      coordinate_join_key__,
      -as.integer(is.finite(lookup_latitude__) & is.finite(lookup_longitude__))
    )
  ][
    !duplicated(coordinate_join_key__)
  ]

  if (!observation_lat_col %in% names(try_dt)) {
    try_dt[, (observation_lat_col) := NA_real_]
  }
  if (!observation_lon_col %in% names(try_dt)) {
    try_dt[, (observation_lon_col) := NA_real_]
  }

  try_dt[, (observation_lat_col) := suppressWarnings(
    as.numeric(as.character(get(observation_lat_col)))
  )]
  try_dt[, (observation_lon_col) := suppressWarnings(
    as.numeric(as.character(get(observation_lon_col)))
  )]
  try_dt[
    ,
    coordinate_join_key__ := trimws(as.character(get(try_join_key)))
  ]

  valid_before <-
    is.finite(try_dt[[observation_lat_col]]) &
    try_dt[[observation_lat_col]] >= -90 &
    try_dt[[observation_lat_col]] <= 90 &
    is.finite(try_dt[[observation_lon_col]]) &
    try_dt[[observation_lon_col]] >= -180 &
    try_dt[[observation_lon_col]] <= 180
  n_valid_before <- sum(valid_before)

  try_dt[
    coordinate_lookup,
    on = .(coordinate_join_key__),
    `:=`(
      joined_latitude__ = i.lookup_latitude__,
      joined_longitude__ = i.lookup_longitude__
    )
  ]

  fill_coordinate <-
    !valid_before &
    is.finite(try_dt$joined_latitude__) &
    try_dt$joined_latitude__ >= -90 &
    try_dt$joined_latitude__ <= 90 &
    is.finite(try_dt$joined_longitude__) &
    try_dt$joined_longitude__ >= -180 &
    try_dt$joined_longitude__ <= 180

  try_dt[
    fill_coordinate,
    (observation_lat_col) := joined_latitude__
  ]
  try_dt[
    fill_coordinate,
    (observation_lon_col) := joined_longitude__
  ]

  valid_after <-
    is.finite(try_dt[[observation_lat_col]]) &
    try_dt[[observation_lat_col]] >= -90 &
    try_dt[[observation_lat_col]] <= 90 &
    is.finite(try_dt[[observation_lon_col]]) &
    try_dt[[observation_lon_col]] >= -180 &
    try_dt[[observation_lon_col]] <= 180

  join_audit <- data.table::data.table(
    join_key = paste0(try_join_key, " -> ", lookup_join_key),
    try_join_key = try_join_key,
    lookup_join_key = lookup_join_key,
    lookup_latitude_column = lookup_lat_col,
    lookup_longitude_column = lookup_lon_col,
    n_try_rows = nrow(try_dt),
    n_coordinate_lookup_rows = nrow(coordinate_lookup),
    n_valid_coordinates_before_join = n_valid_before,
    n_coordinates_filled_from_lookup = sum(fill_coordinate),
    n_valid_coordinates_after_join = sum(valid_after),
    n_rows_still_without_valid_coordinates = sum(!valid_after)
  )

  try_dt[
    ,
    c(
      "coordinate_join_key__",
      "joined_latitude__",
      "joined_longitude__"
    ) := NULL
  ]

  attr(try_dt, "observation_coordinate_join_audit") <- join_audit
  try_dt[]
}


# Assign each TRY observation to exactly one candidate PFT.
#
# Species that occur in only one included PFT inherit that PFT. Species that
# occur in more than one included PFT are assigned observation-by-observation
# to the nearest reference point among that species' candidate PFTs. Ambiguous
# observations without usable coordinates are never copied to every PFT.
assign_try_observations_to_pft <- function(
    try_data,
    pft_species_map,
    pft_coordinate_map,
    species_col = "AccSpeciesID",
    observation_lat_col = "Latitude",
    observation_lon_col = "Longitude",
    max_distance_km = 250,
    unassigned_action = c("drop", "stop")
) {
  unassigned_action <- match.arg(unassigned_action)

  resolve_column <- function(x, requested, candidates, label) {
    if (!is.null(requested) && requested %in% names(x)) {
      return(requested)
    }
    found <- intersect(candidates, names(x))
    if (length(found) == 0L) {
      stop(
        label,
        " column was not found. Tried: ",
        paste(unique(c(requested, candidates)), collapse = ", "),
        call. = FALSE
      )
    }
    found[[1L]]
  }

  as_include_flag <- function(x) {
    if (is.logical(x)) {
      x[is.na(x)] <- FALSE
      return(x)
    }
    tolower(trimws(as.character(x))) %in% c(
      "true", "t", "1", "yes", "y"
    )
  }

  lon_lat_to_xyz <- function(lon, lat) {
    lon_rad <- as.numeric(lon) * pi / 180
    lat_rad <- as.numeric(lat) * pi / 180
    cbind(
      cos(lat_rad) * cos(lon_rad),
      cos(lat_rad) * sin(lon_rad),
      sin(lat_rad)
    )
  }

  nearest_reference <- function(query_lon, query_lat, reference) {
    query_xyz <- lon_lat_to_xyz(query_lon, query_lat)
    reference_xyz <- lon_lat_to_xyz(
      reference$reference_lon__,
      reference$reference_lat__
    )

    if (requireNamespace("RANN", quietly = TRUE)) {
      nearest_index <- as.integer(
        RANN::nn2(
          data = reference_xyz,
          query = query_xyz,
          k = 1L,
          searchtype = "standard"
        )$nn.idx[, 1L]
      )
    } else {
      # Base-R fallback. Work in chunks so a large TRY table does not allocate
      # one observation-by-reference distance matrix.
      nearest_index <- integer(nrow(query_xyz))
      chunk_size <- 1000L
      starts <- seq.int(1L, nrow(query_xyz), by = chunk_size)
      for (start_i in starts) {
        end_i <- min(nrow(query_xyz), start_i + chunk_size - 1L)
        similarity <- tcrossprod(
          query_xyz[start_i:end_i, , drop = FALSE],
          reference_xyz
        )
        nearest_index[start_i:end_i] <- max.col(
          similarity,
          ties.method = "first"
        )
      }
    }

    nearest_dot <- rowSums(
      query_xyz * reference_xyz[nearest_index, , drop = FALSE]
    )
    nearest_dot <- pmax(-1, pmin(1, nearest_dot))

    data.table::data.table(
      nearest_reference_index__ = nearest_index,
      pft_assignment_distance_km = 6371.0088 * acos(nearest_dot)
    )
  }

  try_dt <- data.table::copy(data.table::as.data.table(try_data))
  pft_dt <- data.table::copy(data.table::as.data.table(pft_species_map))
  reference_dt <- data.table::copy(
    data.table::as.data.table(pft_coordinate_map)
  )

  required_try <- c(
    species_col,
    observation_lat_col,
    observation_lon_col
  )
  missing_try <- setdiff(required_try, names(try_dt))
  if (length(missing_try) > 0L) {
    stop(
      "TRY data is missing observation-assignment columns: ",
      paste(missing_try, collapse = ", "),
      call. = FALSE
    )
  }

  required_pft <- c("try_species_id", "final_pft")
  missing_pft <- setdiff(required_pft, names(pft_dt))
  if (length(missing_pft) > 0L) {
    stop(
      "pft_species_map is missing columns: ",
      paste(missing_pft, collapse = ", "),
      call. = FALSE
    )
  }

  reference_pft_col <- resolve_column(
    reference_dt,
    "final_pft",
    c("pftname", "pft_name", "pft", "PFT"),
    "Reference PFT"
  )
  reference_lat_col <- resolve_column(
    reference_dt,
    NULL,
    c("lat", "latitude", "Latitude", "LAT"),
    "Reference latitude"
  )
  reference_lon_col <- resolve_column(
    reference_dt,
    NULL,
    c("lon", "longitude", "Longitude", "LON", "long"),
    "Reference longitude"
  )

  if (
    length(max_distance_km) != 1L ||
    !is.finite(max_distance_km) ||
    max_distance_km <= 0
  ) {
    stop("max_distance_km must be one positive finite number.", call. = FALSE)
  }

  pft_dt[, include_internal__ := if ("include" %in% names(pft_dt)) {
    as_include_flag(include)
  } else {
    TRUE
  }]
  pft_dt <- unique(
    pft_dt[
      include_internal__ == TRUE &
        !is.na(try_species_id) &
        nzchar(trimws(as.character(try_species_id))) &
        !is.na(final_pft) &
        nzchar(trimws(as.character(final_pft))),
      .(
        species_id__ = trimws(as.character(try_species_id)),
        candidate_pft__ = trimws(as.character(final_pft))
      )
    ]
  )

  species_candidates <- pft_dt[
    ,
    .(
      n_candidate_pfts__ = data.table::uniqueN(candidate_pft__),
      candidate_pfts__ = paste(
        sort(unique(candidate_pft__)),
        collapse = "\u001f"
      ),
      sole_candidate_pft__ = if (data.table::uniqueN(candidate_pft__) == 1L) {
        unique(candidate_pft__)[[1L]]
      } else {
        NA_character_
      }
    ),
    by = species_id__
  ]

  reference_dt <- unique(
    reference_dt[
      ,
      .(
        reference_pft__ = trimws(as.character(get(reference_pft_col))),
        reference_lat__ = suppressWarnings(
          as.numeric(as.character(get(reference_lat_col)))
        ),
        reference_lon__ = suppressWarnings(
          as.numeric(as.character(get(reference_lon_col)))
        )
      )
    ][
      !is.na(reference_pft__) &
        nzchar(reference_pft__) &
        is.finite(reference_lat__) &
        reference_lat__ >= -90 &
        reference_lat__ <= 90 &
        is.finite(reference_lon__) &
        reference_lon__ >= -180 &
        reference_lon__ <= 180
    ]
  )
  data.table::setorder(
    reference_dt,
    reference_pft__,
    reference_lat__,
    reference_lon__
  )

  if (nrow(reference_dt) == 0L) {
    stop("pft_coordinate_map has no usable PFT coordinates.", call. = FALSE)
  }

  try_dt[, observation_row_id__ := .I]
  try_dt[, species_id__ := trimws(as.character(get(species_col)))]
  try_dt[, observation_lat__ := suppressWarnings(
    as.numeric(as.character(get(observation_lat_col)))
  )]
  try_dt[, observation_lon__ := suppressWarnings(
    as.numeric(as.character(get(observation_lon_col)))
  )]

  try_dt[
    species_candidates,
    on = .(species_id__),
    `:=`(
      n_candidate_pfts__ = i.n_candidate_pfts__,
      candidate_pfts__ = i.candidate_pfts__,
      sole_candidate_pft__ = i.sole_candidate_pft__
    )
  ]

  try_dt[, assigned_final_pft := NA_character_]
  try_dt[, pft_assignment_method := NA_character_]
  try_dt[, pft_assignment_distance_km := NA_real_]
  try_dt[, pft_assignment_status := "UNASSIGNED"]

  try_dt[
    n_candidate_pfts__ == 1L,
    `:=`(
      assigned_final_pft = sole_candidate_pft__,
      pft_assignment_method = "unique_species_pft",
      pft_assignment_status = "ASSIGNED"
    )
  ]

  try_dt[
    is.na(n_candidate_pfts__),
    pft_assignment_method := "species_not_in_pft_map"
  ]

  ambiguous_valid <- try_dt[
    n_candidate_pfts__ > 1L &
      is.finite(observation_lat__) &
      observation_lat__ >= -90 &
      observation_lat__ <= 90 &
      is.finite(observation_lon__) &
      observation_lon__ >= -180 &
      observation_lon__ <= 180,
    .(
      observation_row_id__,
      observation_lat__,
      observation_lon__,
      candidate_pfts__
    )
  ]

  if (nrow(ambiguous_valid) > 0L) {
    unique_queries <- unique(
      ambiguous_valid[
        ,
        .(
          observation_lat__,
          observation_lon__,
          candidate_pfts__
        )
      ]
    )
    unique_queries[, query_id__ := .I]
    unique_queries[, spatial_pft__ := NA_character_]
    unique_queries[, spatial_distance_km__ := NA_real_]

    for (candidate_key in unique(unique_queries$candidate_pfts__)) {
      candidate_pfts <- strsplit(
        candidate_key,
        "\u001f",
        fixed = TRUE
      )[[1L]]
      query_rows <- which(unique_queries$candidate_pfts__ == candidate_key)
      reference_use <- reference_dt[
        reference_pft__ %in% candidate_pfts
      ]

      missing_reference_pfts <- setdiff(
        candidate_pfts,
        unique(reference_use$reference_pft__)
      )
      if (length(missing_reference_pfts) > 0L) {
        stop(
          "pft_coordinate_map has no reference coordinates for candidate PFT(s): ",
          paste(missing_reference_pfts, collapse = ", "),
          call. = FALSE
        )
      }

      nearest <- nearest_reference(
        query_lon = unique_queries$observation_lon__[query_rows],
        query_lat = unique_queries$observation_lat__[query_rows],
        reference = reference_use
      )
      unique_queries[
        query_rows,
        `:=`(
          spatial_pft__ = reference_use$reference_pft__[
            nearest$nearest_reference_index__
          ],
          spatial_distance_km__ = nearest$pft_assignment_distance_km
        )
      ]
    }

    ambiguous_valid[
      unique_queries,
      on = .(
        observation_lat__,
        observation_lon__,
        candidate_pfts__
      ),
      `:=`(
        spatial_pft__ = i.spatial_pft__,
        spatial_distance_km__ = i.spatial_distance_km__
      )
    ]

    spatial_assignment <- ambiguous_valid[
      is.finite(spatial_distance_km__) &
        spatial_distance_km__ <= max_distance_km &
        !is.na(spatial_pft__),
      .(
        observation_row_id__,
        spatial_pft__,
        spatial_distance_km__
      )
    ]

    try_dt[
      spatial_assignment,
      on = .(observation_row_id__),
      `:=`(
        assigned_final_pft = i.spatial_pft__,
        pft_assignment_method = "nearest_candidate_pft_coordinate",
        pft_assignment_distance_km = i.spatial_distance_km__,
        pft_assignment_status = "ASSIGNED"
      )
    ]

    try_dt[
      n_candidate_pfts__ > 1L &
        pft_assignment_status == "UNASSIGNED" &
        is.finite(observation_lat__) &
        is.finite(observation_lon__),
      pft_assignment_method := "no_candidate_pft_within_max_distance"
    ]
  }

  try_dt[
    n_candidate_pfts__ > 1L &
      pft_assignment_status == "UNASSIGNED" &
      (!is.finite(observation_lat__) | !is.finite(observation_lon__)),
    pft_assignment_method := "ambiguous_species_missing_coordinates"
  ]

  ambiguous_unassigned <- try_dt[
    n_candidate_pfts__ > 1L & pft_assignment_status == "UNASSIGNED"
  ]
  if (nrow(ambiguous_unassigned) > 0L && unassigned_action == "stop") {
    stop(
      nrow(ambiguous_unassigned),
      " ambiguous-species observations could not be assigned to one PFT. ",
      "Inspect the observation_pft_unassigned audit.",
      call. = FALSE
    )
  }

  assignment_audit <- try_dt[
    ,
    .(
      n_observations = .N,
      n_species = data.table::uniqueN(species_id__)
    ),
    by = .(
      pft_assignment_status,
      pft_assignment_method,
      assigned_final_pft
    )
  ]
  data.table::setorder(
    assignment_audit,
    pft_assignment_status,
    pft_assignment_method,
    assigned_final_pft
  )

  unassigned <- data.table::copy(
    try_dt[pft_assignment_status == "UNASSIGNED"]
  )

  try_dt[
    ,
    c(
      "observation_row_id__",
      "species_id__",
      "observation_lat__",
      "observation_lon__",
      "n_candidate_pfts__",
      "candidate_pfts__",
      "sole_candidate_pft__"
    ) := NULL
  ]

  attr(try_dt, "observation_pft_assignment_audit") <- assignment_audit
  attr(try_dt, "observation_pft_unassigned") <- unassigned
  attr(try_dt, "observation_pft_assignment_configuration") <- list(
    max_distance_km = as.numeric(max_distance_km),
    unassigned_action = unassigned_action,
    observation_lat_col = observation_lat_col,
    observation_lon_col = observation_lon_col,
    used_RANN = requireNamespace("RANN", quietly = TRUE)
  )

  try_dt[]
}


prepare_single_pft_try_data_for_ma <- function(
    trydat_use_species,
    pftname,
    trait_map,
    unit_map,
    pft_species_map,
    pft_coordinate_map,
    observation_coordinate_data = NULL,
    unit_col = "UnitName",
    value_col = "StdValue",
    observation_lat_col = "Latitude",
    observation_lon_col = "Longitude",
    max_pft_distance_km = 250,
    drop_errorrisk = TRUE,
    unsupported_action = c("stop", "drop"),
    ambiguous_species_action = c("warn", "stop")
) {
  unsupported_action <- match.arg(unsupported_action)
  ambiguous_species_action <- match.arg(ambiguous_species_action)
  
  if (!is.character(trait_map) || is.null(names(trait_map))) {
    stop(
      "trait_map 必须是 named character vector。",
      call. = FALSE
    )
  }
  
  trait_names <- trimws(as.character(names(trait_map)))
  pecan_vnames <- trimws(as.character(unname(trait_map)))
  valid_trait_map <- !is.na(trait_names) & nzchar(trait_names) &
    !is.na(pecan_vnames) & nzchar(pecan_vnames)
  trait_map <- stats::setNames(
    pecan_vnames[valid_trait_map],
    trait_names[valid_trait_map]
  )
  
  if (length(trait_map) == 0L) {
    stop("trait_map 中没有有效条目。", call. = FALSE)
  }
  if (anyDuplicated(names(trait_map))) {
    stop(
      "trait_map 中 TRY TraitName 重复：",
      paste(
        unique(names(trait_map)[duplicated(names(trait_map))]),
        collapse = "; "
      ),
      call. = FALSE
    )
  }
  
  # --------------------------------------------------------------------------
  # 1. Validate unit_map against trait_map.
  # --------------------------------------------------------------------------
  unit_map_dt <- data.table::copy(data.table::as.data.table(unit_map))
  required_unit_map <- c(
    "TraitName", "pecan_vname", "canonical_unit", "conversion_id"
  )
  missing_unit_map_columns <- setdiff(
    required_unit_map,
    names(unit_map_dt)
  )
  if (length(missing_unit_map_columns) > 0L) {
    stop(
      "unit_map 缺少列：",
      paste(missing_unit_map_columns, collapse = ", "),
      call. = FALSE
    )
  }
  if (anyDuplicated(unit_map_dt$TraitName)) {
    stop("unit_map$TraitName 必须唯一。", call. = FALSE)
  }
  
  missing_unit_rules <- setdiff(
    names(trait_map),
    as.character(unit_map_dt$TraitName)
  )
  if (length(missing_unit_rules) > 0L) {
    stop(
      "unit_map 缺少以下 trait_map 条目：\n",
      paste(missing_unit_rules, collapse = "\n"),
      call. = FALSE
    )
  }
  
  unit_map_use <- data.table::copy(
    unit_map_dt[as.character(TraitName) %in% names(trait_map)]
  )
  unit_map_use[, expected_vname__ := unname(
    trait_map[as.character(TraitName)]
  )]
  
  vname_mismatch <- unit_map_use[
    is.na(expected_vname__) |
      as.character(pecan_vname) != as.character(expected_vname__)
  ]
  if (nrow(vname_mismatch) > 0L) {
    stop(
      "trait_map 与 unit_map 的 pecan_vname 不一致：\n",
      paste(
        paste0(
          vname_mismatch$TraitName,
          " | trait_map=", vname_mismatch$expected_vname__,
          " | unit_map=", vname_mismatch$pecan_vname
        ),
        collapse = "\n"
      ),
      call. = FALSE
    )
  }
  unit_map_use[, expected_vname__ := NULL]
  
  # --------------------------------------------------------------------------
  # 2. Select included species for the target PFT.
  # --------------------------------------------------------------------------
  pft_dt <- data.table::copy(data.table::as.data.table(pft_species_map))
  required_pft_columns <- c("final_pft", "try_species_id")
  missing_pft_columns <- setdiff(required_pft_columns, names(pft_dt))
  if (length(missing_pft_columns) > 0L) {
    stop(
      "pft_species_map 缺少列：",
      paste(missing_pft_columns, collapse = ", "),
      call. = FALSE
    )
  }
  
  as_include_flag <- function(x) {
    if (is.logical(x)) {
      x[is.na(x)] <- FALSE
      return(x)
    }
    tolower(trimws(as.character(x))) %in% c(
      "true", "t", "1", "yes", "y"
    )
  }
  
  pft_dt[, include_internal__ := if ("include" %in% names(pft_dt)) {
    as_include_flag(include)
  } else {
    TRUE
  }]
  
  pft_included <- pft_dt[
    include_internal__ == TRUE &
      !is.na(try_species_id) &
      nzchar(trimws(as.character(try_species_id)))
  ]
  
  target_species_table <- unique(
    pft_included[
      as.character(final_pft) == as.character(pftname),
      .(
        try_species_id = as.character(try_species_id),
        final_pft = as.character(final_pft)
      )
    ]
  )
  
  if (nrow(target_species_table) == 0L) {
    stop(
      "pft_species_map 中没有找到目标 PFT 的 included species：",
      pftname,
      call. = FALSE
    )
  }
  
  target_species_ids <- unique(target_species_table$try_species_id)
  
  species_pft_count <- pft_included[
    ,
    .(
      n_pfts = data.table::uniqueN(final_pft),
      pfts = paste(
        sort(unique(as.character(final_pft))),
        collapse = "; "
      )
    ),
    by = .(try_species_id = as.character(try_species_id))
  ]
  ambiguous_species <- species_pft_count[
    try_species_id %in% target_species_ids & n_pfts > 1L
  ]
  
  # --------------------------------------------------------------------------
  # 3. Filter candidate species, then assign every observation to one PFT.
  # --------------------------------------------------------------------------
  try_source <- data.table::copy(
    data.table::as.data.table(trydat_use_species)
  )
  required_try_columns <- c(
    "AccSpeciesID", "TraitName", value_col, unit_col
  )
  missing_try_columns <- setdiff(required_try_columns, names(try_source))
  if (length(missing_try_columns) > 0L) {
    stop(
      "trydat_use_species 缺少列：",
      paste(missing_try_columns, collapse = ", "),
      call. = FALSE
    )
  }
  
  n_input_rows <- nrow(try_source)
  try_candidate <- data.table::copy(
    try_source[
      as.character(AccSpeciesID) %in% target_species_ids &
        as.character(TraitName) %in% names(trait_map)
    ]
  )
  
  if (nrow(try_candidate) == 0L) {
    stop(
      "目标 PFT 的 species 中没有匹配 trait_map 的 TRY records。",
      call. = FALSE
    )
  }

  coordinate_columns_present <- all(
    c(observation_lat_col, observation_lon_col) %in% names(try_candidate)
  )
  if (coordinate_columns_present) {
    candidate_latitude <- suppressWarnings(as.numeric(
      as.character(try_candidate[[observation_lat_col]])
    ))
    candidate_longitude <- suppressWarnings(as.numeric(
      as.character(try_candidate[[observation_lon_col]])
    ))
    valid_coordinates_before <-
      is.finite(candidate_latitude) &
      candidate_latitude >= -90 &
      candidate_latitude <= 90 &
      is.finite(candidate_longitude) &
      candidate_longitude >= -180 &
      candidate_longitude <= 180
  } else {
    valid_coordinates_before <- rep(FALSE, nrow(try_candidate))
  }

  observation_coordinate_join_audit <- data.table::data.table(
    join_key = NA_character_,
    try_join_key = NA_character_,
    lookup_join_key = NA_character_,
    lookup_latitude_column = NA_character_,
    lookup_longitude_column = NA_character_,
    n_try_rows = nrow(try_candidate),
    n_coordinate_lookup_rows = if (is.null(observation_coordinate_data)) {
      NA_integer_
    } else {
      nrow(data.table::as.data.table(observation_coordinate_data))
    },
    n_valid_coordinates_before_join = sum(valid_coordinates_before),
    n_coordinates_filled_from_lookup = 0L,
    n_valid_coordinates_after_join = sum(valid_coordinates_before),
    n_rows_still_without_valid_coordinates = sum(!valid_coordinates_before)
  )

  if (!coordinate_columns_present && is.null(observation_coordinate_data)) {
    stop(
      "trydat_use_species does not contain Latitude/Longitude columns. ",
      "Provide observation_coordinate_data, for example ",
      "na_species_res$observation_coordinates.",
      call. = FALSE
    )
  }

  if (
    !is.null(observation_coordinate_data) &&
    (!coordinate_columns_present || any(!valid_coordinates_before))
  ) {
    try_candidate <- attach_try_observation_coordinates(
      try_data = try_candidate,
      observation_coordinate_data = observation_coordinate_data,
      observation_lat_col = observation_lat_col,
      observation_lon_col = observation_lon_col
    )
    observation_coordinate_join_audit <- attr(
      try_candidate,
      "observation_coordinate_join_audit"
    )
  }

  missing_coordinate_columns <- setdiff(
    c(observation_lat_col, observation_lon_col),
    names(try_candidate)
  )
  if (length(missing_coordinate_columns) > 0L) {
    stop(
      "Coordinate join did not create required columns: ",
      paste(missing_coordinate_columns, collapse = ", "),
      call. = FALSE
    )
  }

  try_assigned <- assign_try_observations_to_pft(
    try_data = try_candidate,
    pft_species_map = pft_species_map,
    pft_coordinate_map = pft_coordinate_map,
    species_col = "AccSpeciesID",
    observation_lat_col = observation_lat_col,
    observation_lon_col = observation_lon_col,
    max_distance_km = max_pft_distance_km,
    unassigned_action = if (ambiguous_species_action == "stop") {
      "stop"
    } else {
      "drop"
    }
  )

  observation_pft_assignment_audit <- attr(
    try_assigned,
    "observation_pft_assignment_audit"
  )
  observation_pft_unassigned <- attr(
    try_assigned,
    "observation_pft_unassigned"
  )
  observation_pft_assignment_configuration <- attr(
    try_assigned,
    "observation_pft_assignment_configuration"
  )

  if (
    nrow(observation_pft_unassigned) > 0L &&
    ambiguous_species_action == "warn"
  ) {
    warning(
      nrow(observation_pft_unassigned),
      " candidate observations could not be assigned to exactly one PFT ",
      "and were excluded. Inspect observation_pft_unassigned.",
      call. = FALSE
    )
  }

  try_filtered <- data.table::copy(
    try_assigned[
      pft_assignment_status == "ASSIGNED" &
        as.character(assigned_final_pft) == as.character(pftname)
    ]
  )

  if (nrow(try_filtered) == 0L) {
    stop(
      "No TRY observations were assigned to target PFT: ",
      pftname,
      call. = FALSE
    )
  }
  
  n_rows_before_unit_conversion <- nrow(try_filtered)
  n_species_before_unit_conversion <- data.table::uniqueN(
    try_filtered$AccSpeciesID
  )
  n_traits_before_unit_conversion <- data.table::uniqueN(
    try_filtered$TraitName
  )
  
  # ErrorRisk is an outlier-distance diagnostic, not a sampling SE.
  if (isTRUE(drop_errorrisk) && "ErrorRisk" %in% names(try_filtered)) {
    try_filtered[, ErrorRisk_original := ErrorRisk]
    try_filtered[, ErrorRisk := NULL]
  }
  
  try_filtered[, final_pft := as.character(assigned_final_pft)]
  
  # --------------------------------------------------------------------------
  # 4. Perform the actual dimensional conversion.
  # --------------------------------------------------------------------------
  try_corrected <- convert_try_units_with_unit_map(
    try_data = try_filtered,
    unit_map = unit_map_use,
    unit_col = unit_col,
    value_col = value_col,
    days_per_year = 365,
    unsupported_action = unsupported_action
  )
  
  unit_conversion_audit <- attr(
    try_corrected,
    "unit_conversion_audit"
  )
  missing_unit_records <- attr(
    try_corrected,
    "missing_unit_records"
  )
  nonfinite_value_records <- attr(
    try_corrected,
    "nonfinite_value_records"
  )
  unsupported_unit_records <- attr(
    try_corrected,
    "unsupported_unit_records"
  )
  
  try_corrected[, final_pft := as.character(pftname)]
  
  # Keep original TRY row identifiers whenever they are already unique.
  duplicated_obsdataid <- data.table::data.table()
  if ("ObsDataID" %in% names(try_corrected)) {
    duplicated_obsdataid <- try_corrected[
      duplicated(ObsDataID) | duplicated(ObsDataID, fromLast = TRUE)
    ]
    if (nrow(duplicated_obsdataid) > 0L) {
      try_corrected[, TRY_ObsDataID_original := ObsDataID]
      try_corrected[, ObsDataID := seq_len(.N)]
      warning(
        "转换后发现重复 ObsDataID；原值已保存在 ",
        "TRY_ObsDataID_original，ObsDataID 已改为逐行唯一编号。",
        call. = FALSE
      )
    }
  }
  
  present_traits <- unique(as.character(try_corrected$TraitName))
  trait_map_for_ma <- trait_map[
    names(trait_map) %in% present_traits
  ]
  
  preparation_summary <- data.table::data.table(
    final_pft = as.character(pftname),
    n_rows_in_trydat_use_species = n_input_rows,
    n_species_in_pft_map = length(target_species_ids),
    n_ambiguous_species = nrow(ambiguous_species),
    n_candidate_rows_before_spatial_assignment = nrow(try_candidate),
    n_unassigned_candidate_rows = nrow(observation_pft_unassigned),
    n_rows_before_unit_conversion = n_rows_before_unit_conversion,
    n_species_before_unit_conversion = n_species_before_unit_conversion,
    n_traits_before_unit_conversion = n_traits_before_unit_conversion,
    n_dropped_missing_unit = nrow(missing_unit_records),
    n_dropped_nonfinite_value = nrow(nonfinite_value_records),
    n_unsupported_unit = nrow(unsupported_unit_records),
    n_final_rows = nrow(try_corrected),
    n_final_species = data.table::uniqueN(try_corrected$AccSpeciesID),
    n_final_try_traits = data.table::uniqueN(try_corrected$TraitName),
    n_final_pecan_vnames = data.table::uniqueN(
      try_corrected$pecan_vname
    )
  )
  
  attr(try_corrected, "trait_map_for_ma") <- trait_map_for_ma
  attr(try_corrected, "unit_map_used") <- unit_map_use
  attr(try_corrected, "target_species") <- target_species_table
  attr(try_corrected, "ambiguous_species") <- ambiguous_species
  attr(try_corrected, "observation_pft_assignment_audit") <-
    observation_pft_assignment_audit
  attr(try_corrected, "observation_pft_unassigned") <-
    observation_pft_unassigned
  attr(try_corrected, "observation_pft_assignment_configuration") <-
    observation_pft_assignment_configuration
  attr(try_corrected, "observation_coordinate_join_audit") <-
    observation_coordinate_join_audit
  attr(try_corrected, "preparation_summary") <- preparation_summary
  attr(try_corrected, "unit_conversion_audit") <- unit_conversion_audit
  attr(try_corrected, "missing_unit_records") <- missing_unit_records
  attr(try_corrected, "nonfinite_value_records") <- nonfinite_value_records
  attr(try_corrected, "unsupported_unit_records") <- unsupported_unit_records
  attr(try_corrected, "duplicated_obsdataid") <- duplicated_obsdataid
  
  message(
    "Single-PFT TRY preparation finished for: ", pftname,
    "\nPFT species: ", length(target_species_ids),
    "\nAmbiguous species resolved observation-by-observation: ",
    nrow(ambiguous_species),
    "\nUnassigned candidate rows excluded: ",
    format(nrow(observation_pft_unassigned), big.mark = ","),
    "\nRows before unit conversion: ",
    format(n_rows_before_unit_conversion, big.mark = ","),
    "\nRows after unit conversion: ",
    format(nrow(try_corrected), big.mark = ","),
    "\nFinal species: ",
    data.table::uniqueN(try_corrected$AccSpeciesID),
    "\nFinal TRY traits: ",
    data.table::uniqueN(try_corrected$TraitName),
    "\nNo physical-range filtering was performed."
  )
  
  try_corrected[]
}


# Example, after trait_map and filtered TRY data have been created:
# unit_map <- build_try_unit_map(trait_map)
# validate_try_unit_conversion_examples()
# try_data_corrected <- convert_try_units_with_unit_map(
#   try_data = try_data,
#   unit_map = unit_map,
#   unsupported_action = "stop"
# )

# =============================================================================
# Convert PEcAn.data.remote::format_try_for_ma() output to PEcAn MA trait.data
#
# Input : one long data.frame/data.table, try_ma_long
# Output: a named list of base data.frames, one list element per vname
#
# This function does NOT:
#   - convert units;
#   - aggregate observations;
#   - generate priors;
#   - call jagify();
#   - run the meta-analysis.
# =============================================================================

make_trait_data_from_try_ma_long <- function(try_ma_long) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("需要先安装 data.table。", call. = FALSE)
  }
  
  x <- data.table::copy(data.table::as.data.table(try_ma_long))
  
  required_columns <- c(
    "vname",
    "name",
    "mean",
    "statname",
    "stat",
    "n",
    "greenhouse",
    "control",
    "site_id",
    "specie_id",
    "citation_id",
    "treatment_id",
    "cultivar_id",
    "date",
    "time"
  )
  
  missing_columns <- setdiff(required_columns, names(x))
  if (length(missing_columns) > 0L) {
    stop(
      "try_ma_long 缺少 PEcAn MA 所需列：",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }
  
  if (nrow(x) == 0L) {
    stop("try_ma_long 没有任何记录。", call. = FALSE)
  }
  
  # Trait names become names(trait.data).
  x[, vname := trimws(as.character(vname))]
  invalid_vname <- is.na(x$vname) | !nzchar(x$vname)
  if (any(invalid_vname)) {
    stop(
      "try_ma_long 中有 ",
      sum(invalid_vname),
      " 行缺少 vname；请先修正 trait_map/format_try_for_ma 输出。",
      call. = FALSE
    )
  }
  
  # mean is the observation used by the meta-analysis. Do not silently drop it.
  mean_original <- x$mean
  mean_numeric <- suppressWarnings(
    as.numeric(as.character(mean_original))
  )
  invalid_mean <- is.na(mean_numeric) | !is.finite(mean_numeric)
  if (any(invalid_mean)) {
    stop(
      "try_ma_long 中有 ",
      sum(invalid_mean),
      " 行 mean 不是有限数值；请回到 try_data 检查 StdValue。",
      call. = FALSE
    )
  }
  x[, mean := mean_numeric]
  
  # Error statistics are optional, but a supplied value must be numeric.
  stat_text <- trimws(as.character(x$stat))
  stat_supplied <- !is.na(stat_text) &
    nzchar(stat_text) &
    !tolower(stat_text) %in% c("na", "n/a", "nan", "null")
  
  stat_numeric <- suppressWarnings(
    as.numeric(as.character(x$stat))
  )
  unparsable_stat <- stat_supplied & is.na(stat_numeric)
  if (any(unparsable_stat)) {
    stop(
      "try_ma_long 中有 ",
      sum(unparsable_stat),
      " 行 stat 无法转换为数值。",
      call. = FALSE
    )
  }
  x[, stat := stat_numeric]
  
  # Non-positive or non-finite error statistics cannot be used as SE/SD/etc.
  invalid_stat <- !is.na(x$stat) &
    (!is.finite(x$stat) | x$stat <= 0)
  if (any(invalid_stat)) {
    warning(
      sum(invalid_stat),
      " 行 stat 非正数或非有限；已将 stat 和 statname 设为 NA。",
      call. = FALSE
    )
    x[
      invalid_stat,
      `:=`(
        stat = NA_real_,
        statname = NA_character_
      )
    ]
  }
  
  x[, statname := trimws(as.character(statname))]
  x[
    is.na(stat) |
      is.na(statname) |
      !nzchar(statname) |
      tolower(statname) %in% c("na", "n/a", "nan", "null"),
    statname := NA_character_
  ]
  
  missing_statname <- !is.na(x$stat) & is.na(x$statname)
  if (any(missing_statname)) {
    stop(
      "有 ",
      sum(missing_statname),
      " 行提供了 stat，但没有有效 statname；无法判断它是 SE、SD 等。",
      call. = FALSE
    )
  }
  
  # Sample size may be NA, but a supplied sample size must be a positive integer.
  n_text <- trimws(as.character(x$n))
  n_supplied <- !is.na(n_text) &
    nzchar(n_text) &
    !tolower(n_text) %in% c("na", "n/a", "nan", "null")
  
  n_numeric <- suppressWarnings(
    as.numeric(as.character(x$n))
  )
  invalid_n <- n_supplied & (
    is.na(n_numeric) |
      !is.finite(n_numeric) |
      n_numeric <= 0 |
      abs(n_numeric - round(n_numeric)) > sqrt(.Machine$double.eps)
  )
  if (any(invalid_n)) {
    stop(
      "try_ma_long 中有 ",
      sum(invalid_n),
      " 行 n 不是正整数。",
      call. = FALSE
    )
  }
  x[, n := as.integer(round(n_numeric))]
  
  # A sampling error with n = 1 is internally inconsistent. Do not invent n = 2.
  inconsistent_n <- !is.na(x$stat) & !is.na(x$n) & x$n < 2L
  if (any(inconsistent_n)) {
    stop(
      "有 ",
      sum(inconsistent_n),
      " 行同时具有误差统计量但 n < 2；请检查这些记录，函数不会自动把 n 改成 2。",
      call. = FALSE
    )
  }
  
  ma_columns <- setdiff(required_columns, "vname")
  ma_frame <- as.data.frame(x[, ..ma_columns])
  
  trait.data <- split(
    ma_frame,
    f = x$vname,
    drop = TRUE
  )
  
  trait.data <- trait.data[order(names(trait.data))]
  trait.data <- lapply(trait.data, function(one_trait) {
    rownames(one_trait) <- NULL
    one_trait
  })
  
  if (length(trait.data) == 0L) {
    stop("没有生成任何 trait.data 元素。", call. = FALSE)
  }
  
  message(
    "trait.data generated.",
    "\nTraits: ", length(trait.data),
    "\nTotal observation rows: ",
    sum(vapply(trait.data, nrow, integer(1)))
  )
  
  trait.data
}


# =============================================================================
# Example call
# =============================================================================

# trait.data <- make_trait_data_from_try_ma_long(try_ma_long)
#
# length(trait.data)
# names(trait.data)
# head(trait.data[[1]])
# sort(vapply(trait.data, nrow, integer(1)), decreasing = TRUE)
#
# save(
#   trait.data,
#   file = file.path(out_dir, "trait.data.Rdata")
# )
