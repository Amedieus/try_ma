# =============================================================================
# PEcAn trait -> write.config.SIPNET -> SIPNET-v2 parameter unit contract
# =============================================================================
#
# Purpose
# -------
# This file is the single, machine-readable unit contract for the 54 PEcAn
# traits explicitly handled by PEcAn.SIPNET::write.config.SIPNET().  It keeps
# three different concepts separate:
#
#   1. the unit expected for the PEcAn trait value;
#   2. the unit written to sipnet.param;
#   3. the unit used internally by SIPNET after its own initialization.
#
# The contract can be exported as flat CSV, JSON records, and RDS.  The JSON
# and CSV products are intended for a front end or an external audit script.
#
# Version audited
# ---------------
# PEcAn:
#   886cd83f647c79a614fb968b87c5b6b5c50fc0e4
# SIPNET:
#   7b79c63ffd21ee28f00ec8e5701eaa3fed9942ba
#
# Important respiration convention
# --------------------------------
# stem_respiration_rate and root_respiration_rate enter PEcAn in
# umol CO2 kg-1 s-1 at a 25 C reference.  PEcAn must convert them to an
# annual rate at 0 C before writing baseVegResp/baseFineRootResp, because
# SIPNET treats the parameter-file values as annual rates and divides them by
# 365 during model initialization.
# =============================================================================


.sipnet_writer_traits_v2_contract <- c(
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


.sipnet_unit_contract_row <- function(
    order,
    group,
    pecan_trait,
    pecan_input_unit,
    sipnet_parameter,
    sipnet_file_unit,
    sipnet_internal_unit,
    transformation_class,
    writer_formula,
    required_traits = "",
    reference_conditions = "",
    expected_domain = "",
    official_unit_source = "parameters.md",
    audit_status = "OK",
    notes = "") {
  data.frame(
    order = as.integer(order),
    group = as.character(group),
    pecan_trait = as.character(pecan_trait),
    pecan_input_unit = as.character(pecan_input_unit),
    sipnet_parameter = as.character(sipnet_parameter),
    sipnet_file_unit = as.character(sipnet_file_unit),
    sipnet_internal_unit = as.character(sipnet_internal_unit),
    transformation_class = as.character(transformation_class),
    writer_formula = as.character(writer_formula),
    required_traits = as.character(required_traits),
    reference_conditions = as.character(reference_conditions),
    expected_domain = as.character(expected_domain),
    official_unit_source = as.character(official_unit_source),
    audit_status = as.character(audit_status),
    notes = as.character(notes),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}


#' Build the complete SIPNET-v2 writer unit contract
#'
#' @return A base data.frame with one row per PEcAn writer trait.  The output
#'   is intentionally flat so it can be written directly to CSV or converted
#'   to JSON records.
sipnet_writer_unit_contract_v2 <- function() {
  row <- .sipnet_unit_contract_row
  
  rows <- list(
    # -----------------------------------------------------------------------
    # Leaf carbon, photosynthesis, and water-use traits (1-14)
    # -----------------------------------------------------------------------
    row(
      1, "leaf_photosynthesis", "leafC", "percent leaf dry mass",
      "cFracLeaf", "g C g-1 leaf dry mass", "g C g-1 leaf dry mass",
      "UNIT_CONVERSION", "cFracLeaf = leafC / 100",
      expected_domain = "0 <= leafC <= 100",
      notes = "leafC is also used with SLA to calculate leafCSpWt."
    ),
    row(
      2, "leaf_photosynthesis", "SLA", "m2 leaf kg-1 leaf dry mass",
      "leafCSpWt", "g C m-2 leaf", "g C m-2 leaf",
      "DERIVED_ALGEBRAIC", "leafCSpWt = 1000 * cFracLeaf / SLA",
      required_traits = "leafC",
      expected_domain = "SLA > 0",
      notes = "SLA is also required when converting area-based Amax to mass-based aMax."
    ),
    row(
      3, "leaf_photosynthesis", "Amax", "umol CO2 m-2 leaf s-1",
      "aMax", "nmol CO2 g-1 leaf s-1", "nmol CO2 g-1 leaf s-1",
      "DERIVED_ALGEBRAIC", "aMax = unit_convert(Amax * SLA, umol kg-1 s-1, nmol g-1 s-1)",
      required_traits = "SLA",
      reference_conditions = "maximum PAR; full interception; no stress",
      expected_domain = "Amax > 0"
    ),
    row(
      4, "leaf_photosynthesis", "AmaxFrac", "fraction",
      "aMaxFrac", "unitless", "unitless",
      "IDENTITY", "aMaxFrac = AmaxFrac",
      expected_domain = "0 <= AmaxFrac <= 1"
    ),
    row(
      5, "leaf_photosynthesis", "extinction_coefficient", "ratio",
      "attenuation", "unitless", "unitless",
      "IDENTITY", "attenuation = extinction_coefficient",
      expected_domain = "extinction_coefficient >= 0"
    ),
    row(
      6, "leaf_photosynthesis", "leaf_respiration_rate_m2", "umol CO2 m-2 leaf s-1",
      "baseFolRespFrac", "unitless", "unitless",
      "DERIVED_ALGEBRAIC", "baseFolRespFrac = clamp(leaf_respiration_rate_m2 / Amax_area, 0, 1)",
      required_traits = "Amax; SLA if Amax must be recovered from aMax",
      reference_conditions = "respiration and Amax must use compatible temperature and gas-exchange definitions",
      expected_domain = "leaf_respiration_rate_m2 >= 0"
    ),
    row(
      7, "leaf_photosynthesis", "Vm_low_temp", "degree C",
      "psnTMin", "degree C", "degree C",
      "IDENTITY", "psnTMin = Vm_low_temp"
    ),
    row(
      8, "leaf_photosynthesis", "psnTOpt", "degree C",
      "psnTOpt", "degree C", "degree C",
      "IDENTITY", "psnTOpt = psnTOpt",
      expected_domain = "psnTOpt > psnTMin"
    ),
    row(
      9, "leaf_photosynthesis", "growth_resp_factor", "fraction",
      "growthRespFrac", "unitless", "unitless",
      "IDENTITY", "growthRespFrac = growth_resp_factor",
      expected_domain = "0 <= growth_resp_factor <= 1"
    ),
    row(
      10, "leaf_photosynthesis", "half_saturation_PAR", "mol photons m-2 ground day-1",
      "halfSatPar", "Einsteins m-2 ground day-1", "Einsteins m-2 ground day-1",
      "IDENTITY", "halfSatPar = half_saturation_PAR",
      expected_domain = "half_saturation_PAR > 0",
      notes = "One Einstein is one mole of photons. Ground-area and leaf-area PAR must not be mixed."
    ),
    row(
      11, "leaf_photosynthesis", "dVPDSlope", "kPa-1 by official convention",
      "dVpdSlope", "kPa-1 by official convention", "kPa-1 by official convention",
      "IDENTITY", "dVpdSlope = dVPDSlope",
      required_traits = "dVpdExp",
      expected_domain = "dVPDSlope >= 0",
      official_unit_source = "parameters.md; model equation",
      audit_status = "APPROXIMATE_DIMENSION",
      notes = "Strict dimensional unit is kPa^(-dVpdExp); official documentation simplifies it to kPa-1."
    ),
    row(
      12, "leaf_photosynthesis", "dVpdExp", "unitless exponent",
      "dVpdExp", "unitless", "unitless",
      "IDENTITY", "dVpdExp = dVpdExp",
      expected_domain = "dVpdExp >= 0",
      official_unit_source = "model equation",
      audit_status = "DOC_CONFLICT_RESOLVED",
      notes = "PEcAn trait.dictionary.csv currently gives this trait the WUE-constant unit; the exponent is dimensionless."
    ),
    row(
      13, "leaf_photosynthesis", "leaf_turnover_rate", "year-1",
      "leafTurnoverRate", "year-1", "day-1",
      "ANNUAL_TO_DAILY_IN_SIPNET", "leafTurnoverRate_file = leaf_turnover_rate; internal = file / 365",
      expected_domain = "leaf_turnover_rate >= 0"
    ),
    row(
      14, "leaf_photosynthesis", "wueConst", "mg CO2 kPa g-1 H2O",
      "wueConst", "mg CO2 kPa g-1 H2O", "mg CO2 kPa g-1 H2O",
      "IDENTITY", "wueConst = wueConst; SIPNET then calculates WUE = wueConst / VPD_kPa",
      expected_domain = "wueConst > 0",
      official_unit_source = "sipnet.c water-use equation",
      audit_status = "DOC_CONFLICT_RESOLVED",
      notes = "parameters.md labels wueConst unitless, but the SIPNET equation requires this dimensional unit. The current bridge formula 2.442 * WUE_umol_per_mmol * VPD_kPa is retained."
    ),
    
    # -----------------------------------------------------------------------
    # Autotrophic respiration, root, and allocation traits (15-24)
    # -----------------------------------------------------------------------
    row(
      15, "plant_respiration_allocation", "veg_respiration_Q10", "unitless Q10",
      "vegRespQ10", "unitless", "unitless",
      "IDENTITY", "vegRespQ10 = veg_respiration_Q10",
      expected_domain = "veg_respiration_Q10 > 0"
    ),
    row(
      16, "plant_respiration_allocation", "stem_respiration_rate", "umol CO2 kg-1 s-1",
      "baseVegResp", "g C respired g-1 plant C year-1 at 0 C", "g C respired g-1 plant C day-1 at 0 C",
      "RESPIRATION_TO_ANNUAL_RATE",
      "baseVegResp_file = stem_respiration_rate * 12.0107e-6 / 1000 * 86400 * 365 * vegRespQ10^(-25/10)",
      required_traits = "veg_respiration_Q10",
      reference_conditions = "PEcAn input at 25 C; SIPNET base rate at 0 C",
      expected_domain = "stem_respiration_rate >= 0",
      official_unit_source = "write.config.SIPNET.R; parameters.md; sipnet.c",
      audit_status = "PECAN_MUST_ANNUALIZE",
      notes = "The value passed to SIPNET must already be annual. If the observed rate is per kg tissue dry mass rather than per kg tissue C, divide by the tissue carbon fraction before annualization."
    ),
    row(
      17, "plant_respiration_allocation", "root_turnover_rate", "year-1",
      "fineRootTurnoverRate", "year-1", "day-1",
      "ANNUAL_TO_DAILY_IN_SIPNET", "fineRootTurnoverRate_file = root_turnover_rate; internal = file / 365",
      expected_domain = "root_turnover_rate >= 0",
      reference_conditions = "PEcAn root trait must represent the SIPNET fine-root pool"
    ),
    row(
      18, "plant_respiration_allocation", "fine_root_respiration_Q10", "unitless Q10",
      "fineRootQ10", "unitless", "unitless",
      "IDENTITY", "fineRootQ10 = fine_root_respiration_Q10",
      expected_domain = "fine_root_respiration_Q10 > 0"
    ),
    row(
      19, "plant_respiration_allocation", "root_respiration_rate", "umol CO2 kg-1 s-1",
      "baseFineRootResp", "g C respired g-1 fine-root C year-1 at 0 C", "g C respired g-1 fine-root C day-1 at 0 C",
      "RESPIRATION_TO_ANNUAL_RATE",
      "baseFineRootResp_file = root_respiration_rate * 12.0107e-6 / 1000 * 86400 * 365 * fineRootQ10^(-25/10)",
      required_traits = "fine_root_respiration_Q10",
      reference_conditions = "PEcAn input at 25 C; SIPNET base rate at 0 C; root trait must represent fine roots",
      expected_domain = "root_respiration_rate >= 0",
      official_unit_source = "write.config.SIPNET.R; parameters.md; sipnet.c",
      audit_status = "PECAN_MUST_ANNUALIZE",
      notes = "The value passed to SIPNET must already be annual. For TRY rates normalized by root dry mass, divide by the root carbon fraction before annualization if a carbon-pool-normalized rate is required."
    ),
    row(
      20, "plant_respiration_allocation", "coarse_root_respiration_Q10", "unitless Q10",
      "coarseRootQ10", "unitless", "unitless",
      "IDENTITY", "coarseRootQ10 = coarse_root_respiration_Q10",
      expected_domain = "coarse_root_respiration_Q10 > 0"
    ),
    row(
      21, "plant_respiration_allocation", "root_allocation_fraction", "fraction",
      "fineRootAllocation", "unitless", "unitless",
      "IDENTITY", "fineRootAllocation = root_allocation_fraction",
      required_traits = "wood_allocation_fraction; leaf_allocation_fraction",
      expected_domain = "0 <= root_allocation_fraction <= 1; three explicit allocations must sum to <= 1"
    ),
    row(
      22, "plant_respiration_allocation", "wood_allocation_fraction", "fraction",
      "woodAllocation", "unitless", "unitless",
      "IDENTITY", "woodAllocation = wood_allocation_fraction",
      required_traits = "root_allocation_fraction; leaf_allocation_fraction",
      expected_domain = "0 <= wood_allocation_fraction <= 1; three explicit allocations must sum to <= 1"
    ),
    row(
      23, "plant_respiration_allocation", "leaf_allocation_fraction", "fraction",
      "leafAllocation", "unitless", "unitless",
      "IDENTITY", "leafAllocation = leaf_allocation_fraction",
      required_traits = "root_allocation_fraction; wood_allocation_fraction",
      expected_domain = "0 <= leaf_allocation_fraction <= 1; three explicit allocations must sum to <= 1"
    ),
    row(
      24, "plant_respiration_allocation", "wood_turnover_rate", "year-1",
      "woodTurnoverRate", "year-1", "day-1",
      "ANNUAL_TO_DAILY_IN_SIPNET", "woodTurnoverRate_file = wood_turnover_rate; internal = file / 365",
      expected_domain = "wood_turnover_rate >= 0"
    ),
    
    # -----------------------------------------------------------------------
    # Soil decomposition and hydrology traits (25-37)
    # -----------------------------------------------------------------------
    row(
      25, "soil_hydrology", "soil_respiration_Q10", "unitless Q10",
      "soilRespQ10", "unitless", "unitless",
      "IDENTITY", "soilRespQ10 = soil_respiration_Q10",
      expected_domain = "soil_respiration_Q10 > 0"
    ),
    row(
      26, "soil_hydrology", "som_respiration_rate", "year-1",
      "baseSoilResp", "g C respired g-1 soil C year-1 at 0 C", "g C respired g-1 soil C day-1 at 0 C",
      "ANNUAL_TO_DAILY_IN_SIPNET", "baseSoilResp_file = som_respiration_rate; internal = file / 365",
      reference_conditions = "0 C and moisture-saturated soil",
      expected_domain = "som_respiration_rate >= 0"
    ),
    row(
      27, "soil_hydrology", "turn_over_time", "year-1",
      "litterBreakdownRate", "g C broken down g-1 litter C year-1", "g C broken down g-1 litter C day-1",
      "ANNUAL_TO_DAILY_IN_SIPNET", "litterBreakdownRate_file = turn_over_time; internal = file / 365",
      reference_conditions = "0 C and maximum soil moisture",
      expected_domain = "turn_over_time >= 0",
      notes = "Despite its name, turn_over_time is a first-order rate, not a duration in years."
    ),
    row(
      28, "soil_hydrology", "fracLitterRespired", "fraction",
      "fracLitterRespired", "unitless", "unitless",
      "IDENTITY", "fracLitterRespired = fracLitterRespired",
      expected_domain = "0 <= fracLitterRespired <= 1"
    ),
    row(
      29, "soil_hydrology", "frozenSoilEff", "fraction",
      "frozenSoilEff", "unitless", "unitless",
      "IDENTITY", "frozenSoilEff = frozenSoilEff",
      expected_domain = "0 <= frozenSoilEff <= 1"
    ),
    row(
      30, "soil_hydrology", "frozenSoilFolREff", "fraction",
      "frozenSoilFolREff", "unitless", "unitless",
      "IDENTITY", "frozenSoilFolREff = frozenSoilFolREff",
      expected_domain = "0 <= frozenSoilFolREff <= 1"
    ),
    row(
      31, "soil_hydrology", "soilWHC", "cm water",
      "soilWHC", "cm water", "cm water",
      "IDENTITY_OR_SOIL_PHYSICS_OVERRIDE", "soilWHC = soilWHC; may later be overwritten from soil_physics",
      expected_domain = "soilWHC > 0",
      notes = "write.config.SIPNET can replace this trait-derived value using settings$run$inputs$soil_physics."
    ),
    row(
      32, "soil_hydrology", "immedEvapFrac", "fraction",
      "immedEvapFrac", "unitless", "unitless",
      "IDENTITY", "immedEvapFrac = immedEvapFrac",
      expected_domain = "0 <= immedEvapFrac <= 1"
    ),
    row(
      33, "soil_hydrology", "leafWHC", "cm water",
      "leafPoolDepth", "cm water per unit leaf area", "cm water per unit leaf area",
      "RENAME_IDENTITY", "leafPoolDepth = leafWHC",
      expected_domain = "leafWHC >= 0",
      notes = "Used only when SIPNET leaf-water functionality is enabled."
    ),
    row(
      34, "soil_hydrology", "waterRemoveFrac", "day-1",
      "waterRemoveFrac", "day-1", "day-1",
      "IDENTITY", "waterRemoveFrac = waterRemoveFrac",
      expected_domain = "0 <= waterRemoveFrac <= 1"
    ),
    row(
      35, "soil_hydrology", "fastFlowFrac", "fraction",
      "fastFlowFrac", "unitless", "unitless",
      "IDENTITY", "fastFlowFrac = fastFlowFrac",
      expected_domain = "0 <= fastFlowFrac <= 1"
    ),
    row(
      36, "soil_hydrology", "rdConst", "unitless scalar",
      "rdConst", "unitless scalar", "unitless scalar",
      "IDENTITY", "rdConst = rdConst; aerodynamic resistance rd = rdConst / wind_speed",
      expected_domain = "rdConst > 0"
    ),
    row(
      37, "soil_hydrology", "water_drain_frac", "day-1",
      "waterDrainFrac", "day-1", "day-1",
      "RENAME_IDENTITY", "waterDrainFrac = water_drain_frac",
      expected_domain = "water_drain_frac >= 0",
      notes = "Values above 1 are allowed by SIPNET and represent drainage in less than one day."
    ),
    
    # -----------------------------------------------------------------------
    # Phenology, stoichiometry, nitrogen, and methane traits (38-54)
    # -----------------------------------------------------------------------
    row(
      38, "phenology_nitrogen_methane", "GDD", "degree C day",
      "gddLeafOn", "degree C day", "degree C day",
      "RENAME_IDENTITY", "gddLeafOn = GDD",
      expected_domain = "GDD >= 0"
    ),
    row(
      39, "phenology_nitrogen_methane", "leafOnReallocFrac", "fraction",
      "leafOnReallocFrac", "unitless", "unitless",
      "IDENTITY", "leafOnReallocFrac = leafOnReallocFrac",
      expected_domain = "0 <= leafOnReallocFrac <= 1",
      official_unit_source = "state.h; model-structure.md"
    ),
    row(
      40, "phenology_nitrogen_methane", "fracLeafFall", "fraction",
      "fracLeafFall", "unitless", "unitless",
      "IDENTITY", "fracLeafFall = fracLeafFall",
      expected_domain = "0 <= fracLeafFall <= 1"
    ),
    row(
      41, "phenology_nitrogen_methane", "leafGrowth", "g C m-2 ground",
      "leafGrowth", "g C m-2 ground", "g C m-2 ground",
      "IDENTITY", "leafGrowth = leafGrowth",
      expected_domain = "leafGrowth >= 0"
    ),
    row(
      42, "phenology_nitrogen_methane", "c2n_leaf", "g C g-1 N",
      "leafCN", "g C g-1 N", "g C g-1 N",
      "RENAME_IDENTITY", "leafCN = c2n_leaf",
      expected_domain = "c2n_leaf > 0",
      official_unit_source = "state.h; parameters.md"
    ),
    row(
      43, "phenology_nitrogen_methane", "c2n_wood", "g C g-1 N",
      "woodCN", "g C g-1 N", "g C g-1 N",
      "RENAME_IDENTITY", "woodCN = c2n_wood",
      expected_domain = "c2n_wood > 0",
      official_unit_source = "state.h; parameters.md"
    ),
    row(
      44, "phenology_nitrogen_methane", "c2n_fineroot", "g C g-1 N",
      "fineRootCN", "g C g-1 N", "g C g-1 N",
      "RENAME_IDENTITY", "fineRootCN = c2n_fineroot",
      expected_domain = "c2n_fineroot > 0",
      official_unit_source = "state.h; parameters.md"
    ),
    row(
      45, "phenology_nitrogen_methane", "kCN", "g C g-1 N",
      "kCN", "g C g-1 N", "g C g-1 N",
      "IDENTITY", "kCN = kCN",
      expected_domain = "kCN > 0",
      official_unit_source = "state.h; model equation",
      notes = "C:N value at which the decomposition C:N limitation is one half."
    ),
    row(
      46, "phenology_nitrogen_methane", "n_volatilization_rate", "day-1",
      "nVolatilizationFrac", "day-1", "day-1",
      "RENAME_IDENTITY", "nVolatilizationFrac = n_volatilization_rate",
      expected_domain = "0 <= n_volatilization_rate <= 1"
    ),
    row(
      47, "phenology_nitrogen_methane", "n_leaching_frac", "fraction day-1",
      "nLeachingFrac", "day-1", "day-1",
      "RENAME_IDENTITY", "nLeachingFrac = n_leaching_frac",
      expected_domain = "0 <= n_leaching_frac <= 1",
      official_unit_source = "parameters.md; state.h",
      audit_status = "DOC_CONFLICT_RESOLVED",
      notes = "PEcAn trait.dictionary.csv says unitless; SIPNET defines a fraction available for leaching per day."
    ),
    row(
      48, "phenology_nitrogen_methane", "n_fixation_frac_max", "fraction",
      "nFixationFracMax", "unitless", "unitless",
      "RENAME_IDENTITY", "nFixationFracMax = n_fixation_frac_max",
      expected_domain = "0 <= n_fixation_frac_max <= 1",
      official_unit_source = "state.h"
    ),
    row(
      49, "phenology_nitrogen_methane", "n_fix_half_sat", "g N m-2 ground",
      "halfNFixationMax", "g N m-2 ground", "g N m-2 ground",
      "RENAME_IDENTITY", "halfNFixationMax = n_fix_half_sat",
      expected_domain = "n_fix_half_sat >= 0",
      official_unit_source = "state.h"
    ),
    row(
      50, "phenology_nitrogen_methane", "f_anoxia", "soil wetness fraction",
      "fAnoxia", "unitless", "unitless",
      "RENAME_IDENTITY", "fAnoxia = f_anoxia",
      expected_domain = "0 < f_anoxia < 1"
    ),
    row(
      51, "phenology_nitrogen_methane", "anaerobic_decomp_rate", "relative fraction",
      "anaerobicDecompRate", "unitless", "unitless",
      "RENAME_IDENTITY", "anaerobicDecompRate = anaerobic_decomp_rate",
      expected_domain = "0 < anaerobic_decomp_rate <= 1"
    ),
    row(
      52, "phenology_nitrogen_methane", "anaerobic_trans_exp", "unitless exponent",
      "anaerobicTransExp", "unitless", "unitless",
      "RENAME_IDENTITY", "anaerobicTransExp = anaerobic_trans_exp",
      expected_domain = "anaerobic_trans_exp >= 1"
    ),
    row(
      53, "phenology_nitrogen_methane", "soil_methane_rate", "day-1",
      "soilMethaneRate", "day-1", "day-1",
      "RENAME_IDENTITY", "soilMethaneRate = soil_methane_rate",
      expected_domain = "0 <= soil_methane_rate < 1"
    ),
    row(
      54, "phenology_nitrogen_methane", "litter_methane_rate", "day-1",
      "litterMethaneRate", "day-1", "day-1",
      "RENAME_IDENTITY", "litterMethaneRate = litter_methane_rate",
      expected_domain = "0 <= litter_methane_rate < 1"
    )
  )
  
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  
  validate_sipnet_writer_unit_contract(out, stop_on_error = TRUE)
  out
}


#' Validate the completeness and uniqueness of a writer unit contract
#'
#' @param contract A data.frame returned by sipnet_writer_unit_contract_v2().
#' @param writer_traits Optional current writer trait vector or function.  This
#'   accommodates both representations currently present in the repository.
#' @param stop_on_error Stop immediately when the contract is invalid.
#' @return A named list containing validation results.
validate_sipnet_writer_unit_contract <- function(
    contract,
    writer_traits = NULL,
    stop_on_error = TRUE) {
  required_columns <- c(
    "order", "group", "pecan_trait", "pecan_input_unit",
    "sipnet_parameter", "sipnet_file_unit", "sipnet_internal_unit",
    "transformation_class", "writer_formula", "required_traits",
    "reference_conditions", "expected_domain", "official_unit_source",
    "audit_status", "notes"
  )
  
  errors <- character()
  
  if (!is.data.frame(contract)) {
    errors <- c(errors, "contract must be a data.frame")
  } else {
    missing_columns <- setdiff(required_columns, names(contract))
    if (length(missing_columns) > 0L) {
      errors <- c(
        errors,
        paste0("missing columns: ", paste(missing_columns, collapse = ", "))
      )
    }
    
    if (nrow(contract) != length(.sipnet_writer_traits_v2_contract)) {
      errors <- c(
        errors,
        paste0(
          "expected ", length(.sipnet_writer_traits_v2_contract),
          " rows but found ", nrow(contract)
        )
      )
    }
    
    if (anyDuplicated(contract$pecan_trait)) {
      errors <- c(errors, "pecan_trait contains duplicates")
    }
    if (anyDuplicated(contract$sipnet_parameter)) {
      errors <- c(errors, "sipnet_parameter contains duplicates")
    }
    if (anyDuplicated(contract$order)) {
      errors <- c(errors, "order contains duplicates")
    }
    
    missing_expected <- setdiff(
      .sipnet_writer_traits_v2_contract,
      as.character(contract$pecan_trait)
    )
    unexpected <- setdiff(
      as.character(contract$pecan_trait),
      .sipnet_writer_traits_v2_contract
    )
    
    if (length(missing_expected) > 0L) {
      errors <- c(
        errors,
        paste0("missing expected traits: ", paste(missing_expected, collapse = ", "))
      )
    }
    if (length(unexpected) > 0L) {
      errors <- c(
        errors,
        paste0("unexpected traits: ", paste(unexpected, collapse = ", "))
      )
    }
    
    empty_unit <- !nzchar(trimws(as.character(contract$pecan_input_unit))) |
      !nzchar(trimws(as.character(contract$sipnet_file_unit)))
    if (any(empty_unit)) {
      errors <- c(
        errors,
        paste0(
          "empty input/file unit for: ",
          paste(contract$pecan_trait[empty_unit], collapse = ", ")
        )
      )
    }
  }
  
  if (!is.null(writer_traits)) {
    writer_traits_value <- if (is.function(writer_traits)) {
      writer_traits()
    } else {
      writer_traits
    }
    
    writer_traits_value <- as.character(writer_traits_value)
    missing_from_contract <- setdiff(
      writer_traits_value,
      as.character(contract$pecan_trait)
    )
    extra_in_contract <- setdiff(
      as.character(contract$pecan_trait),
      writer_traits_value
    )
    
    if (length(missing_from_contract) > 0L) {
      errors <- c(
        errors,
        paste0(
          "current writer traits missing from contract: ",
          paste(missing_from_contract, collapse = ", ")
        )
      )
    }
    if (length(extra_in_contract) > 0L) {
      errors <- c(
        errors,
        paste0(
          "contract traits absent from current writer list: ",
          paste(extra_in_contract, collapse = ", ")
        )
      )
    }
  }
  
  valid <- length(errors) == 0L
  result <- list(
    valid = valid,
    n_rows = if (is.data.frame(contract)) nrow(contract) else NA_integer_,
    expected_n_rows = length(.sipnet_writer_traits_v2_contract),
    errors = errors
  )
  
  if (!valid && isTRUE(stop_on_error)) {
    stop(
      "Invalid SIPNET writer unit contract:\n- ",
      paste(errors, collapse = "\n- "),
      call. = FALSE
    )
  }
  
  result
}


#' Convert PEcAn mass-specific respiration to a SIPNET annual base rate
#'
#' This helper implements the convention recorded in contract rows 16 and 19.
#' It returns the annual value that should be written to sipnet.param. SIPNET
#' will divide the value by 365 internally.
#'
#' @param rate_umol_co2_kg_s Respiration in umol CO2 kg-1 s-1.
#' @param q10 Temperature Q10.
#' @param measurement_temperature_c Reference temperature of the PEcAn trait.
#' @param sipnet_base_temperature_c Base temperature expected by SIPNET.
#' @param carbon_fraction Carbon mass / tissue dry mass. Use 1 only when the
#'   input denominator is already tissue carbon rather than tissue dry mass.
#' @param days_per_year Calendar convention used by SIPNET.
#' @return Annual g C respired per g tissue C at the SIPNET base temperature.
pecan_respiration_to_sipnet_annual <- function(
    rate_umol_co2_kg_s,
    q10,
    measurement_temperature_c = 25,
    sipnet_base_temperature_c = 0,
    carbon_fraction = 1,
    days_per_year = 365) {
  values <- list(
    rate_umol_co2_kg_s = rate_umol_co2_kg_s,
    q10 = q10,
    measurement_temperature_c = measurement_temperature_c,
    sipnet_base_temperature_c = sipnet_base_temperature_c,
    carbon_fraction = carbon_fraction,
    days_per_year = days_per_year
  )
  
  nonfinite <- vapply(
    values,
    function(x) any(!is.finite(as.numeric(x))),
    logical(1)
  )
  if (any(nonfinite)) {
    stop(
      "Non-finite respiration conversion input: ",
      paste(names(nonfinite)[nonfinite], collapse = ", "),
      call. = FALSE
    )
  }
  
  if (any(as.numeric(rate_umol_co2_kg_s) < 0)) {
    stop("rate_umol_co2_kg_s must be non-negative.", call. = FALSE)
  }
  if (any(as.numeric(q10) <= 0)) {
    stop("q10 must be positive.", call. = FALSE)
  }
  if (any(as.numeric(carbon_fraction) <= 0 | as.numeric(carbon_fraction) > 1)) {
    stop("carbon_fraction must be in (0, 1].", call. = FALSE)
  }
  if (length(days_per_year) != 1L || days_per_year <= 0) {
    stop("days_per_year must be one positive value.", call. = FALSE)
  }
  
  # umol CO2 kg-1 s-1 -> g C g-1 dry mass day-1
  daily_rate_measurement_temp <-
    as.numeric(rate_umol_co2_kg_s) *
    12.0107e-6 / 1000 * 86400
  
  # Convert a dry-mass denominator to a carbon-mass denominator.
  daily_rate_per_carbon <-
    daily_rate_measurement_temp / as.numeric(carbon_fraction)
  
  # Q10 adjustment followed by conversion to the annual parameter-file rate.
  daily_rate_base_temp <-
    daily_rate_per_carbon *
    as.numeric(q10) ^ (
      (
        as.numeric(sipnet_base_temperature_c) -
          as.numeric(measurement_temperature_c)
      ) / 10
    )
  
  daily_rate_base_temp * as.numeric(days_per_year)
}


#' Export the unit contract for R and front-end consumers
#'
#' @param out_dir Output directory.
#' @param contract Unit contract data.frame.
#' @param prefix Output filename prefix.
#' @param write_csv Write a flat UTF-8 CSV.
#' @param write_json Write a JSON object with metadata and row records.
#' @param write_rds Write the exact R object.
#' @param pretty Pretty-print JSON.
#' @return Invisibly returns output paths and validation results.
export_sipnet_writer_unit_contract <- function(
    out_dir,
    contract = sipnet_writer_unit_contract_v2(),
    prefix = "sipnet_writer_unit_contract_v2",
    write_csv = TRUE,
    write_json = TRUE,
    write_rds = TRUE,
    pretty = TRUE) {
  validation <- validate_sipnet_writer_unit_contract(
    contract,
    stop_on_error = TRUE
  )
  
  if (length(out_dir) != 1L || is.na(out_dir) || !nzchar(trimws(out_dir))) {
    stop("out_dir must be one non-empty path.", call. = FALSE)
  }
  
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_dir <- normalizePath(out_dir, mustWork = TRUE)
  
  paths <- list(csv = NULL, json = NULL, rds = NULL)
  
  if (isTRUE(write_json) && !requireNamespace("jsonlite", quietly = TRUE)) {
    stop(
      "Package `jsonlite` is required for JSON export. Install it with ",
      "install.packages(\"jsonlite\"), or set write_json = FALSE.",
      call. = FALSE
    )
  }
  
  if (isTRUE(write_csv)) {
    paths$csv <- file.path(out_dir, paste0(prefix, ".csv"))
    utils::write.csv(
      contract,
      paths$csv,
      row.names = FALSE,
      na = "",
      fileEncoding = "UTF-8"
    )
  }
  
  if (isTRUE(write_json)) {
    paths$json <- file.path(out_dir, paste0(prefix, ".json"))
    
    records <- lapply(seq_len(nrow(contract)), function(index) {
      as.list(contract[index, , drop = FALSE])
    })
    
    payload <- list(
      schema_version = "1.0.0",
      model = "SIPNET",
      model_revision = "v2",
      record_count = nrow(contract),
      generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
      source_revisions = list(
        pecan = "886cd83f647c79a614fb968b87c5b6b5c50fc0e4",
        sipnet = "7b79c63ffd21ee28f00ec8e5701eaa3fed9942ba"
      ),
      sources = list(
        pecan_writer = paste0(
          "https://github.com/PecanProject/pecan/blob/",
          "886cd83f647c79a614fb968b87c5b6b5c50fc0e4/",
          "models/sipnet/R/write.configs.SIPNET.R"
        ),
        pecan_trait_dictionary = paste0(
          "https://github.com/PecanProject/pecan/blob/",
          "886cd83f647c79a614fb968b87c5b6b5c50fc0e4/",
          "base/utils/data/trait.dictionary.csv"
        ),
        sipnet_parameters = paste0(
          "https://github.com/PecanProject/sipnet/blob/",
          "7b79c63ffd21ee28f00ec8e5701eaa3fed9942ba/",
          "docs/parameters.md"
        ),
        sipnet_source = paste0(
          "https://github.com/PecanProject/sipnet/blob/",
          "7b79c63ffd21ee28f00ec8e5701eaa3fed9942ba/",
          "src/sipnet/sipnet.c"
        ),
        sipnet_state = paste0(
          "https://github.com/PecanProject/sipnet/blob/",
          "7b79c63ffd21ee28f00ec8e5701eaa3fed9942ba/",
          "src/sipnet/state.h"
        )
      ),
      records = records
    )
    
    jsonlite::write_json(
      payload,
      paths$json,
      pretty = isTRUE(pretty),
      auto_unbox = TRUE,
      na = "null",
      digits = NA
    )
  }
  
  if (isTRUE(write_rds)) {
    paths$rds <- file.path(out_dir, paste0(prefix, ".rds"))
    saveRDS(contract, paths$rds, compress = FALSE)
  }
  
  invisible(list(
    contract = contract,
    validation = validation,
    paths = paths
  ))
}


# =============================================================================
# RStudio example
# =============================================================================
#
# source("sipnet_writer_unit_contract.R")
#
# writer_unit_map <- sipnet_writer_unit_contract_v2()
# View(writer_unit_map)
#
# unit_export <- export_sipnet_writer_unit_contract(
#   out_dir = file.path(getwd(), "unit_contract")
# )
#
# unit_export$paths
# =============================================================================
