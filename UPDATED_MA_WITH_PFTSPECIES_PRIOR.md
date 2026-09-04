# updated_MA_with_pftspecies_prior

This workflow extends the merged **minimum-bridge MA QC** pipeline. It is run
from RStudio and does not read pecan.xml.

## Run

Open run_prema_pecan_trait_ma_rstudio.R and edit PFT_NAME,
PFTSPECIES_RDATA, TRYDAT_USE_SPECIES_RDATA, and
PFT_COORDINATE_MAP_FILE. Then use **Source** in RStudio.

The default output directory contains pft_range_species, 1deg, and
prior_species_10pct, so old checkpoints from the share-species / row-prior
workflow are not reused.

## PFT species range

The default pft_range_species rule is species-level:

1. Read candidate species listed for PFT_NAME in pftspecies.
2. Read every valid coordinate point assigned to PFT_NAME.
3. Treat every PFT point as the center of an inclusive latitude/longitude
   rectangle with latitude +/- 1 degree and longitude +/- 1 degree.
4. Retain a candidate species if any valid TRY coordinate falls inside the
   union of those rectangles.
5. Retain a candidate species if it has no valid coordinates at all.
6. Exclude a species that has valid coordinates but all are outside the union.
7. Once a species is eligible, retain all its TRY trait rows, including its
   rows without coordinates.

This is not a 1-degree grid-cell lookup and is not a kilometre radius. Longitude
matching is circular at +/-180 degrees.

An optional observation-coordinate table can fill missing TRY coordinates.
Without one, the workflow uses Latitude and Longitude in the TRY RData. The
no-coordinate fallback is visible in the audit; it is not treated as spatial
evidence.

Main spatial audit files:

- 01_canonical_try/pft_species_range_audit.csv
- 01_canonical_try/pft_reference_points_used.csv
- 01_canonical_try/pft_selection_audit.csv
- 01_canonical_try/observation_coordinate_join_audit.csv

## Prior species rule

The split is performed independently for every PEcAn target, using unique
species_key values rather than observation rows:

| Usable species for one target | Prior rule | Likelihood rule |
|---:|---|---|
| 1-2 | no random holdout; use one median per available species for a broad empirical prior | retain all species |
| 3-19 | randomly hold out 1 species | remove that species from the likelihood |
| 20 or more | hold out ceiling(10% * n_species) | remove held-out species from the likelihood |

The seed makes the split reproducible. Every selected prior species is first
reduced to one median target value, so species with many observations do not
receive more prior weight. For targets with one or two species, prior and
likelihood overlap by design because the user requested no holdout; this is
marked as ALL_SPECIES_PRIOR_NO_HOLDOUT_LE2.

Main prior audit files:

- 02_prema_pecan_observations/prior_species_split_audit.csv
- 02_prema_pecan_observations/prior_species_assignments.csv
- 02_prema_pecan_observations/prior_species_values.csv
- 02_prema_pecan_observations/prior_species_source_observations.csv
- 02_prema_pecan_observations/prior_parameter_audit.csv

prema_pecan_target_observations.rds contains the likelihood observations after
the holdout. The unsplit target observations are saved separately as
prema_pecan_target_observations_all.rds.

## MA, QC, and export

- MA still runs with random = TRUE.
- The post-MA policy remains classification_policy = "minimum_bridge".
- Only a missing/unusable beta.o posterior or a physical-domain violation
  blocks a target. Other diagnostics remain recorded but do not block it.
- PEcAn/SIPNET-readable 21-column samples remain under
  05_pecan_samples/samples.Rdata.

The pipeline bundle exposes the two new audits directly:

    View(prema_result$pft_species_range_audit)
    View(prema_result$prior_species_audit)

## Regression tests

    Rscript tests/test_pft_range_species_and_prior_split.R
    Rscript tests/test_minimum_bridge_ma_qc.R
