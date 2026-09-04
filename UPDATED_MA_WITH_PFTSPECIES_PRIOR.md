# updated_MA_with_pftspecies_prior

This workflow extends the merged **minimum-bridge MA QC** pipeline. It is run
from RStudio and does not read pecan.xml.

## Run

Open run_prema_pecan_trait_ma_rstudio.R and edit PFT_NAME,
PFTSPECIES_RDATA, TRYDAT_USE_SPECIES_RDATA, and
PFT_COORDINATE_MAP_FILE. Then use **Source** in RStudio.

The default output directory contains pft_range_species, 1deg, and
prior_species_10pct, so it is separate from the older share-species and
row-prior workflow outputs.

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
7. Once a species is eligible, retain all its TRY trait rows, including rows
   without coordinates and rows outside the boxes.

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
- 01_canonical_try/excluded_outside_range_rows.csv

## Prior species rule

Prior species are selected independently for every PEcAn target, using unique
species_key values rather than observation rows:

| Usable species for one target | Prior rule | Likelihood rule |
|---:|---|---|
| 1-2 | no random sample; use one median per available species | retain all species |
| 3-19 | randomly sample 1 species | retain all species |
| 20 or more | sample ceiling(10% * n_species) | retain all species |

The random seed makes selection reproducible. Every selected prior species is
first reduced to one median target value, so a species with many observations
does not receive more prior weight. Selected prior species remain in the
likelihood, matching the explicitly accepted empirical-Bayes overlap. The
overlap and every selected species are recorded in the audit.

The species split is an **anchor-species** split on target observations already
created by the bridge. A proxy target may contain a secondary component paired
from another species under the deliberately permissive secondary-source rule.
Therefore this workflow does not claim independent prior/likelihood data.

Before JAGS starts, the workflow runs PEcAn.MA::jagify on the same trait.data
and reproduces PEcAn.MA's exact jagged-Y-median prior CDF check. If necessary
it widens the prior while keeping the selected-species
location and physical support. Positive targets use lognormal priors, fractions
use beta priors, and leafC uses a species-centered uniform prior clipped to
0-100. Every adjustment is recorded. If a physically bounded prior cannot pass
the same central-CDF requirement used by PEcAn, the workflow stops before the
parallel MA with a targeted error.

Main prior audit files:

- 02_prema_pecan_observations/prior_species_split_audit.csv
- 02_prema_pecan_observations/prior_species_assignments.csv
- 02_prema_pecan_observations/prior_species_values.csv
- 02_prema_pecan_observations/prior_species_source_observations.csv
- 02_prema_pecan_observations/prior_parameter_audit.csv
- 02_prema_pecan_observations/prior_compatibility_audit.csv
- 02_prema_pecan_observations/prior_preflight_jagged_summary.csv

prema_pecan_target_observations.rds contains all target observations used by the
MA likelihood. The identical pre-prior-selection candidate table is retained as
prema_pecan_target_observations_all.rds for audit compatibility.

## MA, QC, and export

- MA still runs with random = TRUE.
- The post-MA policy remains classification_policy = "minimum_bridge".
- Only a missing/unusable beta.o posterior or a physical-domain violation
  blocks a target. Other diagnostics remain recorded but do not block it.
- PEcAn/SIPNET-readable 21-column samples remain under
  05_pecan_samples/samples.Rdata.

The pipeline bundle exposes the new audits directly:

    View(prema_result$pft_species_range_audit)
    View(prema_result$prior_species_audit)
    View(prema_result$prior_compatibility_audit)

## Regression tests

    Rscript tests/test_spatial_pft_and_priors.R
    Rscript tests/test_pft_range_species_and_prior_split.R
    Rscript tests/test_minimum_bridge_ma_qc.R
