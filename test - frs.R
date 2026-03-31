# ============================================================================
# FRS Usage — paste into your R Markdown walkthrough
# ============================================================================
#
#
# Placement in your .Rmd:
#   1. FRS on clean data: right after the coefficient recovery check
#      (at "*** Here: add Igl scores for the clean dataset")
#   2. FRS on impaired data: right after all impairment datasets are created
#      (after dat_impl_severe is made, before the sanity checks block)
# ============================================================================

source("compute_frs.R")


# ══════════════════════════════════════════════════════════════════════════
# 1. FRS on the clean dataset
# ══════════════════════════════════════════════════════════════════════════
# Expected: all components at or near 1.000.
# Any deviation is a DGP sanity check, not a data quality problem.

frs_clean <- compute_frs(dat)
print_frs(frs_clean)

# If you see outlier < 1 for debt_to_equity, that's expected — the gamma
# distribution in the DGP produces a right tail that IQR flags. This is
# a feature, not a bug: it shows the FRS is working and tells you that
# even "perfect" data has distributional properties the FRS picks up.


# ══════════════════════════════════════════════════════════════════════════
# 2. FRS on all impaired datasets (using pre-imputation snapshots)
# ══════════════════════════════════════════════════════════════════════════
# Now that each impair_*() function attaches attr(d, "pre_imputation"),
# compute_frs_all() automatically uses the snapshot. The FRS sees the
# data as a practitioner would: NAs still present, noise unrescaled,
# implausible values in place.

impaired_list <- list(
  mcar_mild      = dat_mcar_mild,
  mcar_severe    = dat_mcar_severe,
  mar_mild       = dat_mar_mild,
  mar_severe     = dat_mar_severe,
  mnar_mild      = dat_mnar_mild,
  mnar_severe    = dat_mnar_severe,
  noise_mild     = dat_noise_mild,
  noise_severe   = dat_noise_severe,
  impl_mild      = dat_impl_mild,
  impl_severe    = dat_impl_severe
)

frs_all <- compute_frs_all(dat, impaired_list)


# ── Quick console inspection ──────────────────────────────────────────────

# Mean FRS per condition (excluding the summary MEAN row)
cat("\n=== Mean FRS by Condition ===\n")
frs_vars <- frs_all[frs_all$var != "MEAN", ]
print(sort(tapply(frs_vars$frs, frs_vars$condition, mean), decreasing = TRUE))

# Full component breakdown for one condition (useful for debugging)
cat("\n=== Component Detail: MCAR Severe ===\n")
print(round(frs_all[frs_all$condition == "mcar_severe", ], 3))


# ── What to expect per impairment type ────────────────────────────────────
#
# MCAR / MAR / MNAR:
#   - completeness drops (NAs visible in snapshot)
#   - other components stay near 1 (damage is purely missingness)
#   - MCAR affects 3 vars; MAR/MNAR affect only interest_cov
#
# Noise:
#   - completeness = 1 (no NAs)
#   - outlier may drop slightly (noise pushes values into tails)
#   - plausibility stays near 1 unless noise pushes past domain bounds
#   - consistency may drop if noise breaks inter-variable relationships
#
# Implausible:
#   - completeness = 1 (no NAs)
#   - plausibility drops hard (negative D/E, negative age, 6-SD outliers)
#   - outlier drops (extreme values flagged)
#   - consistency may drop (implausible values break relationships)
#
# Clean:
#   - all components near 1 (sanity check for the DGP)