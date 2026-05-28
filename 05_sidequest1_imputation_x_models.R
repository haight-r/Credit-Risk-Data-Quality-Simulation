# ============================================================================
# 05 SideQuest 1: Imputation Comparison x Model Type
# ============================================================================

# SQ1: Does the imputation method matter for coefficient recovery?
#
# Compares three imputation strategies — RR regression, median, and MICE —
# on their ability to recover the DGP's true betas after logistic regression.
#
# Scope: All 10 impairment conditions (5 types × 2 severities).
# Missingness conditions (MCAR, MAR, MNAR) generate NAs directly.
# Noise and implausible conditions also generate NAs indirectly —
# domain_to_na() converts impossible values (e.g., negative interest
# coverage from noise, negative firm_age from implausible) to NA before
# imputation. Since the same preparation pipeline runs uniformly on
# every dataset, all conditions are relevant for comparing how the
# imputation method affects downstream coefficient recovery.
#
# Assumes the following objects exist in the environment (from the main
# walkthrough Rmd):
#   - dat               (clean dataset)
#   - result            (DGP output list: $scaling, $true_betas, etc.)
#   - pd_formula        (model formula)
#   - impaired_list     (named list of all 10 impaired datasets)
#   - prep_regr_list    (regression-imputed prepared datasets, all 10)
#   - prepare_for_modeling()  (from the main Rmd)
#   - fit_logistic_models()   (from the main Rmd)
#
# The main walkthrough only prepares median/MICE for missingness
# conditions. This script prepares the noise and implausible conditions
# under median and MICE on the fly if they're not already available.
#
# Output: coefficient recovery comparison across methods, with summary
# tables and visualizations.
# ============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)

# ============================================================================
# STEP 0: Ensure all 10 conditions have median and MICE prepared datasets
# ============================================================================
# The main walkthrough prepares all 10 conditions under regression, but
# only the 6 missingness conditions under median and MICE. Here we fill
# in the gaps: noise and implausible conditions get prepared with median
# and MICE so the comparison covers the full grid.
#
# Why this matters: domain_to_na() runs before imputation for ALL
# conditions. For implausible values, it converts impossible entries
# (negative firm_age, out-of-range crefo, etc.) to NA — which then get
# filled differently depending on the imputation method. For noise,
# extreme added noise can push values below zero, triggering the same
# domain check. The preparation pipeline is uniform by design; the
# imputation method choice is the only thing that varies here.

all_conditions <- c("mcar_mild", "mcar_severe",
                    "mar_mild",  "mar_severe",
                    "mnar_mild", "mnar_severe",
                    "noise_mild", "noise_severe",
                    "impl_mild", "impl_severe")

# --- Build full median list ------------------------------------------------
# Start from whatever the walkthrough already prepared, then fill gaps
if (exists("median_list")) {
  median_full <- median_list
} else {
  median_full <- list()
}

missing_from_median <- setdiff(all_conditions, names(median_full))
if (length(missing_from_median) > 0) {
  cat("Preparing median imputation for:",
      paste(missing_from_median, collapse = ", "), "\n")
  for (cond in missing_from_median) {
    median_full[[cond]] <- prepare_for_modeling(
      impaired_list[[cond]], result$scaling, method = "median"
    )
  }
}

# --- Build full MICE list --------------------------------------------------
if (exists("mice_list")) {
  mice_full <- mice_list
} else {
  mice_full <- list()
}

missing_from_mice <- setdiff(all_conditions, names(mice_full))
if (length(missing_from_mice) > 0) {
  cat("Preparing MICE imputation for:",
      paste(missing_from_mice, collapse = ", "), "\n")
  for (cond in missing_from_mice) {
    mice_full[[cond]] <- prepare_for_modeling(
      impaired_list[[cond]], result$scaling, method = "mice"
    )
  }
}

# --- Regression list is already complete from the walkthrough ---------------
regr_full <- prep_regr_list[all_conditions]

# Sanity check: all three prepared lists cover all 10 conditions
stopifnot(
  all(all_conditions %in% names(regr_full)),
  all(all_conditions %in% names(median_full)),
  all(all_conditions %in% names(mice_full))
)

cat("SQ1: Comparing imputation methods across", length(all_conditions),
    "conditions (5 types × 2 severities).\n")


# ============================================================================
# STEP 1: Fit logistic regression under each imputation method
# ============================================================================
# We reuse fit_logistic_models() three times, each time passing a different
# prepared_list. The impaired_list stays the same (raw impaired data with
# NAs → glm does complete-case analysis). The "clean" baseline also stays
# the same — it's the shared reference point.

cat("\n--- Fitting models: Regression imputation ---\n")
glm_regr <- fit_logistic_models(
  clean_data    = dat,
  impaired_list = impaired_list,
  prepared_list = regr_full,
  formula       = pd_formula,
  true_betas    = result$true_betas
)

cat("\n--- Fitting models: Median imputation ---\n")
glm_median <- fit_logistic_models(
  clean_data    = dat,
  impaired_list = impaired_list,
  prepared_list = median_full,
  formula       = pd_formula,
  true_betas    = result$true_betas
)

cat("\n--- Fitting models: MICE imputation ---\n")
glm_mice <- fit_logistic_models(
  clean_data    = dat,
  impaired_list = impaired_list,
  prepared_list = mice_full,
  formula       = pd_formula,
  true_betas    = result$true_betas
)


# ============================================================================
# STEP 2: Stack coefficient recovery tables with a method label
# ============================================================================
# Each fit_logistic_models() call produces identical clean and impaired rows
# (same data, same fit). We keep one copy of those and tag the prepared rows
# by method.

tag_method <- function(coef_df, method_label) {
  coef_df$method <- ifelse(coef_df$stage == "prepared", method_label, coef_df$stage)
  coef_df
}

coef_regr   <- tag_method(glm_regr$coef_recovery,   "regression")
coef_median <- tag_method(glm_median$coef_recovery,  "median")
coef_mice   <- tag_method(glm_mice$coef_recovery,    "mice")

# The clean + impaired rows are identical across all three calls.
# Keep them once (from the regression run), then append the three
# sets of prepared rows.
baseline_rows <- coef_regr[coef_regr$stage %in% c("clean", "impaired"), ]
prepared_rows <- rbind(
  coef_regr[coef_regr$stage == "prepared", ],
  coef_median[coef_median$stage == "prepared", ],
  coef_mice[coef_mice$stage == "prepared", ]
)

sq1_coefs <- rbind(baseline_rows, prepared_rows)
rownames(sq1_coefs) <- NULL

# Factor ordering for plots
sq1_coefs$method <- factor(sq1_coefs$method,
                           levels = c("clean", "impaired",
                                      "regression", "median", "mice"))

sq1_coefs$condition <- factor(sq1_coefs$condition,
                              levels = c("clean", all_conditions))

cat("\nCoefficient recovery table has", nrow(sq1_coefs), "rows.\n")


# ============================================================================
# STEP 3: Summary table — mean absolute beta deviation by method × condition
# ============================================================================
# This is the headline metric: averaged across all 8 betas, how far does
# each method land from truth?

sq1_summary <- sq1_coefs %>%
  filter(method != "clean") %>%   # clean is the benchmark, not a comparison
  group_by(method, condition) %>%
  summarise(
    mean_abs_diff = mean(abs_diff),
    max_abs_diff  = max(abs_diff),
    n_obs         = first(n_obs),     # same for all terms within a fit
    .groups       = "drop"
  ) %>%
  arrange(condition, method)

cat("\n=== SQ1: Mean Absolute Beta Deviation by Method × Condition ===\n")
print(as.data.frame(sq1_summary), row.names = FALSE)

# Also: collapsed across conditions (overall method ranking)
sq1_overall <- sq1_coefs %>%
  filter(!(method %in% c("clean", "impaired"))) %>%
  group_by(method) %>%
  summarise(
    mean_abs_diff = mean(abs_diff),
    max_abs_diff  = max(abs_diff),
    .groups       = "drop"
  ) %>%
  arrange(mean_abs_diff)

cat("\n=== SQ1: Overall Method Ranking (lower = better) ===\n")
print(as.data.frame(sq1_overall), row.names = FALSE)


# ============================================================================
# STEP 4: Per-beta breakdown — which coefficients does each method
#         recover best/worst?
# ============================================================================

sq1_by_beta <- sq1_coefs %>%
  filter(!(method %in% c("clean", "impaired"))) %>%
  group_by(method, term) %>%
  summarise(
    mean_abs_diff = mean(abs_diff),
    .groups       = "drop"
  ) %>%
  pivot_wider(names_from = method, values_from = mean_abs_diff)

cat("\n=== SQ1: Mean |beta - truth| by Term × Method ===\n")
print(as.data.frame(sq1_by_beta), row.names = FALSE)


# ============================================================================
# STEP 5: Visualization — Coefficient recovery dot plot
# ============================================================================
# Same layout as the main walkthrough's recovery plot, but the shape/colour
# dimension is now imputation method rather than stage.

# --- Plot 1: Dot plot of estimated betas, faceted by term ----------------

sq1_plot_data <- sq1_coefs %>%
  filter(method != "clean")   # clean reference shown as dashed line

p1 <- ggplot(sq1_plot_data,
             aes(x = estimated, y = condition, colour = method, shape = method)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5)) +
  geom_vline(aes(xintercept = true), linetype = "dashed", colour = "grey40") +
  facet_wrap(~ term, scales = "free_x", ncol = 4) +
  scale_colour_manual(
    values = c(impaired   = "#e74c3c",
               regression = "#3498db",
               median     = "#f39c12",
               mice       = "#2ecc71")
  ) +
  scale_shape_manual(
    values = c(impaired = 16, regression = 15, median = 17, mice = 18)
  ) +
  labs(
    title    = "SQ1: Coefficient Recovery by Imputation Method",
    subtitle = "Dashed line = DGP truth | All 10 impairment conditions",
    x = "Estimated coefficient", y = NULL,
    colour = "Method", shape = "Method"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "bottom")

print(p1)


# --- Plot 2: Bar chart of mean |deviation| by method × mechanism ---------

sq1_by_mechanism <- sq1_coefs %>%
  filter(!(method %in% c("clean", "impaired"))) %>%
  group_by(method, type, severity) %>%
  summarise(mean_abs_diff = mean(abs_diff), .groups = "drop")

p2 <- ggplot(sq1_by_mechanism,
             aes(x = method, y = mean_abs_diff, fill = method)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(mean_abs_diff, 4)), vjust = -0.5, size = 2.5) +
  facet_grid(severity ~ type) +
  scale_fill_manual(
    values = c(regression = "#3498db", median = "#f39c12", mice = "#2ecc71")
  ) +
  labs(
    title    = "SQ1: Mean Absolute Beta Deviation by Mechanism × Method",
    subtitle = "Lower = closer to DGP truth",
    x = NULL, y = "Mean |estimated − true|"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

print(p2)


# --- Plot 3: Heatmap of per-beta recovery by method ----------------------

sq1_heat_data <- sq1_coefs %>%
  filter(!(method %in% c("clean", "impaired"))) %>%
  group_by(method, term) %>%
  summarise(mean_abs_diff = mean(abs_diff), .groups = "drop")

p3 <- ggplot(sq1_heat_data,
             aes(x = method, y = term, fill = mean_abs_diff)) +
  geom_tile() +
  geom_text(aes(label = round(mean_abs_diff, 4)), size = 3) +
  scale_fill_gradient(low = "#d4edda", high = "#f5c6cb",
                      name = "Mean |diff|") +
  labs(
    title    = "SQ1: Beta Recovery Heatmap by Term × Method",
    subtitle = "Green = closer to truth | Red = further from truth",
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p3)


# ============================================================================
# STEP 6: Focused comparisons — conditions where method choice matters most
# ============================================================================

# --- 6a. MAR-severe -------------------------------------------------------
# Regression imputation inflates collinearity between crefo and
# debt_to_equity because it uses the model's covariate structure to
# impute crefo. Does median or MICE sidestep this?

cat("\n=== SQ1: MAR-Severe Beta Recovery (detailed) ===\n")
mar_severe_detail <- sq1_coefs %>%
  filter(condition == "mar_severe",
         method %in% c("regression", "median", "mice")) %>%
  select(method, term, true, estimated, diff, abs_diff) %>%
  arrange(term, method)

print(as.data.frame(mar_severe_detail), row.names = FALSE)

# --- 6b. Implausible-severe ------------------------------------------------
# domain_to_na() converts many corrupted values to NA. The three methods
# then fill those gaps differently. With ~28% of rows corrupted across
# four variables, this is a heavy imputation burden — method differences
# should be visible here.

cat("\n=== SQ1: Implausible-Severe Beta Recovery (detailed) ===\n")
impl_severe_detail <- sq1_coefs %>%
  filter(condition == "impl_severe",
         method %in% c("regression", "median", "mice")) %>%
  select(method, term, true, estimated, diff, abs_diff) %>%
  arrange(term, method)

print(as.data.frame(impl_severe_detail), row.names = FALSE)


# ============================================================================
# DONE
# ============================================================================
cat("\n✓ SQ1 complete. Objects in environment:\n")
cat("  sq1_coefs       — full coefficient recovery table with method labels\n")
cat("  sq1_summary     — mean |deviation| by method × condition\n")
cat("  sq1_overall     — overall method ranking\n")
cat("  sq1_by_beta     — per-term breakdown by method\n")
cat("  p1, p2, p3      — ggplot objects\n")
