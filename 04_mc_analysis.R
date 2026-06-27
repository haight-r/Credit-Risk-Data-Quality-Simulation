# ============================================================================
# 04_mc_analysis.R
# ============================================================================
# Loads Monte Carlo output / creates tables and figures
#
# Input:  mc_results_final.rds  (from 03_monte_carlo.R)
# Output: tables/  → CSV summary tables
#         figures/ → PDF plots
#
# Dependencies: ggplot2, dplyr, tidyr
# ============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)

mc <- readRDS("mc_results_final.rds")

dir.create("tables",  showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)

cat(sprintf("Loaded MC results: %d iterations, %d metric rows, %d coef rows, %d importance rows, %d univariate rows\n",
            mc$config$N_MC,
            nrow(mc$metrics),
            nrow(mc$coefs),
            nrow(mc$importance),
            nrow(mc$univariate)))


# ============================================================================
# COLOR PALETTE & THEME
# ============================================================================
# Consistent across all figures. One color per impairment type.

type_colors <- c(
  clean       = "#2C3E50",
  MCAR        = "#3498DB",
  MAR         = "#2ECC71",
  MNAR        = "#E67E22",
  noise       = "#E74C3C",
  implausible = "#9B59B6"
)

stage_colors <- c(
  clean    = "#2C3E50",
  impaired = "#E74C3C",
  prepared = "#2ECC71"
)

severity_shapes <- c(mild = 16, severe = 17, none = 15)

theme_thesis <- theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    strip.text       = element_text(face = "bold"),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )


# ============================================================================
# TABLE 1: MAIN RESULTS — METRIC MEANS & SDs
# ============================================================================
# The centerpiece table: Gini, Brier, O/E, KS by type × severity × stage × model.

tbl_main <- mc$metrics %>%
  group_by(model_type, type, severity, stage) %>%
  summarise(
    n_iter     = n_distinct(iteration),
    gini_mean  = mean(gini),     gini_sd  = sd(gini),
    brier_mean = mean(brier),    brier_sd = sd(brier),
    oe_mean    = mean(oe_ratio), oe_sd    = sd(oe_ratio),
    ks_mean    = mean(ks),       ks_sd    = sd(ks),
    .groups    = "drop"
  ) %>%
  arrange(model_type, type, severity, desc(stage))

write.csv(tbl_main, "tables/table1_main_results.csv", row.names = FALSE)
cat("Saved: tables/table1_main_results.csv\n")


# ============================================================================
# TABLE 2: DEGRADATION FROM CLEAN BASELINE
# ============================================================================
# How much does each condition lose relative to that iteration's clean model?
# Computed per-iteration first (paired), then summarised.

clean_baseline <- mc$metrics %>%
  filter(stage == "clean") %>%
  select(iteration, model_type, gini_clean = gini, brier_clean = brier,
         oe_clean = oe_ratio, ks_clean = ks)

tbl_delta <- mc$metrics %>%
  filter(stage != "clean") %>%
  left_join(clean_baseline, by = c("iteration", "model_type")) %>%
  mutate(
    delta_gini  = gini     - gini_clean,
    delta_brier = brier    - brier_clean,
    delta_oe    = oe_ratio - oe_clean,
    delta_ks    = ks       - ks_clean
  ) %>%
  group_by(model_type, type, severity, stage) %>%
  summarise(
    dgini_mean  = mean(delta_gini),   dgini_sd  = sd(delta_gini),
    dbrier_mean = mean(delta_brier),  dbrier_sd  = sd(delta_brier),
    doe_mean    = mean(delta_oe),     doe_sd     = sd(delta_oe),
    dks_mean    = mean(delta_ks),     dks_sd     = sd(delta_ks),
    .groups     = "drop"
  ) %>%
  arrange(model_type, type, severity, desc(stage))

write.csv(tbl_delta, "tables/table2_degradation_from_clean.csv", row.names = FALSE)
cat("Saved: tables/table2_degradation_from_clean.csv\n")


# ============================================================================
# TABLE 3: ERSATZWERT RECOVERY — HOW MUCH DOES PREPARATION RECOVER?
# ============================================================================
# Compares impaired → prepared shift per iteration.

impaired_metrics <- mc$metrics %>%
  filter(stage == "impaired") %>%
  select(iteration, model_type, condition,
         gini_imp = gini, brier_imp = brier, oe_imp = oe_ratio)

prepared_metrics <- mc$metrics %>%
  filter(stage == "prepared") %>%
  select(iteration, model_type, condition,
         gini_prep = gini, brier_prep = brier, oe_prep = oe_ratio)

tbl_recovery <- impaired_metrics %>%
  inner_join(prepared_metrics, by = c("iteration", "model_type", "condition")) %>%
  inner_join(
    clean_baseline %>% select(iteration, model_type, gini_clean, brier_clean, oe_clean),
    by = c("iteration", "model_type")
  ) %>%
  mutate(
    # How much of the impairment loss does preparation recover?
    # Recovery = 1 means fully restored to clean, 0 means no improvement
    gini_loss      = gini_imp  - gini_clean,
    gini_recovered = gini_prep - gini_imp,
    brier_loss      = brier_imp  - brier_clean,
    brier_recovered = brier_prep - brier_imp,
    type     = sub("_mild$|_severe$", "", condition),
    severity = ifelse(grepl("severe", condition), "severe", "mild")
  ) %>%
  group_by(model_type, type, severity) %>%
  summarise(
    gini_loss_mean     = mean(gini_loss),
    gini_recovery_mean = mean(gini_recovered),
    brier_loss_mean     = mean(brier_loss),
    brier_recovery_mean = mean(brier_recovered),
    .groups = "drop"
  )

write.csv(tbl_recovery, "tables/table3_ersatzwert_recovery.csv", row.names = FALSE)
cat("Saved: tables/table3_ersatzwert_recovery.csv\n")


# ============================================================================
# TABLE 4: COEFFICIENT RECOVERY (GLM only)
# ============================================================================
# Mean estimated beta ± sd across iterations, by term × condition × stage.
# Uses clean-data estimates as baseline (not true DGP betas).

clean_coefs <- mc$coefs %>%
  filter(stage == "clean") %>%
  group_by(term) %>%
  summarise(clean_est_mean = mean(estimated), .groups = "drop")

tbl_coefs <- mc$coefs %>%
  group_by(stage, type, severity, term) %>%
  summarise(
    est_mean = mean(estimated),
    est_sd   = sd(estimated),
    true     = first(true),
    .groups  = "drop"
  ) %>%
  left_join(clean_coefs, by = "term") %>%
  mutate(
    diff_from_clean = est_mean - clean_est_mean
  ) %>%
  arrange(term, type, severity, desc(stage))

write.csv(tbl_coefs, "tables/table4_coefficient_recovery.csv", row.names = FALSE)
cat("Saved: tables/table4_coefficient_recovery.csv\n")


# ============================================================================
# TABLE 5: XGBOOST FEATURE IMPORTANCE
# ============================================================================

tbl_importance <- mc$importance %>%
  group_by(stage, type, severity, Feature) %>%
  summarise(
    gain_mean = mean(Gain),
    gain_sd   = sd(Gain),
    .groups   = "drop"
  ) %>%
  arrange(Feature, type, severity, desc(stage))

write.csv(tbl_importance, "tables/table5_xgboost_importance.csv", row.names = FALSE)
cat("Saved: tables/table5_xgboost_importance.csv\n")


# ============================================================================
# FIGURE 1: GINI BOXPLOTS — CLEAN vs IMPAIRED vs PREPARED
# ============================================================================
# One panel per impairment type, both severities, all three stages.

plot_data_gini <- mc$metrics %>%
  mutate(
    stage    = factor(stage, levels = c("clean", "impaired", "prepared")),
    severity = factor(severity, levels = c("none", "mild", "severe")),
    # Create a readable panel label
    panel    = ifelse(type == "clean", "clean",
                      paste0(type, " — ", severity))
  )

# Separate GLM and XGBoost

for (mt in c("glm", "xgboost")) {
  
  clean_gini_mean <- mc$metrics %>%
    filter(model_type == mt, stage == "clean") %>%
    pull(gini) %>% mean()
  
  p <- ggplot(
    plot_data_gini %>% filter(model_type == mt),
    aes(x = stage, y = gini, fill = stage)
  ) +
    geom_hline(yintercept = clean_gini_mean, linetype = "dotted", color = "grey40") +
    geom_boxplot(outlier.size = 0.5, alpha = 0.7) +
    scale_fill_manual(values = stage_colors) +
    facet_wrap(~ panel, scales = "free_x", nrow = 2) +
    labs(
      title = sprintf("Gini coefficient across MC iterations (%s)", toupper(mt)),
      subtitle = sprintf("N = %d iterations, %d firms each", mc$config$N_MC, mc$config$N_FIRMS),
      x = NULL, y = "Gini"
    ) +
    theme_thesis +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
  
  ggsave(sprintf("figures/fig1_gini_boxplots_%s.pdf", mt),
         p, width = 12, height = 7)
  cat(sprintf("Saved: figures/fig1_gini_boxplots_%s.pdf\n", mt))
}


# ============================================================================
# FIGURE 2: BRIER SCORE BOXPLOTS
# ============================================================================

for (mt in c("glm", "xgboost")) {
  
  p <- ggplot(
    plot_data_gini %>% filter(model_type == mt),
    aes(x = stage, y = brier, fill = stage)
  ) +
    geom_boxplot(outlier.size = 0.5, alpha = 0.7) +
    scale_fill_manual(values = stage_colors) +
    facet_wrap(~ panel, scales = "free_x", nrow = 2) +
    labs(
      title = sprintf("Brier score across MC iterations (%s)", toupper(mt)),
      subtitle = sprintf("N = %d iterations, %d firms each", mc$config$N_MC, mc$config$N_FIRMS),
      x = NULL, y = "Brier score"
    ) +
    theme_thesis +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
  
  ggsave(sprintf("figures/fig2_brier_boxplots_%s.pdf", mt),
         p, width = 12, height = 7)
  cat(sprintf("Saved: figures/fig2_brier_boxplots_%s.pdf\n", mt))
}


# ============================================================================
# FIGURE 3: O/E RATIO BOXPLOTS
# ============================================================================

for (mt in c("glm", "xgboost")) {
  
  p <- ggplot(
    plot_data_gini %>% filter(model_type == mt),
    aes(x = stage, y = oe_ratio, fill = stage)
  ) +
    geom_boxplot(outlier.size = 0.5, alpha = 0.7) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
    scale_fill_manual(values = stage_colors) +
    facet_wrap(~ panel, scales = "free_x", nrow = 2) +
    labs(
      title = sprintf("O/E ratio across MC iterations (%s)", toupper(mt)),
      subtitle = sprintf("Dashed line = perfect calibration (O/E = 1). N = %d iterations",
                         mc$config$N_MC),
      x = NULL, y = "O/E ratio"
    ) +
    theme_thesis +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
  
  ggsave(sprintf("figures/fig3_oe_boxplots_%s.pdf", mt),
         p, width = 12, height = 7)
  cat(sprintf("Saved: figures/fig3_oe_boxplots_%s.pdf\n", mt))
}


# ============================================================================
# FIGURE 4: GINI DEGRADATION HEATMAP
# ============================================================================
# Mean delta-Gini from clean, as a heatmap: rows = impairment, columns = stage × model.

heatmap_data <- mc$metrics %>%
  filter(stage != "clean") %>%
  left_join(clean_baseline, by = c("iteration", "model_type")) %>%
  mutate(delta_gini = gini - gini_clean) %>%
  group_by(model_type, type, severity, stage) %>%
  summarise(delta_gini = mean(delta_gini), .groups = "drop") %>%
  mutate(
    row_label = paste(type, severity),
    col_label = paste(toupper(model_type), stage)
  )

p <- ggplot(heatmap_data, aes(x = col_label, y = row_label, fill = delta_gini)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.4f", delta_gini)), size = 3) +
  scale_fill_gradient2(
    low = "#E74C3C", mid = "white", high = "#2ECC71",
    midpoint = 0, name = "Mean\n\u0394 Gini"
  ) +
  labs(
    title = "Mean Gini degradation from clean baseline",
    subtitle = "Negative = worse discrimination; per-iteration pairing",
    x = NULL, y = NULL
  ) +
  theme_thesis +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave("figures/fig4_gini_degradation_heatmap.pdf", p, width = 10, height = 6)
cat("Saved: figures/fig4_gini_degradation_heatmap.pdf\n")


# ============================================================================
# FIGURE 5: O/E DEGRADATION HEATMAP
# ============================================================================

heatmap_oe <- mc$metrics %>%
  filter(stage != "clean") %>%
  left_join(clean_baseline, by = c("iteration", "model_type")) %>%
  mutate(delta_oe = oe_ratio - oe_clean) %>%
  group_by(model_type, type, severity, stage) %>%
  summarise(delta_oe = mean(delta_oe), .groups = "drop") %>%
  mutate(
    row_label = paste(type, severity),
    col_label = paste(toupper(model_type), stage)
  )

p <- ggplot(heatmap_oe, aes(x = col_label, y = row_label, fill = delta_oe)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.4f", delta_oe)), size = 3) +
  scale_fill_gradient2(
    low = "#E74C3C", mid = "white", high = "#2ECC71",
    midpoint = 0, name = "Mean\n\u0394 O/E"
  ) +
  labs(
    title = "Mean O/E ratio shift from clean baseline",
    subtitle = "Departure from 0 = calibration distortion; per-iteration pairing",
    x = NULL, y = NULL
  ) +
  theme_thesis +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave("figures/fig5_oe_degradation_heatmap.pdf", p, width = 10, height = 6)
cat("Saved: figures/fig5_oe_degradation_heatmap.pdf\n")


# ============================================================================
# FIGURE 6: COEFFICIENT RECOVERY — CLEAN vs IMPAIRED vs PREPARED
# ============================================================================
# Per-term dot plots showing mean ± 1 SD across iterations.

coef_summary <- mc$coefs %>%
  group_by(stage, condition, type, severity, term) %>%
  summarise(
    est_mean = mean(estimated),
    est_lo   = mean(estimated) - sd(estimated),
    est_hi   = mean(estimated) + sd(estimated),
    true_val = first(true),
    .groups  = "drop"
  ) %>%
  mutate(
    stage = factor(stage, levels = c("clean", "impaired", "prepared")),
    panel = ifelse(condition == "clean", "clean",
                   paste0(type, " — ", severity))
  )

# Select key terms (skip intercept — different scale)
key_terms <- c("log_assets", "debt_to_equity", "interest_cov",
               "firm_age", "crefo")

p <- ggplot(
  coef_summary %>% filter(term %in% key_terms),
  aes(x = stage, y = est_mean, color = stage)
) +
  geom_pointrange(aes(ymin = est_lo, ymax = est_hi), size = 0.4) +
  geom_hline(aes(yintercept = true_val), linetype = "dashed",
             color = "grey50", linewidth = 0.3) +
  scale_color_manual(values = stage_colors) +
  facet_grid(term ~ panel, scales = "free_y") +
  labs(
    title = "GLM coefficient recovery across MC iterations",
    subtitle = "Points = mean estimate, bars = \u00B1 1 SD. Dashed line = true DGP beta.",
    x = NULL, y = "Estimated coefficient"
  ) +
  theme_thesis +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 7),
    strip.text.y = element_text(angle = 0)
  )

ggsave("figures/fig6_coefficient_recovery.pdf", p, width = 14, height = 10)
cat("Saved: figures/fig6_coefficient_recovery.pdf\n")


# ============================================================================
# FIGURE 7: XGBOOST FEATURE IMPORTANCE SHIFTS
# ============================================================================
# How does each feature's Gain share change from clean → impaired → prepared?

imp_summary <- mc$importance %>%
  mutate(
    stage = factor(stage, levels = c("clean", "impaired", "prepared")),
    panel = ifelse(condition == "clean", "clean",
                   paste0(type, " — ", severity))
  ) %>%
  group_by(stage, panel, Feature) %>%
  summarise(gain_mean = mean(Gain), .groups = "drop")

p <- ggplot(
  imp_summary,
  aes(x = Feature, y = gain_mean, fill = stage)
) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = stage_colors) +
  facet_wrap(~ panel, nrow = 2) +
  labs(
    title = "XGBoost feature importance (Gain) across MC iterations",
    subtitle = sprintf("N = %d iterations. Bars = mean Gain share.", mc$config$N_MC),
    x = NULL, y = "Mean Gain share"
  ) +
  theme_thesis +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

ggsave("figures/fig7_xgboost_importance.pdf", p, width = 14, height = 8)
cat("Saved: figures/fig7_xgboost_importance.pdf\n")


# ============================================================================
# FIGURE 8: GLM vs XGBOOST COMPARISON
# ============================================================================
# Paired scatter: each dot is one condition in one iteration.
# x = GLM metric, y = XGBoost metric. Above diagonal = XGBoost better.

glm_wide <- mc$metrics %>%
  filter(model_type == "glm") %>%
  select(iteration, condition, stage, gini_glm = gini, brier_glm = brier)

xgb_wide <- mc$metrics %>%
  filter(model_type == "xgboost") %>%
  select(iteration, condition, stage, gini_xgb = gini, brier_xgb = brier)

model_comp <- inner_join(glm_wide, xgb_wide,
                         by = c("iteration", "condition", "stage")) %>%
  mutate(
    type     = ifelse(condition == "clean", "clean",
                      sub("_mild$|_severe$", "", condition)),
    severity = ifelse(condition == "clean", "none",
                      ifelse(grepl("severe", condition), "severe", "mild")),
    stage    = factor(stage, levels = c("clean", "impaired", "prepared"))
  )

p <- ggplot(
  model_comp %>% filter(stage != "clean"),
  aes(x = gini_glm, y = gini_xgb, color = type, shape = severity)
) +
  geom_point(alpha = 0.3, size = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_manual(values = type_colors) +
  scale_shape_manual(values = severity_shapes) +
  facet_wrap(~ stage) +
  labs(
    title = "GLM vs XGBoost: Gini coefficient",
    subtitle = "Each dot = one condition in one iteration. Above diagonal = XGBoost better.",
    x = "GLM Gini", y = "XGBoost Gini"
  ) +
  theme_thesis

ggsave("figures/fig8_glm_vs_xgboost_gini.pdf", p, width = 10, height = 5)
cat("Saved: figures/fig8_glm_vs_xgboost_gini.pdf\n")


# ============================================================================
# TABLE 7: UNIVARIATE DISCRIMINATORY POWER (Trennschärfe)
# ============================================================================
# Mean univariate AUC per variable × impairment type × severity × stage,
# across MC iterations. Three stages: clean, impaired (pre-imputation),
# prepared (post-imputation).

tbl_univariate <- mc$univariate %>%
  group_by(variable, type, severity, stage) %>%
  summarise(
    auc_mean     = mean(auc, na.rm = TRUE),
    auc_sd       = sd(auc, na.rm = TRUE),
    gini_mean    = mean(gini, na.rm = TRUE),
    gini_sd      = sd(gini, na.rm = TRUE),
    n_complete   = mean(n_complete, na.rm = TRUE),
    n_missing    = mean(n_missing, na.rm = TRUE),
    .groups      = "drop"
  ) %>%
  arrange(variable, type, severity, match(stage, c("clean", "impaired", "prepared")))

write.csv(tbl_univariate, "tables/table7_univariate_auc.csv", row.names = FALSE)
cat("Saved: tables/table7_univariate_auc.csv\n")


# ============================================================================
# TABLE 8: UNIVARIATE AUC DELTA — IMPUTATION EFFECT PER VARIABLE
# ============================================================================
# Per-iteration paired comparison: how does imputation change each variable's
# individual discriminatory power?

uni_impaired <- mc$univariate %>%
  filter(stage == "impaired") %>%
  select(iteration, variable, condition, type, severity,
         auc_imp = auc, n_complete_imp = n_complete)

uni_prepared <- mc$univariate %>%
  filter(stage == "prepared") %>%
  select(iteration, variable, condition,
         auc_prep = auc, n_complete_prep = n_complete)

uni_clean <- mc$univariate %>%
  filter(stage == "clean") %>%
  select(iteration, variable, auc_clean = auc)

tbl_uni_delta <- uni_impaired %>%
  inner_join(uni_prepared, by = c("iteration", "variable", "condition")) %>%
  inner_join(uni_clean, by = c("iteration", "variable")) %>%
  mutate(
    delta_imp_from_clean  = auc_imp  - auc_clean,   # corruption damage
    delta_prep_from_imp   = auc_prep - auc_imp,      # imputation effect
    delta_prep_from_clean = auc_prep - auc_clean      # net effect
  ) %>%
  group_by(variable, type, severity) %>%
  summarise(
    damage_mean   = mean(delta_imp_from_clean, na.rm = TRUE),
    damage_sd     = sd(delta_imp_from_clean, na.rm = TRUE),
    repair_mean   = mean(delta_prep_from_imp, na.rm = TRUE),
    repair_sd     = sd(delta_prep_from_imp, na.rm = TRUE),
    net_mean      = mean(delta_prep_from_clean, na.rm = TRUE),
    net_sd        = sd(delta_prep_from_clean, na.rm = TRUE),
    .groups       = "drop"
  ) %>%
  arrange(variable, type, severity)

write.csv(tbl_uni_delta, "tables/table8_univariate_delta.csv", row.names = FALSE)
cat("Saved: tables/table8_univariate_delta.csv\n")


# ============================================================================
# FIGURE 9: UNIVARIATE AUC — DUMBBELL PLOT (before/after imputation)
# ============================================================================
# For each variable × condition: connected dots showing impaired → prepared AUC.
# Clean baseline shown as vertical reference line.

uni_summary <- mc$univariate %>%
  filter(stage %in% c("impaired", "prepared")) %>%
  group_by(variable, condition, type, severity, stage) %>%
  summarise(auc_mean = mean(auc, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = stage, values_from = auc_mean,
              names_prefix = "auc_")

clean_uni_baseline <- mc$univariate %>%
  filter(stage == "clean") %>%
  group_by(variable) %>%
  summarise(auc_clean = mean(auc, na.rm = TRUE), .groups = "drop")

uni_summary <- uni_summary %>%
  left_join(clean_uni_baseline, by = "variable") %>%
  mutate(
    panel = paste0(type, " — ", severity),
    # Color the segment: green if imputation helped, red if it hurt
    helped = auc_prepared >= auc_impaired
  )

p <- ggplot(uni_summary, aes(y = variable)) +
  # Clean baseline as vertical reference
  geom_point(aes(x = auc_clean), shape = 124, size = 4, color = "grey40") +
  # Segment connecting impaired → prepared
  geom_segment(aes(x = auc_impaired, xend = auc_prepared,
                   yend = variable, color = helped),
               linewidth = 0.7, show.legend = FALSE) +
  # Impaired dot (red)
  geom_point(aes(x = auc_impaired), color = "#E74C3C", size = 2) +
  # Prepared dot (green)
  geom_point(aes(x = auc_prepared), color = "#2ECC71", size = 2) +
  scale_color_manual(values = c("TRUE" = "#2ECC71", "FALSE" = "#E74C3C")) +
  facet_wrap(~ panel, ncol = 2) +
  labs(
    title = "Univariate discriminatory power: before vs after imputation",
    subtitle = "Red dot = pre-imputation (CCA), green dot = post-imputation. Grey bar = clean baseline.",
    x = "Univariate AUC", y = NULL
  ) +
  theme_thesis

ggsave("figures/fig9_univariate_auc_dumbbell.pdf", p, width = 12, height = 10)
cat("Saved: figures/fig9_univariate_auc_dumbbell.pdf\n")


# ============================================================================
# FIGURE 10: UNIVARIATE AUC HEATMAP — NET CHANGE FROM CLEAN
# ============================================================================
# Variables as rows, conditions as columns, color = mean delta AUC from clean.

uni_heatmap <- mc$univariate %>%
  filter(stage == "prepared") %>%
  inner_join(
    mc$univariate %>%
      filter(stage == "clean") %>%
      select(iteration, variable, auc_clean = auc),
    by = c("iteration", "variable")
  ) %>%
  mutate(delta_auc = auc - auc_clean) %>%
  group_by(variable, type, severity) %>%
  summarise(delta_auc = mean(delta_auc, na.rm = TRUE), .groups = "drop") %>%
  mutate(col_label = paste(type, severity))

p <- ggplot(uni_heatmap, aes(x = col_label, y = variable, fill = delta_auc)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.4f", delta_auc)), size = 3) +
  scale_fill_gradient2(
    low = "#E74C3C", mid = "white", high = "#2ECC71",
    midpoint = 0, name = "Mean\n\u0394 AUC"
  ) +
  labs(
    title = "Net univariate AUC change after full preparation pipeline",
    subtitle = "Negative = variable lost discriminatory power vs clean baseline",
    x = NULL, y = NULL
  ) +
  theme_thesis +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave("figures/fig10_univariate_auc_heatmap.pdf", p, width = 10, height = 5)
cat("Saved: figures/fig10_univariate_auc_heatmap.pdf\n")


# ============================================================================
# PAIRED SIGNIFICANCE TESTS
# ============================================================================
# Per condition: is the prepared model significantly different from clean?
# Paired by iteration (same DGP draw → same clean baseline).

cat("\n=== Paired tests: prepared vs clean (per iteration) ===\n")
cat("H0: mean(metric_prepared - metric_clean) = 0\n\n")

test_results <- mc$metrics %>%
  filter(stage %in% c("clean", "prepared")) %>%
  select(iteration, model_type, stage, condition, gini, brier, oe_ratio) %>%
  pivot_wider(
    names_from  = stage,
    values_from = c(gini, brier, oe_ratio),
    names_sep   = "_"
  ) %>%
  filter(!is.na(gini_prepared)) %>%
  group_by(model_type, condition) %>%
  summarise(
    n = n(),
    gini_diff  = mean(gini_prepared - gini_clean),
    gini_p     = tryCatch(
      t.test(gini_prepared - gini_clean)$p.value,
      error = function(e) NA_real_
    ),
    brier_diff = mean(brier_prepared - brier_clean),
    brier_p    = tryCatch(
      t.test(brier_prepared - brier_clean)$p.value,
      error = function(e) NA_real_
    ),
    oe_diff    = mean(oe_ratio_prepared - oe_ratio_clean),
    oe_p       = tryCatch(
      t.test(oe_ratio_prepared - oe_ratio_clean)$p.value,
      error = function(e) NA_real_
    ),
    .groups = "drop"
  ) %>%
  arrange(model_type, condition)

write.csv(test_results, "tables/table6_paired_tests.csv", row.names = FALSE)
cat("Saved: tables/table6_paired_tests.csv\n")

# Print a summary
print(test_results %>% select(model_type, condition, gini_diff, gini_p, brier_diff, brier_p))


# ============================================================================
# SUMMARY PRINTOUT
# ============================================================================

cat("\n\n========================================\n")
cat("ANALYSIS COMPLETE\n")
cat("========================================\n\n")
cat("Tables saved:\n")
cat("  tables/table1_main_results.csv\n")
cat("  tables/table2_degradation_from_clean.csv\n")
cat("  tables/table3_ersatzwert_recovery.csv\n")
cat("  tables/table4_coefficient_recovery.csv\n")
cat("  tables/table5_xgboost_importance.csv\n")
cat("  tables/table6_paired_tests.csv\n")
cat("  tables/table7_univariate_auc.csv\n")
cat("  tables/table8_univariate_delta.csv\n")
cat("\nFigures saved:\n")
cat("  figures/fig1_gini_boxplots_glm.pdf     + xgboost\n")
cat("  figures/fig2_brier_boxplots_glm.pdf    + xgboost\n")
cat("  figures/fig3_oe_boxplots_glm.pdf       + xgboost\n")
cat("  figures/fig4_gini_degradation_heatmap.pdf\n")
cat("  figures/fig5_oe_degradation_heatmap.pdf\n")
cat("  figures/fig6_coefficient_recovery.pdf\n")
cat("  figures/fig7_xgboost_importance.pdf\n")
cat("  figures/fig8_glm_vs_xgboost_gini.pdf\n")
cat("  figures/fig9_univariate_auc_dumbbell.pdf\n")
cat("  figures/fig10_univariate_auc_heatmap.pdf\n")
cat("\nDone.\n")