# ============================================================================
# 04 Checks and visualizations
# ============================================================================




# ── Default Rate by Year: RR-style dual-axis chart ──────────────────

library(ggplot2)
library(dplyr)

# ── Aggregate by observation year ────────────────────────────────────
year_stats <- dat %>%
  group_by(obs_year) %>%
  summarise(
    n_obs        = n(),
    n_defaults   = sum(default),
    default_rate = mean(default),
    .groups      = "drop"
  )

# ── Scaling factor for dual axis ────────────────────────────────────
# Maps the right-axis (count) to the left-axis (rate) range
max_rate  <- max(year_stats$default_rate) * 1.15
max_count <- max(year_stats$n_obs) * 1.15
scale_factor <- max_rate / max_count

# ── Plot ────────────────────────────────────────────────────────────
ggplot(year_stats, aes(x = obs_year)) +
  # Bars: firm-year count (scaled to rate axis)
  geom_col(aes(y = n_obs * scale_factor),
           fill = "grey75", width = 0.7) +
  # Line: default rate
  geom_line(aes(y = default_rate),
            colour = "#1a3a5c", linewidth = 1.1) +
  geom_point(aes(y = default_rate),
             colour = "#1a3a5c", size = 2.2) +
  # Dual axes
  scale_y_continuous(
    name   = "Ausfallrate",
    labels = scales::percent_format(accuracy = 0.1),
    limits = c(0, max_rate),
    sec.axis = sec_axis(~ . / scale_factor,
                        name   = "Fallzahl",
                        labels = scales::comma_format())
  ) +
  scale_x_continuous(
    breaks = seq(min(year_stats$obs_year), max(year_stats$obs_year), by = 1)
  ) +
  labs(
    title = "Simulated Default Rate and Portfolio Size by Year",
    x     = "Jahr"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  )

# ── Optional: print the table too ───────────────────────────────────
cat("\n── Year-level summary ──\n")
print(year_stats, n = 30)



