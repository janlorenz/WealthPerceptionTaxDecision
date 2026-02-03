library(tidyverse)
library(RColorBrewer)
library(poweRlaw)
library(ggraph)
library(igraph)
library(patchwork)
library(scales)
library(glue)

# Fig. 3.: Model trajectories with different initial wealth and different initial
# tax rates in the Tax rate/Gini phase plane.
# File: Fig03_trajectories_baseline.pdf
# BehaviorSpace: trajectories (225 runs)
# Data: simdata/WealthPerception trajectories-table.csv

df <- read_csv(
  "simdata/WealthPerception trajectories-table.csv",
  skip = 6,
  show_col_types = FALSE
) |>
  mutate(
    run = `[run number]`,
    step = `[step]`,
    initial_taxrate = `taxrate...3`,
    taxrate = `taxrate...27`,
    gini = total_gini,
    wealth_generation = fct_recode(
      wealth_generation,
      "equal" = "1"
    ),
  ) |>
  arrange(wealth_generation, initial_taxrate, step)
dfsum <- df |>
  summarize(
    gini = mean(total_gini),
    taxrate = mean(taxrate),
    .by = c("step", "wealth_generation", "initial_taxrate")
  ) |>
  arrange(wealth_generation, initial_taxrate, step)
dfinit <- df |>
  filter(step == 0) |>
  select(wealth_generation, initial_taxrate, initial_gini) |>
  summarize(
    gini = mean(initial_gini),
    taxrate = mean(initial_taxrate),
    .by = c(initial_taxrate, wealth_generation)
  )
ggplot(
  mapping = aes(
    x = taxrate,
    y = gini,
    color = wealth_generation,
    group = paste(initial_taxrate, wealth_generation)
  )
) +
  stat_density_2d(
    data = df, # |> filter(step >= 100),
    mapping = aes(x = taxrate, y = gini, fill = after_stat(density)),
    geom = "raster",
    contour = FALSE,
    show.legend = FALSE
  ) +
  geom_path(data = dfsum) +
  geom_point(data = dfinit, size = 3) +
  geom_point(data = dfsum, size = 1, alpha = 0.4) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_gradient(low = "white", high = "black") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  labs(
    #title = "Model Trajectories Tax Rate vs. Inequality, baseline parameters",
    #subtitle = " Using 3 initial tax rates times 3 different initial wealth distributions",
    x = "Tax rate",
    y = "Gini coefficient",
    color = "Initial wealth distributions",
    #caption = "Lines show trajectories averaged over 25 simulation runs.\nGray shades show system's positions of all simulation runs over all 200 time steps."
  ) +
  theme_minimal() +
  guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
  theme(
    legend.position = "bottom",
    # legend.box = "horizontal",
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.5, "cm")
  )
ggsave(
  "Figs/Fig03_trajectories_baseline.pdf",
  width = 6,
  height = 5,
  device = cairo_pdf
)

# Fig. 4: Economic outcome variables over time.
# File: Fig04_econ_dyn.pdf
# BehaviorSpace: econ-dyn-bl-all
# Data: simdata/WealthPerception econ-dyn-bl-all-table.csv
d <- read_csv(
  "simdata/WealthPerception econ-dyn-bl-all-table.csv",
  skip = 6,
  show_col_types = FALSE
) |>
  transmute(
    run = `[run number]`,
    step = `[step]`,
    gini = `gini [wealth] of turtles`,
    wealth_min = `min [wealth] of turtles`,
    wealth_max = `max [wealth] of turtles`,
    wealth_med = `median [wealth] of turtles`,
    wealth_avg = `mean [wealth] of turtles`,
    share_top10 = `wealth_fraction_top [wealth] of turtles 0.1`,
    share_top1 = `wealth_fraction_top [wealth] of turtles 0.01`,
    share_bot50 = `wealth_fraction_bottom [wealth] of turtles 0.5`,
    share_mid40 = `wealth_fraction_mid40 [wealth] of turtles`,
    tail_exp = tail_exponent,
    immobility_top20 = immobility_top20,
    immobility_bot20 = immobility_bottom20,
    lt_growth_factor = if_else(
      long_term_growth_factor == 0,
      NA_real_,
      long_term_growth_factor
    ),
  )
summary_all <- d |>
  group_by(step) |>
  summarise(
    across(
      -c(run),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        se = ~ sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))
      ),
      .names = "{.col}__{.fn}"
    ),
    .groups = "drop"
  ) |>
  pivot_longer(-step, names_to = c("variable", "stat"), names_sep = "__") |>
  pivot_wider(names_from = stat, values_from = value) |>
  mutate(
    lower = mean - 1.96 * se,
    upper = mean + 1.96 * se
  )
# Color and themes
set1_blue <- brewer.pal(3, "Set1")[2]
common_theme_legendoff <- theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.margin = unit(c(0.2, 0, 1.6, 0), "cm")
  )
common_theme <- theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "bottom",
    legend.key.size = unit(0.3, "cm"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )
figgini <- summary_all |>
  filter(variable == "gini", step >= 1) |>
  ggplot(aes(x = step, y = mean)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = set1_blue, alpha = 0.4) +
  geom_line(color = set1_blue, linewidth = 0.5) +
  common_theme_legendoff +
  labs(title = "Gini coefficient", x = "Time", y = NULL, tag = "A")
figlt_gr <- summary_all |>
  filter(variable == "lt_growth_factor", step >= 1) |>
  ggplot(aes(x = step, y = mean)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = set1_blue, alpha = 0.4) +
  geom_line(color = set1_blue, linewidth = 0.5) +
  ylim(c(1.008, 1.011)) +
  common_theme_legendoff +
  labs(title = "Long-term growth factor", x = "Time", y = NULL, tag = "C")
figwealth_shares <- summary_all |>
  filter(
    variable %in% c("share_top10", "share_mid40", "share_bot50"),
    step >= 1
  ) |>
  mutate(
    variable = factor(
      variable,
      levels = c("share_top10", "share_mid40", "share_bot50")
    )
  ) |>
  ggplot(aes(x = step, y = mean, color = variable)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = variable), alpha = 0.4) +
  geom_line(linewidth = 0.5) +
  scale_color_brewer(
    palette = "Set1",
    name = "Wealth Group",
    labels = c(
      share_top10 = "Top 10%",
      share_mid40 = "Middle 40%",
      share_bot50 = "Bottom 50%"
    )
  ) +
  scale_fill_brewer(
    palette = "Set1",
    name = "Wealth Group",
    labels = c(
      share_top10 = "Top 10%",
      share_mid40 = "Middle 40%",
      share_bot50 = "Bottom 50%"
    )
  ) +
  common_theme +
  labs(title = "Wealth shares", x = "Time", y = NULL, tag = "B")
figimmobility <- summary_all |>
  filter(variable %in% c("immobility_top20", "immobility_bot20"), step >= 10) |>
  mutate(
    variable = factor(
      variable,
      levels = c("immobility_top20", "immobility_bot20")
    )
  ) |>
  ggplot(aes(x = step, y = mean, color = variable)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = variable), alpha = 0.4) +
  geom_line(linewidth = 0.5) +
  scale_color_brewer(
    palette = "Set1",
    name = "Group",
    labels = c(
      immobility_top20 = "Top 20%",
      immobility_bot20 = "Bottom 20%"
    )
  ) +
  scale_fill_brewer(
    palette = "Set1",
    name = "Group",
    labels = c(
      immobility_top20 = "Top 20%",
      immobility_bot20 = "Bottom 20%"
    )
  ) +
  common_theme +
  labs(title = "Social immobility", x = "Time", y = NULL, tag = "D")
# Save plot
(figgini | figwealth_shares) / (figlt_gr | figimmobility)
ggsave("Figs/Fig04_econ_dyn.pdf", width = 9, height = 7, device = cairo_pdf)


# Fig. 5: Heavy-tailed distribution.
# File: Fig05_wealth_powerlaw.pdf
# BehaviorSpace: Not used, instead NetLogo function save-wealth-distr at tick 200
# Data: simdata/wealth_data_100.csv
df <- read_csv(
  "simdata/wealth_data_100.csv",
  col_names = c("run", "tick", "who", "wealth"),
  show_col_types = FALSE
)
# Create power-law object
pl <- conpl$new(df$wealth)
# Estimate xmin (threshold where power-law holds) and exponent (alpha)
est <- estimate_xmin(pl, xmins = seq(15, 17, by = 0.001))
pl$setXmin(est$xmin)
pl$setPars(est$pars)
# Set base R plot style similar to theme_minimal
par(
  bg = "white", # White background
  bty = "n", # No box around plot
  las = 1, # Horizontal axis labels
  tck = -0.01, # Minimal tick marks
  mgp = c(2, 0.6, 0), # Axis title and label spacing
  cex.axis = 0.9, # Slightly smaller axis labels
  cex.lab = 1, # Axis titles size
  cex.main = 1.1, # Main title size
  font.main = 2, # Bold title
  col.axis = "gray30", # Softer axis color
  col.lab = "gray10", # Softer label color
  family = "sans" # Use a clean sans font
)
# Plot with fitted power law
pdf("Figs/Fig05_wealth_powerlaw.pdf", width = 7, height = 5)
plot(
  pl,
  main = "CCDF of Wealth with Power-Law Fit",
  xlab = "Wealth",
  ylab = "CDF",
  cex.axis = 0.9, # axis tick label size
  cex.lab = 1, # axis label size
  cex.main = 1.1, # main title size
  col.axis = "gray30", # axis label color
  col.lab = "gray10", # axis title color
  family = "sans",
  yaxt = "n",
  xaxt = "n"
)
grid(col = "gray90", lty = "solid")
lines(pl, col = "#E41A1C", lwd = 2)
# Add nicer x-axis and y-axis (log scale but without scientific notation)
x_ticks <- c(0.1, 1, 10, 100, 1000, 10000)
axis(
  1,
  at = x_ticks,
  labels = format(x_ticks, scientific = FALSE),
  col.axis = "gray30"
)
y_ticks <- c(0.00001, 0.0001, 0.001, 0.01, 0.1, 1)
axis(
  2,
  at = y_ticks,
  labels = format(y_ticks, scientific = FALSE, drop0trailing = TRUE),
  las = 1,
  col.axis = "gray30"
)
dev.off()


# Fig. 6.: Effects of individual parameters on economic outcomes.
# File: Fig06_econ_growth.pdf
# BehaviorSpace: econ-dyn-mu, econ-dyn-sigma, econ-dyn-tax, econ-dyn-eff
# Data:
# simdata/WealthPerception econ-dyn-mu-table.csv
# simdata/WealthPerception econ-dyn-sigma-table.csv
# simdata/WealthPerception econ-dyn-tax-table.csv
# simdata/WealthPerception econ-dyn-eff-table.csv
load_and_reshape <- function(file, param_name) {
  read_csv(file, skip = 6, show_col_types = FALSE) |>
    transmute(
      run = `[run number]`,
      param_value = .data[[param_name]], # dynamically select param column
      gini = `gini [wealth] of turtles`,
      share_top10 = `wealth_fraction_top [wealth] of turtles 0.1`,
      immobility_top20 = immobility_top20,
      long_term_growth_factor = long_term_growth_factor,
      is_baseline = ifelse(
        mu == 0.01 & sigma == 0.25 & taxrate == 0.015 & efficiency == 0.95,
        TRUE,
        FALSE
      )
    ) |>
    mutate(
      parameter = param_name # label the parameter varied
    ) |>
    pivot_longer(
      cols = c(gini, share_top10, immobility_top20, long_term_growth_factor),
      names_to = "Outcome",
      values_to = "Value"
    )
}
# Load and combine all four
df_all <- bind_rows(
  load_and_reshape("simdata/WealthPerception econ-dyn-mu-table.csv", "mu"),
  load_and_reshape(
    "simdata/WealthPerception econ-dyn-sigma-table.csv",
    "sigma"
  ),
  load_and_reshape(
    "simdata/WealthPerception econ-dyn-taxrate-table.csv",
    "taxrate"
  ),
  load_and_reshape(
    "simdata/WealthPerception econ-dyn-efficiency-table.csv",
    "efficiency"
  )
)
# Strange work around to get breaks for x axis
breaks_fun <- function(x) {
  case_when(
    round(min(x), digits = 4) == 0.0002 ~ seq(0.002, 0.038, by = 0.004), # mu
    round(min(x), digits = 4) == 0.0275 ~ seq(0.05, 0.5, by = 0.05), # sigma
    round(min(x), digits = 4) == 0.5275 ~ seq(0.55, 1, by = 0.05), # efficiency
    .default = seq(0, 0.045, by = 0.005), # taxrate
    #round(min(x), digits = 4) == -0.0022 ~ seq(0, 0.045, by = 0.005), # taxrate
  )
}
# Necessary for greeks rendering in pdf
param_labels <- c(
  mu = "μ",
  sigma = "σ",
  taxrate = "τ Tax rate",
  efficiency = "𝜙 Efficiency"
)
# Plotting
df_all |>
  mutate(
    parameter = factor(
      parameter,
      levels = c("mu", "sigma", "taxrate", "efficiency")
    ),
    Outcome = factor(
      Outcome,
      levels = c(
        "gini",
        "share_top10",
        "immobility_top20",
        "long_term_growth_factor"
      ),
      labels = c(
        "Gini\ncoefficient",
        "Top 10% \nwealth share",
        "Top 20% \nImmobility",
        "Long-term \ngrowth factor"
      )
    )
  ) |>
  ggplot(aes(
    x = param_value,
    y = Value,
    fill = factor(parameter),
    color = is_baseline,
    size = is_baseline
  )) +
  facet_grid(
    Outcome ~ parameter,
    scales = "free",
    switch = "both",
    labeller = labeller(parameter = param_labels)
  ) +
  geom_point(alpha = 0.5, shape = 21) +
  geom_smooth(
    linewidth = 1,
    color = "black",
    method = 'loess',
    formula = 'y ~ x'
  ) +
  scale_color_manual(values = c("white", "black")) +
  scale_fill_brewer(palette = "Set1") +
  scale_size_manual(values = c(4, 6)) +
  scale_x_continuous(breaks = breaks_fun, minor_breaks = NULL) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 5),
    minor_breaks = NULL
  ) +
  labs(
    title = NULL,
    x = NULL,
    y = NULL
    # x = "Parameter value",
    # y = "Outcome"
  ) +
  theme_minimal(base_size = 24) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    strip.placement = "outside",
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )
ggsave(
  "Figs/Fig06_econ_growth.pdf",
  width = 12,
  height = 9,
  device = cairo_pdf
)


# Fig. 7: Effects of the tax rate on the long-term growth factor after 200 time steps.
# File: Fig07_growth.pdf
# BehaviorSpace: econ growth efficiency tax
# Data: WealthPerception econ growth efficiency tax-table.csv
df <- read_csv(
  "simdata/WealthPerception econ growth efficiency tax-table.csv",
  skip = 6,
  show_col_types = FALSE
)
df |>
  filter(efficiency %in% c(0.9, 0.95, 1)) |>
  summarize(
    median = median(long_term_growth_factor),
    mean = mean(long_term_growth_factor),
    n = n(),
    sd = sd(long_term_growth_factor),
    q1 = quantile(long_term_growth_factor, 0.25),
    q3 = quantile(long_term_growth_factor, 0.75),
    stderr = sd / sqrt(n),
    .by = c(taxrate, efficiency)
  ) |>
  mutate(
    is_baseline = ifelse(taxrate == 0.015 & efficiency == 0.95, TRUE, FALSE)
  ) |>
  ggplot(aes(x = taxrate, y = mean, color = factor(efficiency))) +
  geom_hline(yintercept = exp(0.01)) +
  geom_point(aes(size = is_baseline)) +
  geom_line() +
  geom_errorbar(
    aes(ymin = mean - 1.96 * stderr, ymax = mean + 1.96 * stderr),
    width = 0.0025,
    linewidth = 1
  ) +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    width = 0.002,
    alpha = 0.5
  ) +
  scale_x_continuous(breaks = seq(0, 0.05, 0.01)) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 5),
    minor_breaks = NULL
  ) +
  scale_color_brewer(palette = "Set2") +
  labs(
    #title = "Growth-optimal tax rate",
    x = "Tax rate (τ)",
    y = "Long-term growth factor",
    #caption = "μ = 0.01, σ = 0.25 implies expected growth factor 1.01005 under 𝜙 = 1 (black line)\nbig dot = baseline parameters, error bars show one standard deviation from the mean.\nEach point is based on 100 simulation runs at t = 200.",
  ) +
  theme_minimal() +
  guides(color = guide_legend(title = "Efficiency (𝜙)"), size = "none") +
  theme(
    axis.text.x = element_text(hjust = 0.5, size = 10)
  )
ggsave(
  "Figs/Fig07_growth.pdf",
  width = 6,
  height = 4,
  device = cairo_pdf
)

# Fig. 8: Effects of parameter combinations on inequality and social mobility
# File: Fig08_gatsby.pdf
# BehaviorSpace:
# Data:
# Load data
d <- read_csv("simdata/WealthPerception gatsby-table.csv", skip = 6) |>
  rename(
    gini = `gini [wealth] of turtles`,
    immobility_top20 = immobility_top20,
    immobility_bot20 = immobility_bottom20,
    lt_growth_factor = long_term_growth_factor,
    run = `[run number]`
  ) |>
  select(
    run,
    sigma,
    taxrate,
    gini,
    immobility_top20,
    immobility_bot20,
    lt_growth_factor
  ) |>
  group_by(sigma, taxrate) |>
  mutate(config_id = cur_group_id()) |>
  ungroup() |>
  arrange(config_id)
# Summarize data
s <- d |>
  group_by(sigma, taxrate) |>
  summarise(
    mean_gini = mean(gini),
    se_gini = sd(gini) / sqrt(n()),
    mean_immobility = mean(immobility_top20),
    se_immobility = sd(immobility_top20) / sqrt(n()),
    .groups = "drop"
  ) |>
  mutate(
    lower_gini = mean_gini - 1.96 * se_gini,
    upper_gini = mean_gini + 1.96 * se_gini,
    lower_immobility = mean_immobility - 1.96 * se_immobility,
    upper_immobility = mean_immobility + 1.96 * se_immobility
  )
# Panel C: Effect of tau and sigma on Gini
fig_gini_tau_sigma <- s |>
  ggplot(aes(
    sigma,
    mean_gini,
    color = factor(taxrate),
    fill = factor(taxrate)
  )) +
  geom_line() +
  geom_point() +
  geom_ribbon(
    aes(ymin = lower_gini, ymax = upper_gini),
    alpha = 0.2,
    color = NA
  ) +
  scale_color_viridis_d(option = "D") +
  scale_fill_viridis_d(option = "D") +
  labs(
    x = expression(sigma),
    y = "Gini coefficient",
    color = expression(tau),
    fill = expression(tau),
    tag = "C"
  ) +
  guides(color = "none", fill = "none") +
  theme_minimal(base_size = 18)
# Panel A
fig_gini_tau_sigma_2 <- s |>
  ggplot(aes(
    taxrate,
    mean_gini,
    color = factor(sigma),
    fill = factor(sigma)
  )) +
  geom_line() +
  geom_point() +
  geom_ribbon(
    aes(ymin = lower_gini, ymax = upper_gini),
    alpha = 0.2,
    color = NA
  ) +
  scale_color_viridis_d(option = "F", end = 0.8) +
  scale_fill_viridis_d(option = "F", end = 0.8) +
  labs(
    x = "Tax rate (τ)",
    y = "Gini coefficient",
    color = expression(sigma),
    fill = expression(sigma),
    tag = "A"
  ) +
  guides(color = "none", fill = "none") +
  theme_minimal(base_size = 18)
# Panel D:
fig_immobility_tau_sigma <- s |>
  ggplot(aes(
    sigma,
    mean_immobility,
    color = factor(taxrate),
    fill = factor(taxrate)
  )) +
  geom_line() +
  geom_point() +
  geom_ribbon(
    aes(ymin = lower_immobility, ymax = upper_immobility),
    alpha = 0.2,
    color = NA
  ) +
  scale_color_viridis_d(option = "D") +
  scale_fill_viridis_d(option = "D") +
  labs(
    x = expression(sigma),
    y = "Immobility Top 20%",
    tag = "D"
  ) +
  guides(color = "none", fill = "none") +
  theme_minimal(base_size = 18)
# Panel B:
fig_immobility_tau_sigma_2 <- s |>
  ggplot(aes(
    taxrate,
    mean_immobility,
    color = factor(sigma),
    fill = factor(sigma)
  )) +
  geom_line() +
  geom_point() +
  geom_ribbon(
    aes(ymin = lower_immobility, ymax = upper_immobility),
    alpha = 0.2,
    color = NA
  ) +
  scale_color_viridis_d(option = "F", end = 0.8) +
  scale_fill_viridis_d(option = "F", end = 0.8) +
  labs(
    x = "Tax rate (τ)",
    y = "Immobility Top 20%",
    tag = "B"
  ) +
  guides(color = "none", fill = "none") +
  theme_minimal(base_size = 18)
# Panel E: Gatsby with line
fig_gatsby_line <- d |>
  ggplot(aes(gini, immobility_top20, color = factor(sigma))) + #, alpha = taxrate)) +
  geom_point(shape = 16, size = 3, alpha = 0.25) +
  geom_smooth(aes(group = factor(sigma)), method = "lm", se = FALSE) +
  ylim(c(0.4, 0.9)) +
  labs(
    x = "Gini coefficient",
    y = "Immobility Top 20%",
    color = expression(sigma),
    tag = "E"
  ) +
  scale_color_viridis_d(option = "F", end = 0.8) +
  theme_minimal(base_size = 18) +
  theme(legend.position = "bottom")
# Panel F
fig_gatsby_line2 <- d |>
  ggplot(aes(gini, immobility_top20, color = factor(taxrate))) + #, alpha = sigma)) +
  geom_point(shape = 16, size = 3, alpha = 0.25) +
  geom_smooth(aes(group = factor(taxrate)), method = "lm", se = FALSE) +
  ylim(c(0.4, 0.9)) +
  labs(
    x = "Gini coefficient",
    y = "Immobility Top 20%",
    color = expression(tau),
    tag = "F"
  ) +
  scale_color_viridis_d(option = "D") +
  theme_minimal(base_size = 18) +
  theme(legend.position = "bottom")
((fig_gini_tau_sigma_2 |
  fig_immobility_tau_sigma_2 |
  fig_gini_tau_sigma |
  fig_immobility_tau_sigma) /
  (fig_gatsby_line | fig_gatsby_line2)) +
  guide_area() +
  plot_layout(nrow = 3, heights = c(1, 1.85, 0.05))
ggsave(
  "Figs/Fig08_gatsby.pdf",
  width = 14,
  height = 11,
  device = cairo_pdf
)


# Fig. 9: Example of how social dynamics shape agents’ perceptions.
# File: Fig09_perception_demo.pdf
purple_palette_function <- colorRampPalette(c("white", "magenta3", "black"))
purpal <- purple_palette_function(8)
wealth <- c(20, 13, 9, 7, 5.5, 5, 4.5, 4)
# print(purpal)
nodes_df <- tibble(
  id = 1:8,
  label = paste(c(
    'Q1 PQ1',
    'Q1 PQ2',
    'Q2 PQ3',
    'Q2 PQ2',
    'Q3 PQ3',
    'Q3 PQ2',
    'Q4 PQ3',
    'Q4 PQ4'
  )),
  fillcolor = purpal,
  quartile_group = c(1, 1, 2, 2, 3, 3, 4, 4),
  perceived_quartile_group = c(1, 2, 3, 2, 3, 2, 3, 4),
  wealth = wealth
)
edges_df <- tibble(
  from = c(1, 1, 1, 2, 2, 3, 4, 5, 5, 6, 6, 7),
  to = c(2, 3, 4, 3, 4, 6, 5, 7, 8, 7, 8, 8)
)
nodes_df <-
  bind_rows(
    edges_df,
    edges_df |> rename(from = to, to = from),
    tibble(from = 1:8, to = 1:8)
  ) |>
  left_join(nodes_df |> select(to = id, wealth), by = join_by(to)) |>
  summarize(perceived_mean = mean(wealth), .by = from) |>
  right_join(nodes_df, by = join_by("from" == "id")) |>
  rename(id = from)
my_graph <- graph_from_data_frame(
  d = edges_df,
  vertices = nodes_df,
  directed = FALSE
)
pos <- tibble(
  y = 8 - c(1, 1.9, 3, 4, 5, 6, 7, 8),
  x = c(2, 1.5, 2.4, 1.4, 2.6, 1.5, 2.5, 2)
)
Gnet <- ggraph(my_graph, layout = pos) + # Or any other layout
  geom_edge_link() +
  geom_node_point(
    aes(size = wealth, fill = fillcolor),
    shape = 21, # Shape 21 allows for both fill and color
    color = 'black'
  ) +
  geom_node_label(
    aes(
      label = paste0(
        paste("Wealth:", wealth),
        "\nQ",
        quartile_group,
        "→ Perceived Q",
        perceived_quartile_group,
        "\nPerceived mean: ",
        nodes_df$perceived_mean
      )
    ),
    size = 2,
    nudge_x = 0.1,
    nudge_y = -0.5
  ) +
  annotate(
    geom = "text",
    x = 2,
    y = -1.5,
    label = glue("Real mean wealth: {mean(wealth)}"),
    hjust = 0.5,
    vjust = 1,
    size = 4
  ) +
  scale_fill_identity() + # Use the exact colors from your data frame
  scale_color_identity() + # Use the exact colors for the text
  scale_size_area(max_size = 25) + # Adjust the range for node size
  labs(tag = "A") +
  guides(size = "none") +
  theme_graph() +
  coord_cartesian(clip = "off")
Gmean <- nodes_df |>
  ggplot() +
  geom_point(
    aes(x = id, y = wealth, fill = fillcolor, size = wealth),
    shape = 21
  ) +
  geom_point(aes(x = id, y = perceived_mean), color = "royalblue1") +
  geom_line(
    aes(x = id, y = perceived_mean),
    linetype = "dashed",
    color = "royalblue1"
  ) +
  geom_line(aes(x = id, y = mean(wealth)), color = "royalblue4") +
  ylim(c(0, 20)) +
  scale_x_continuous(breaks = 1:8, minor_breaks = c()) +
  scale_fill_identity() +
  scale_size_area(max_size = 8) +
  annotate(
    geom = "text",
    label = "Perceived mean wealth",
    color = "royalblue1",
    x = 3.2,
    y = 12.5,
    hjust = 0,
    size = 3
  ) +
  annotate(
    geom = "text",
    label = "Mean wealth",
    color = "royalblue4",
    x = 4.5,
    y = 9.5,
    hjust = 0,
    size = 3
  ) +
  guides(size = "none") +
  labs(x = "Rank", y = "Wealth", tag = "C") +
  theme_minimal() +
  coord_cartesian(clip = "off")
Gbar <- nodes_df |>
  ggplot(aes(x = perceived_quartile_group, fill = fillcolor)) +
  geom_bar(color = "black") +
  scale_fill_identity() +
  labs(x = "Perceived\nQuartile Group (PQ)", y = "") +
  scale_y_continuous(breaks = c()) +
  labs(tag = "B") +
  theme_minimal() +
  theme(panel.grid = element_blank())
free(Gnet) |
  (Gbar / Gmean + plot_layout(heights = c(1, 2))) +
    plot_layout(widths = c(3, 4))
ggsave(
  "Figs/Fig09_perception_demo.pdf",
  width = 7.5,
  height = 5,
  device = cairo_pdf
)


# Fig. 10: Perceived Wealth Decile across Homophily Strength and Wealth Perception.
# File: Fig10_homophily_decile.pdf
# BehaviorSpace: homophily_oneshot
# Data: simdata/WealthPerception_homophily2 homophily_oneshot-table.csv
dat_oneshot = read_csv(
  "simdata/WealthPerception homophily_oneshot-table.csv",
  skip = 6
) |>
  rename(
    run = `[run number]`,
    decile1 = `count turtles with [perceived_decile_group = 1]`,
    decile2 = `count turtles with [perceived_decile_group = 2]`,
    decile3 = `count turtles with [perceived_decile_group = 3]`,
    decile4 = `count turtles with [perceived_decile_group = 4]`,
    decile5 = `count turtles with [perceived_decile_group = 5]`,
    decile6 = `count turtles with [perceived_decile_group = 6]`,
    decile7 = `count turtles with [perceived_decile_group = 7]`,
    decile8 = `count turtles with [perceived_decile_group = 8]`,
    decile9 = `count turtles with [perceived_decile_group = 9]`,
    decile10 = `count turtles with [perceived_decile_group = 10]`,
    homophily = homophily_strength,
    wealth_change = `wealth_change?`
  )
df_long2 <- dat_oneshot |>
  pivot_longer(
    cols = starts_with("decile"),
    names_to = "decile",
    values_to = "count"
  ) |>
  mutate(
    decile_num = as.numeric(gsub("decile", "", decile)),
    proportion = count / 1000
  ) |>
  group_by(homophily, wealth_change, decile_num) |> # include all grouping vars except repetition
  summarise(
    mean_count = mean(count, na.rm = TRUE),
    sd_count = sd(count, na.rm = TRUE),
    se_count = sd(count, na.rm = TRUE) / sqrt(n()),
    mean_prop = mean(proportion, na.rm = TRUE),
    sd_prop = sd(proportion, na.rm = TRUE),
    se_prop = sd(proportion, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )
df_long2 |>
  ggplot(aes(x = decile_num, y = mean_prop)) +
  geom_hline(yintercept = 0.1, color = "gray30") +
  geom_bar(stat = "identity", fill = "skyblue3") +
  geom_errorbar(
    aes(ymin = pmax(mean_prop - sd_prop, 0), ymax = mean_prop + sd_prop),
    width = 0.2,
    color = "navyblue"
  ) +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(
    breaks = c(0, 0.1, 0.2),
    position = "right",
    labels = scales::percent_format(accuracy = 1)
  ) +
  facet_grid(
    homophily ~ wealth_change,
    switch = "y",
    labeller = labeller(
      wealth_change = c(
        "FALSE" = "Wealth fixed",
        "TRUE" = "Wealth change"
      ),
      homophily = function(h) paste("h =", h)
    )
  ) +
  labs(
    x = "Perceived decile",
    y = "",
    title = ""
  ) +
  theme(legend.position = "right")
ggsave(
  "Figs/Fig10_homophily_decile.pdf",
  width = 8,
  height = 6,
  device = cairo_pdf
)

# Fig. 11: Network visualization across different homophily strengths.
# File: Fig11_networks.png
# Network data has been extracted from simulation runs and layouted and visualized in Gephi.

# Fig. 12: Time evolution of the percentage of agents with wealth below the mean
# wealth they perceive in their social network.
# File: Fig12_homophily_perceived_mean_time.pdf
# BehaviorSpace: homophily and perceived mean
# Data: simdata/WealthPerception homophily and perceived mean-table.csv
df <- read_csv(
  "simdata/WealthPerception homophily and perceived mean-table.csv",
  skip = 6,
  show_col_types = FALSE
) |>
  mutate(
    run = `[run number]`,
    step = `[step]`
  )
dflong <- df |>
  pivot_longer(
    cols = c(
      fraction_below_mean,
      fraction_below_perceived_mean,
      for_more_tax
    ),
    names_to = "Outcome",
    values_to = "Value"
  ) |>
  arrange(desc(efficiency)) |>
  mutate(
    Measure = case_when(
      Outcome == "fraction_below_mean" ~
        "Agents with wealth below mean (= for more tax under full efficiency and no perception bias)",
      Outcome == "fraction_below_perceived_mean" |
        (Outcome == "for_more_tax" & efficiency == 1) ~
        "Agents with wealth below perceived mean (= for more tax under full efficiency)",
      Outcome == "for_more_tax" & efficiency != 1 ~
        paste0("Agents for more tax under efficiency ", 100 * efficiency, "%")
    ) |>
      as_factor(),
    wealth_change = if_else(
      `wealth_change?` == 1,
      "With wealth change",
      "Only network rewiring"
    ) |>
      as_factor() |>
      fct_rev()
  ) |>
  summarize(
    `Mean Value` = mean(Value),
    `SD Value` = sd(Value),
    n = n(),
    .by = c(Measure, homophily_strength, wealth_change, step)
  )
greenpal <- colorRampPalette(RColorBrewer::brewer.pal(9, "YlGn")[4:9])(7)
dflong |>
  filter(
    Measure ==
      "Agents with wealth below perceived mean (= for more tax under full efficiency)",
    # "Percentage with wealth below perceived mean\n(= percentage for more tax under full efficiency)",
    wealth_change == "Only network rewiring",
    step > 0,
    homophily_strength > 0
  ) |>
  mutate(`Homophily strength (h)` = factor(homophily_strength)) |>
  ggplot(aes(
    x = step,
    y = `Mean Value`,
    color = `Homophily strength (h)`,
    label = n
  )) +
  geom_point() +
  geom_line() +
  geom_errorbar(
    aes(
      ymin = `Mean Value` - `SD Value`,
      ymax = `Mean Value` + `SD Value`
    ),
    width = 0.5
  ) +
  # geom_label() +
  geom_hline(yintercept = 0.5, color = "gray") +
  scale_x_continuous(breaks = seq(1, 20, 1), minor_breaks = NULL) +
  # scale y as percentage
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 5),
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_color_manual(values = greenpal) +
  #facet_grid(~wealth_change) +
  labs(
    #title = "Time evolution of Percentage with Wealth below Perceived Mean Wealth",
    x = "Time",
    y = "Percentage with wealth below perceived mean",
    color = "Homophily\nstrength (h)",
    #caption = "Initial wealth distribution is a standard lognormal distribution. Initial network is random without homophily and minimal degree 10.\nNumber of simulations per data point: 70. Error bars: 1 SD"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.caption = element_text(
      hjust = 0, # Left align the caption
      margin = margin(t = 10, b = 5, l = 0, r = -200), # Negative left/right margins
    )
  )
ggsave(
  "Figs/Fig12_homophily_perceived_mean_time.pdf",
  width = 8,
  height = 5,
  device = cairo_pdf
)


# Fig. 13: Impact of homophily on perception of the mean wealth and the tax
# preferences.
# File: Fig13_homophily_perceived_mean.pdf
# BehaviorSpace: homophily and perceived mean (same as Fig. 12)
# Data: simdata/WealthPerception homophily and perceived mean-table.csv (same as Fig. 12)
dflong |> # dflong is take from Fig. 12 code
  filter(step == 20) |>
  ggplot(aes(
    x = homophily_strength,
    y = `Mean Value`,
    color = Measure,
    label = n
  )) +
  geom_point() +
  geom_line() +
  geom_errorbar(
    aes(
      ymin = `Mean Value` - `SD Value`,
      ymax = `Mean Value` + `SD Value`
    ),
    width = 0.5
  ) +
  # geom_label() +
  geom_hline(yintercept = 0.5, color = "gray") +
  scale_x_continuous(breaks = seq(0, 14, 2), minor_breaks = NULL) +
  # scale y as percentage
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 10),
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_color_manual(
    values = c("black", rev(RColorBrewer::brewer.pal(9, "Reds")[4:9]))
  ) +
  facet_grid(~wealth_change) +
  labs(
    #title = "Impact of Homophily on Perception of Mean Wealth and Tax Preferences",
    x = "Homophily strength (h)",
    y = "Mean value at t=20",
    color = NULL
    #caption = "Initial wealth distribution is a standard lognormal distribution. Initial network is random without homophily and minimal degree 10.\nWealth change with baseline parameters μ = 0.01, σ = 0.25, τ = 0.015. No enogenous change of tax.\nNumber of simulations per data point: 60 for actual mean, 70 for perceived mean and full efficiency, 10 for all other. Error bars: 1 SD"
  ) +
  theme_minimal(base_size = 12) +
  guides(color = guide_legend(ncol = 1)) +
  theme(
    plot.caption = element_text(
      hjust = 0, # Left align the caption
      margin = margin(t = 10, b = 5, l = 0, r = -200), # Negative left/right margins
    ),
    legend.key.size = unit(0, 'lines'),
    legend.position = "bottom",
    legend.byrow = TRUE
  )
ggsave(
  "Figs/Fig13_homophily_perceived_mean.pdf",
  width = 7,
  height = 4.5,
  device = cairo_pdf
)


# Fig. 14: Tax rate dynamics.
# File: Fig14_taxrate_dyn.pdf
# BehaviorSpace: tax_rate_new_social_on
# Data: WealthPerception tax_rate_new_social_on-table_new.csv
df <- read_csv(
  "simdata/WealthPerception tax_rate_new_social_on-table.csv",
  skip = 6,
  show_col_types = FALSE
) |>
  mutate(
    run = `[run number]`,
    step = `[step]`,
    taxrate = `taxrate...27`
  ) |>
  mutate(
    run_id = 1 + (as.numeric(factor(run)) %% 10),
    is_baseline = ifelse(
      homophily_strength == 10 & efficiency == 0.95,
      TRUE,
      FALSE
    )
  )
dfmean <- df |>
  summarize(
    mean_tax = mean(taxrate),
    mean_gini = mean(total_gini),
    mean_immobility_top20 = mean(immobility_top20),
    is_baseline = first(is_baseline),
    .by = c(step, efficiency, homophily_strength)
  )
# Plot
p1 <- df |>
  filter(efficiency %in% c(0.7, 0.95), homophily_strength %in% c(0, 10)) |>
  mutate(homophily_strength = fct_rev(factor(homophily_strength))) |>
  ggplot(aes(x = step, y = taxrate, color = factor(run_id))) +
  geom_line(alpha = 0.5) +
  geom_line(
    data = dfmean |>
      filter(efficiency %in% c(0.7, 0.95), homophily_strength %in% c(0, 10)),
    aes(x = step, y = mean_tax, linewidth = is_baseline),
    color = "darkred"
  ) +
  facet_grid(
    homophily_strength ~ efficiency,
    scales = "free",
    switch = "both",
    labeller = labeller(
      efficiency = c(
        "0.95" = "𝜙 = 95%",
        "0.7" = "𝜙 = 70%"
      ),
      homophily_strength = c(
        "0" = "h = 0",
        "10" = "h = 10"
      )
    )
  ) +
  scale_x_continuous(
    breaks = seq(0, 200, 50),
    minor_breaks = NULL,
    #position = "top"
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 4),
    minor_breaks = NULL,
    labels = scales::percent_format(accuracy = 1),
    position = "right"
  ) +
  scale_color_brewer(palette = "Set3") +
  scale_linewidth(range = c(0.5, 1.5)) +
  labs(
    #title = "Tax Rate Dynamics under Homophily Strength 10 and Efficiency 0.95",
    x = NULL, #"Time",
    y = "Tax rate (τ)",
    tag = "A"
    #caption = "Each thin line represents one of 50 simulation runs with homophily strength 10 and efficiency 0.95.\nThe thick blue line shows a smoothed average over all runs."
  ) +
  geom_text(
    data = data.frame(
      step = 30,
      taxrate = 0.07,
      homophily_strength = factor("10", levels = c("10", "0")),
      efficiency = 0.95
    ),
    aes(x = step, y = taxrate, label = "baseline"),
    inherit.aes = FALSE,
    size = 5,
    color = "darkred"
  ) +
  guides(color = "none", linewidth = "none") +
  theme_minimal(base_size = 18) +
  theme(strip.placement = "outside")
df2 <- read_csv(
  "simdata/WealthPerception tax_rate_new_social_on-table.csv",
  skip = 6,
  show_col_types = FALSE
) |>
  mutate(
    run = `[run number]`,
    step = `[step]`,
    taxrate = `taxrate...27`
  )
dfsum <- df2 |>
  filter(step >= 150) |>
  summarize(
    mean_tax = mean(taxrate),
    sd_tax = sd(taxrate),
    mean_gini = mean(total_gini),
    mean_immobility_top20 = mean(immobility_top20),
    sd_tax = sd(taxrate),
    n = n(),
    .by = c(homophily_strength, efficiency)
  )
p2 <- ggplot(
  mapping = aes(x = efficiency, y = homophily_strength, fill = mean_tax)
) +
  geom_tile(data = dfsum |> filter(efficiency == 0.99), width = 0.05) +
  geom_tile(data = dfsum |> filter(efficiency != 0.99), width = 0.05) +
  geom_tile(
    data = expand_grid(
      homophily_strength = c(0, 10),
      efficiency = c(0.7, 0.95),
      mean_tax = 0
    ) |>
      mutate(
        is_baseline = ifelse(
          homophily_strength == 10 & efficiency == 0.95,
          TRUE,
          FALSE
        )
      ),
    mapping = aes(linewidth = is_baseline),
    width = 0.05,
    height = 2,
    # linewidth = 1,
    alpha = 0,
    color = "darkred"
  ) +
  geom_label(
    data = dfsum |>
      filter(
        efficiency == 0.99 &
          homophily_strength == 14 |
          efficiency == 0.99 & homophily_strength == 0
      ),
    mapping = aes(label = paste0(round(mean_tax * 100, 2), "%")),
    position = position_nudge(x = 0.005),
    fill = "white",
    size = 4
  ) +
  geom_label(
    data = dfsum |>
      filter(
        efficiency == 0.95 &
          homophily_strength == 0 |
          efficiency == 0.95 & homophily_strength == 10 |
          efficiency == 0.7 & homophily_strength == 0 |
          efficiency == 0.85 & homophily_strength == 2 |
          efficiency == 0.9 & homophily_strength == 6 |
          efficiency == 0.95 & homophily_strength == 14 |
          efficiency == 0.75 & homophily_strength == 2 |
          efficiency == 0.7 & homophily_strength == 10
      ),
    mapping = aes(label = paste0(round(mean_tax * 100, 2), "%")),
    fill = "white",
    size = 4
  ) +
  scale_y_continuous(breaks = seq(0, 14, 2), minor_breaks = NULL) +
  scale_x_continuous(
    breaks = c(seq(0.7, 0.95, 0.05), 0.99),
    minor_breaks = NULL,
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_fill_gradientn(
    colors = c("white", RColorBrewer::brewer.pal(6, "YlGnBu")),
    values = scales::rescale(c(0, 0.0005, 0.05, 0.2, 0.5, 0.61, 0.66)), # Specify the points where colors change
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_linewidth(range = c(0.5, 1.5)) +
  labs(
    #title = "Average Tax Rate by Homophily Strength and Efficiency",
    y = "Homophily strength (h)",
    x = "Efficiency (𝜙)",
    fill = "Average\ntax rate (τ)",
    tag = "B"
    # caption = "Each tile is based on 15 simulation runs. Average of time steps 150 to 200."
  ) +
  annotate(
    "text",
    y = 9,
    x = 0.8,
    label = "Valley of\nzero wealth tax",
    size = 5,
    color = "black"
  ) +
  annotate(
    "text",
    y = 10,
    x = 0.895,
    label = "baseline",
    size = 5,
    color = "darkred"
  ) +
  guides(linewidth = "none") +
  theme_minimal(base_size = 18)
free(p1) / p2
ggsave(
  "Figs/Fig14_taxrate_dyn.pdf",
  width = 8.5,
  height = 10,
  device = cairo_pdf
)


## Fig. Appendix

dfsumlong <- dfsum |>
  pivot_longer(
    cols = c(mean_gini, mean_immobility_top20),
    names_to = "Measure",
    values_to = "Value"
  )

p1_majority <- df |>
  filter(
    run_id == 4,
    step < 100,
    step > 0,
    efficiency %in% c(0.7, 0.95),
    homophily_strength %in% c(0, 10)
  ) |>
  mutate(homophily_strength = fct_rev(factor(homophily_strength))) |>
  mutate(
    `under full efficiency\nand no perception bias` = fraction_below_mean,
    `under full efficiency` = fraction_below_perceived_mean,
    `under efficiency 𝜙\n(as in the simulation)` = for_more_tax
  ) |>
  pivot_longer(
    cols = c(
      `under full efficiency\nand no perception bias`,
      `under full efficiency`,
      `under efficiency 𝜙\n(as in the simulation)`
    )
  ) |>
  ggplot(aes(x = step, y = value, color = fct_rev(name))) +
  geom_hline(yintercept = 0.5, color = "gray") +
  geom_line() +
  facet_grid(
    homophily_strength ~ efficiency,
    #switch = "both",
    labeller = labeller(
      efficiency = c(
        "0.95" = "𝜙 = 95%",
        "0.7" = "𝜙 = 70%"
      ),
      homophily_strength = c(
        "0" = "h = 0",
        "10" = "h = 10"
      )
    )
  ) +
  scale_x_continuous(
    breaks = seq(0, 100, 20),
    minor_breaks = NULL,
    #position = "top"
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = c(0, 0.5, 1),
    minor_breaks = NULL,
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_color_manual(
    values = c("deepskyblue3", "purple", "darkgreen")
  ) +
  scale_linewidth(range = c(0.5, 1.5)) +
  labs(
    #title = "Tax Rate Dynamics under Homophily Strength 10 and Efficiency 0.95",
    x = NULL, #"Time",
    y = NULL,
    color = "Agents for more tax",
    tag = "A"
    #caption = "Each thin line represents one of 50 simulation runs with homophily strength 10 and efficiency 0.95.\nThe thick blue line shows a smoothed average over all runs."
  ) +
  guides(linewidth = "none", color = guide_legend(ncol = 1)) +
  theme_minimal(base_size = 18) +
  theme(legend.position = "right")

p1_ineq_immob <-
  dfmean |>
  filter(
    step < 100,
    efficiency %in% c(0.7, 0.95),
    homophily_strength %in% c(0, 10)
  ) |>
  pivot_longer(cols = c(mean_gini, mean_immobility_top20)) |>
  mutate(
    homophily_strength = fct_rev(factor(homophily_strength)),
    efficiency = fct_rev(factor(efficiency))
  ) |>
  ggplot(aes(
    x = step,
    y = value,
    color = homophily_strength,
    linetype = efficiency
  )) +
  geom_line() +
  facet_wrap(
    ~name,
    nrow = 1,
    labeller = labeller(
      name = c(
        "mean_gini" = "Gini",
        "mean_immobility_top20" = "Immobility Top 20%"
      )
    )
  ) +
  scale_x_continuous(
    breaks = seq(0, 100, 20),
    minor_breaks = NULL,
    #position = "top"
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    minor_breaks = NULL,
    # labels = scales::percent_format(accuracy = 1),
    # position = "right"
  ) +
  scale_color_manual(values = c("darkred", "deepskyblue2")) +
  scale_linetype(labels = c("95%", "70%")) +
  labs(
    #title = "Tax Rate Dynamics under Homophily Strength 10 and Efficiency 0.95",
    x = NULL, #"Time",
    y = NULL,
    color = "Homophily\nstrength (h)",
    linetype = "Efficiency (𝜙)",
    tag = "B"
    #caption = "Each thin line represents one of 50 simulation runs with homophily strength 10 and efficiency 0.95.\nThe thick blue line shows a smoothed average over all runs."
  ) +
  guides() +
  theme_minimal(base_size = 18)

p2_gini_immob <- ggplot(
  mapping = aes(x = efficiency, y = homophily_strength, fill = Value)
) +
  geom_tile(data = dfsumlong |> filter(efficiency == 0.99), width = 0.05) +
  geom_tile(data = dfsumlong |> filter(efficiency != 0.99), width = 0.05) +
  geom_tile(
    data = expand_grid(
      homophily_strength = c(0, 10),
      efficiency = c(0.7, 0.95),
      Value = 0
    ) |>
      mutate(
        homophily_strength_fct = fct_rev(factor(homophily_strength)),
        efficiency_fct = fct_rev(factor(efficiency))
      ),
    mapping = aes(linetype = efficiency_fct, color = homophily_strength_fct),
    width = 0.05,
    height = 2,
    alpha = 0,
    linewidth = 1
  ) +
  geom_label(
    data = dfsumlong |>
      filter(
        efficiency == 0.99 &
          homophily_strength == 14 |
          efficiency == 0.99 & homophily_strength == 0
      ),
    mapping = aes(label = paste0(round(Value, 2))),
    position = position_nudge(x = 0.01),
    fill = "white",
    size = 3
  ) +
  geom_label(
    data = dfsumlong |>
      filter(
        efficiency == 0.95 &
          homophily_strength == 0 |
          efficiency == 0.95 & homophily_strength == 10 |
          efficiency == 0.7 & homophily_strength == 0 |
          efficiency == 0.85 & homophily_strength == 2 |
          efficiency == 0.9 & homophily_strength == 6 |
          efficiency == 0.95 & homophily_strength == 14 |
          efficiency == 0.75 & homophily_strength == 2 |
          efficiency == 0.7 & homophily_strength == 10
      ),
    mapping = aes(label = paste0(round(Value, 2))),
    fill = "white",
    size = 3
  ) +
  facet_wrap(
    ~Measure,
    nrow = 1,
    labeller = as_labeller(
      c(
        mean_gini = "Gini",
        mean_immobility_top20 = "Immobility Top 20%"
      )
    )
  ) +
  scale_y_continuous(breaks = seq(0, 14, 2), minor_breaks = NULL) +
  scale_x_continuous(
    breaks = c(seq(0.7, 0.95, 0.1), 0.99),
    #minor_breaks = NULL,
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_color_manual(values = c("darkred", "deepskyblue2")) +
  scale_fill_gradientn(
    limits = c(0, 1),
    colors = c("white", RColorBrewer::brewer.pal(6, "YlGnBu")),
    values = scales::rescale(c(0, 1)) # Specify the points where colors change
    #  values = scales::rescale(c(0, 0.0005, 0.05, 0.2, 0.5, 0.61, 0.66)) # Specify the points where colors change
  ) +
  labs(
    #title = "Average Tax Rate by Homophily Strength and Efficiency",
    y = "Homophily strength (h)",
    x = "Efficiency (𝜙)",
    fill = NULL,
    tag = "C"
    # caption = "Each tile is based on 15 simulation runs. Average of time steps 150 to 200."
  ) +
  annotate(
    "text",
    y = 9,
    x = 0.8,
    label = "Valley of\nzero wealth tax",
    size = 5,
    color = "white"
  ) +
  guides(linetype = "none", color = "none") +
  theme_minimal(base_size = 18)

p1_majority /
  p1_ineq_immob /
  p2_gini_immob +
  plot_layout(heights = c(1.3, 0.7, 1))
ggsave(
  "Figs/FigAppendix_taxrate_dyn.pdf",
  width = 10,
  height = 12,
  device = cairo_pdf
)
