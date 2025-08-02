# Title: Generation of Publication-Quality Figures for Bayesian pRF Analysis
# This script loads the saved Bayesian model objects and generates a series of
# multi-panel figures to visually support the claims made in the results section.

# --- 1. Setup: Install and load necessary packages ---
required_packages <- c("tidyverse", "rstanarm", "patchwork", "ggdist", "tidybayes")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, repos = "http://cran.us.r-project.org")
  }
  library(pkg, character.only = TRUE)
}

# --- 2. Define Plotting Globals ---
area_colors <- c("V1" = "#0072B2", "V2" = "#009E73", "V3" = "#D55E00")
contrast_colors <- c("Low" = "#E41A1C", "Mid" = "#4DAF4A", "High" = "#377EB8")
theme_set(theme_classic(base_size = 14))

# --- 3. Load Data and Model Objects ---
cat("--- Loading Data and All Model Objects ---\n")
# Load master data table for context
master_table <- read_csv("pRF_analysis_master_table.csv", show_col_types = FALSE) %>%
  mutate(
    Area = factor(Area, levels = c("V1", "V2", "V3")),
    Contrast = factor(Contrast, levels = c("Low", "Mid", "High"))
  )

# Load all model .rds files from the output directory
output_dir <- "full_bayesian_analysis_output"
rds_files <- list.files(output_dir, pattern = "\\.rds$", full.names = TRUE)
if (length(rds_files) == 0) {
  stop("No .rds model files found in ", output_dir)
}
models <- rds_files %>%
  set_names(basename(.) %>% str_remove("_model_object\\.rds$")) %>%
  map(readRDS)
cat("✓ All 8 model objects loaded successfully.\n\n")


# --- Helper function to generate interaction plots ---
create_interaction_plot <- function(model, metric_name) {
  # Create a grid of conditions to predict for
  grid <- expand.grid(
    Area = c("V1", "V2", "V3"),
    Contrast = c("Low", "Mid", "High"),
    Participant = master_table$Participant[1] # Dummy participant for prediction
  )
  
  # Get posterior predictions on the response scale
  posterior_preds <- posterior_epred(model, newdata = grid)
  
  # Summarize the predictions
  plot_data <- grid %>%
    select(-Participant) %>%
    distinct() %>%
    mutate(
      mean = apply(posterior_preds, 2, mean),
      lower = apply(posterior_preds, 2, function(x) quantile(x, 0.025)),
      upper = apply(posterior_preds, 2, function(x) quantile(x, 0.975))
    )

  ggplot(plot_data, aes(x = Contrast, y = mean, color = Area, group = Area)) +
    geom_line(linewidth = 1.2) +
    geom_pointrange(aes(ymin = lower, ymax = upper), linewidth = 1, size = 0.7) +
    scale_color_manual(values = area_colors) +
    labs(y = paste("Predicted", metric_name)) +
    theme(legend.position = "none")
}

# --- Helper function to generate coefficient plots ---
create_coefficient_plot <- function(model, title) {
  # Extract posterior draws for fixed effects only
  posterior_draws <- as_draws_df(model) %>%
    select(starts_with("Area"), starts_with("Contrast"), starts_with("Area:"), `(Intercept)`) %>%
    # Pivot to a long format for plotting
    pivot_longer(everything(), names_to = "parameter", values_to = "value") %>%
    # Clean up parameter names
    mutate(parameter = str_replace_all(parameter, c("Area" = "A:", "Contrast" = "C:")))

  ggplot(posterior_draws, aes(x = value, y = reorder(parameter, value))) +
    stat_halfeye(aes(fill = after_stat(x > 0)), .width = c(0.8, 0.95)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    scale_fill_manual(values = c("gray80", "skyblue"), guide = "none") +
    labs(x = "Estimated Effect Size (β)", y = "Model Parameter", title = title) +
    theme(axis.title.y = element_blank())
}

# --- 4. Generate Figure 1: pRF Slope Analysis ---
cat("--- Generating Figure 1: pRF Slope Analysis ---\n")
p1_a <- create_interaction_plot(models$slope_polar, "Polar Slope") + labs(title = "A. Polar Slope")
p1_b <- create_interaction_plot(models$slope_radial, "Radial Slope") + labs(title = "B. Radial Slope")
p1_c <- create_coefficient_plot(models$slope_polar, "C. Polar Slope Coefficients")

fig1 <- (p1_a | p1_b) / p1_c +
  plot_layout(heights = c(1, 1.2)) +
  plot_annotation(
    title = "Figure 1: Bayesian Analysis of pRF Slope",
    subtitle = "Interaction plots show model-predicted means and 95% CIs. Coefficient plot shows posterior distributions with 80% and 95% CIs."
  )
ggsave("figure_1_slope_analysis.pdf", fig1, width = 12, height = 9, device = cairo_pdf)
cat("✓ Figure 1 saved as 'figure_1_slope_analysis.pdf'\n\n")


# --- 5. Generate Figure 2: pRF Offset Analysis ---
cat("--- Generating Figure 2: pRF Offset Analysis ---\n")
p2_a <- create_interaction_plot(models$offset_polar, "Polar Offset (deg)") + labs(title = "A. Polar Offset")
p2_b <- create_interaction_plot(models$offset_radial, "Radial Offset (deg)") + labs(title = "B. Radial Offset")
p2_c <- create_coefficient_plot(models$offset_polar, "C. Polar Offset Coefficients")

fig2 <- (p2_a | p2_b) / p2_c +
  plot_layout(heights = c(1, 1.2)) +
  plot_annotation(
    title = "Figure 2: Bayesian Analysis of pRF Offset (Foveal Size)",
    subtitle = "Interaction plots show model-predicted means and 95% CIs. Coefficient plot shows posterior distributions with 80% and 95% CIs."
  )
ggsave("figure_2_offset_analysis.pdf", fig2, width = 12, height = 9, device = cairo_pdf)
cat("✓ Figure 2 saved as 'figure_2_offset_analysis.pdf'\n\n")


# --- 6. Generate Figure 3: Mechanism Visualization ---
cat("--- Generating Figure 3: Mechanism Visualization ---\n")
# We will focus on the 'polar' metric as it shows strong, interesting effects
model_polar_slope <- models$slope_polar
model_polar_offset <- models$offset_polar

# Get posterior draws of all fixed effects for both models
draws_slope <- as_draws_df(model_polar_slope)
draws_offset <- as_draws_df(model_polar_offset)

# Create a grid of every condition we want to plot a line for
grid <- expand.grid(Area = c("V1", "V2", "V3"), Contrast = c("Low", "Mid", "High"))

# Calculate the posterior distribution of the slope and offset for EACH condition
# This requires combining the relevant coefficients from the model
# e.g., for V2-Mid, slope = Intercept + AreaV2 + ContrastMid + AreaV2:ContrastMid
lines_data <- grid %>%
  mutate(
    # For each row (condition), calculate the posterior distribution of its slope and offset
    slope_posterior = pmap(list(Area, Contrast), function(area, contrast) {
      # Start with the intercept
      post_vals <- draws_slope$`(Intercept)`
      # Add area effect if not V1
      if (area != "V1") {
        post_vals <- post_vals + draws_slope[[paste0("Area", area)]]
      }
      # Add contrast effect if not Low
      if (contrast != "Low") {
        post_vals <- post_vals + draws_slope[[paste0("Contrast", contrast)]]
      }
      # Add interaction effect if not V1 or Low contrast
      if (area != "V1" && contrast != "Low") {
        post_vals <- post_vals + draws_slope[[paste0("Area", area, ":Contrast", contrast)]]
      }
      return(post_vals)
    }),
    offset_posterior = pmap(list(Area, Contrast), function(area, contrast) {
      # Start with the intercept
      post_vals <- draws_offset$`(Intercept)`
      # Add area effect if not V1
      if (area != "V1") {
        post_vals <- post_vals + draws_offset[[paste0("Area", area)]]
      }
      # Add contrast effect if not Low
      if (contrast != "Low") {
        post_vals <- post_vals + draws_offset[[paste0("Contrast", contrast)]]
      }
      # Add interaction effect if not V1 or Low contrast
      if (area != "V1" && contrast != "Low") {
        post_vals <- post_vals + draws_offset[[paste0("Area", area, ":Contrast", contrast)]]
      }
      return(post_vals)
    })
  ) %>%
  # Now get the mean slope and offset to draw the line
  mutate(
    slope = map_dbl(slope_posterior, mean),
    offset = map_dbl(offset_posterior, mean)
  )

# Create a new dataframe with start and end points for each line
segment_data <- lines_data %>%
  mutate(
    x_start = 0,
    y_start = offset,
    x_end = 8,
    y_end = offset + slope * 8
  )

fig3 <- ggplot(segment_data, aes(color = Contrast, group = Contrast)) +
  # Use geom_segment to draw the lines explicitly
  geom_segment(aes(x = x_start, y = y_start, xend = x_end, yend = y_end), linewidth = 1.5) +
  scale_color_manual(values = contrast_colors) +
  # Facet by visual area
  facet_wrap(~Area, scales = "free_y") +
  labs(
    title = "Figure 3: Mechanism of pRF Modulation (Polar Metric)",
    subtitle = "Each line represents the model-estimated pRF Size-Eccentricity relationship for a given contrast level.",
    x = "pRF Eccentricity (degrees)",
    y = "pRF Size (degrees)"
  ) +
  theme(legend.position = "bottom", strip.background = element_rect(fill = "gray90")) +
  coord_cartesian(xlim = c(0, 8))

ggsave("figure_3_mechanism_visualization.pdf", fig3, width = 12, height = 6, device = cairo_pdf)
cat("✓ Figure 3 saved as 'figure_3_mechanism_visualization.pdf'\n\n")

cat("--- All Figures Generated Successfully ---\n") 