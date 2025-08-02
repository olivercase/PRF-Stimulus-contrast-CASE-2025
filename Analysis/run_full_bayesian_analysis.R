# Title: Comprehensive Bayesian Re-analysis for All pRF Metrics
# This script re-runs the Bayesian hierarchical models for both slope and offset
# across all available pRF size metrics, as requested. It saves model summaries,
# diagnostic trace plots, and full posterior data for each model.

# --- 1. Setup: Install and load necessary packages ---
required_packages <- c("tidyverse", "rstanarm", "here", "patchwork")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, repos = "http://cran.us.r-project.org")
  }
  library(pkg, character.only = TRUE)
}

# --- 2. Load and Prepare Data ---
cat("--- Loading and Preparing Data ---\n")
data_file <- "pRF_analysis_master_table.csv"
if (!file.exists(data_file)) {
  stop("Error: Master data file not found!")
}

master_table <- read_csv(data_file, show_col_types = FALSE) %>%
  mutate(across(where(is.character), ~str_trim(str_remove_all(., "[{}']")))) %>%
  mutate(
    Participant = as.factor(Participant),
    Area = factor(Area, levels = c("V1", "V2", "V3")),
    Contrast = factor(Contrast, levels = c("Low", "Mid", "High")),
    SizeType = as.factor(SizeType)
  )
cat("✓ Data loaded and cleaned successfully.\n\n")

# Identify the different size types to analyze
size_types <- unique(master_table$SizeType)
cat(paste("Found the following SizeType levels to analyze:", paste(levels(size_types), collapse = ", "), "\n\n"))

# --- 3. Main Analysis Loop ---
# Create a directory to store all the output
output_dir <- "full_bayesian_analysis_output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Loop over each metric (Slope, Offset) and each SizeType
metrics_to_analyze <- c("Slope", "Offset")

for (metric in metrics_to_analyze) {
  for (size_type in size_types) {
    
    cat(paste("--- Analyzing:", metric, "for SizeType:", size_type, "---\n"))
    
    # --- a. Prepare data for the current model ---
    model_data <- master_table %>% filter(SizeType == size_type)
    
    # Skip if there's no data for this combination
    if (nrow(model_data) == 0) {
      cat("Skipping: No data found.\n\n")
      next
    }
    
    formula_str <- paste(metric, "~ Area * Contrast + (1 | Participant)")
    model_formula <- as.formula(formula_str)
    
    # --- b. Fit the Bayesian model ---
    cat("Fitting model...\n")
    bayesian_model <- stan_lmer(
      formula = model_formula,
      data = model_data,
      seed = 42, 
      cores = 4, 
      iter = 2000, 
      warmup = 500,
      adapt_delta = 0.95
    )
    
    # Create a unique name for file outputs
    file_prefix <- file.path(output_dir, paste0(tolower(metric), "_", size_type))

    # --- c. Save Model Summary ---
    cat("Saving model summary...\n")
    sink(paste0(file_prefix, "_summary.txt"))
    print(summary(bayesian_model), digits = 3)
    sink()
    
    # --- d. Save Diagnostic Trace Plots ---
    cat("Saving diagnostic trace plots...\n")
    trace_plot <- plot(bayesian_model, "trace") + 
      ggtitle(paste("Trace Plots for", metric, "-", size_type))
    ggsave(paste0(file_prefix, "_traces.pdf"), trace_plot, width = 12, height = 8)
    
    # --- e. Save Full Posterior Data ---
    cat("Saving full posterior data...\n")
    saveRDS(bayesian_model, file = paste0(file_prefix, "_model_object.rds"))
    
    cat(paste("✓ Analysis complete for", metric, "-", size_type, "\n\n"))
  }
}

cat("--- Full Bayesian Re-analysis Finished ---\n")
cat(paste("All outputs saved in the '", output_dir, "' directory.\n")) 