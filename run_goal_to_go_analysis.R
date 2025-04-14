# run_goal_to_go_analysis.R
# Complete pipeline for 4th and goal to go analysis

# Load required packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(mixtools)

# Source all required files
source("goal_to_go_data.R")
source("goal_to_go_models.R")
source("goal_to_go_fg_model.R")
source("goal_to_go_sampling.R")
source("goal_to_go_simulation.R")

# Run the complete analysis pipeline
run_complete_analysis <- function(extract_data = TRUE, fit_models = TRUE, run_analysis = TRUE,
                                  create_visualizations = TRUE, run_test = TRUE) {
  cat("Starting 4th and Goal Analysis Pipeline\n")
  cat("--------------------------------------\n\n")
  
  # Step 1: Extract and analyze the data
  if (extract_data) {
    cat("Step 1: Extracting and analyzing 4th and goal data...\n")
    goal_data <- extract_4th_and_goal_data()
    analyze_goal_to_go_data(goal_data)
  } else {
    cat("Skipping data extraction step...\n")
  }
  
  # Step 2: Fit the models
  if (fit_models) {
    cat("\nStep 2: Fitting mixture models for yards gained...\n")
    goal_models <- fit_goal_to_go_models(goal_data$goal_to_go)
    
    cat("\nStep 3: Fitting field goal success model...\n")
    fg_data <- goal_data$goal_to_go %>% filter(play_type == "field_goal")
    fg_model <- fit_goal_to_go_fg_model(fg_data)
  } else {
    cat("Skipping model fitting steps...\n")
  }
  
  # Step 4: Run comprehensive analysis
  if (run_analysis) {
    cat("\nStep 4: Running comprehensive analysis across all scenarios...\n")
    cat("This may take a while...\n")
    results <- run_comprehensive_goal_to_go_analysis()
  } else {
    cat("Skipping comprehensive analysis step...\n")
    
    # Try to load results if available
    tryCatch({
      results <- readRDS("4th_and_goal_analysis.rds")
      cat("Loaded existing analysis results\n")
    }, error = function(e) {
      results <- NULL
    })
  }
  
  # Step 5: Create visualizations
  if (create_visualizations && !is.null(results)) {
    cat("\nStep 5: Creating visualizations...\n")
    plot_data <- create_goal_to_go_visualizations(results)
  } else if (create_visualizations) {
    cat("\nStep 5: Creating visualizations from saved results...\n")
    plot_data <- create_goal_to_go_visualizations()
  } else {
    cat("Skipping visualization step...\n")
  }
  
  # Step 6: Run a simple test
  if (run_test) {
    cat("\nStep 6: Running simple test cases...\n")
    run_simple_test()
  } else {
    cat("Skipping test step...\n")
  }
  
  cat("\n4th and Goal Analysis Pipeline Complete!\n")
  cat("Results and visualizations have been saved to your working directory.\n")
}

# Run the complete pipeline if this script is executed directly
if (!interactive()) {
  run_complete_analysis()
}