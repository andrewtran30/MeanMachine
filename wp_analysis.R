# win_probability_analysis.R
# Analysis of win probability calculations for 4th and goal situations

library(dplyr)
library(ggplot2)
library(tidyr)

# Source required files 
source("goal_to_go_data.R")
source("goal_to_go_models.R")
source("goal_to_go_fg_model.R")
source("goal_to_go_sampling.R")
source("goal_to_go_simulation.R")
source("improved_simulation.R")

# Function to analyze win probability across various game scenarios
analyze_win_probability <- function(ytg_values = 1:10, 
                                    score_diffs = c(-14, -10, -7, -4, -3, -2, -1, 0, 1, 3, 7), 
                                    time_values = c(60, 30, 15, 10, 5, 2), 
                                    n_sims = 1000) {
  # Initialize results data frame
  results <- data.frame()
  
  # Create all combinations of game scenarios
  scenarios <- expand.grid(
    ytg = ytg_values,
    score_diff = score_diffs,
    time_remaining = time_values
  )
  
  cat("Analyzing", nrow(scenarios), "different game scenarios...\n")
  
  # Run simulations for each scenario
  for (i in 1:nrow(scenarios)) {
    if (i %% 10 == 0) {
      cat("Progress:", round(i/nrow(scenarios)*100), "%\n")
    }
    
    ytg <- scenarios$ytg[i]
    score_diff <- scenarios$score_diff[i]
    time_remaining <- scenarios$time_remaining[i]
    
    # Run simulation for this scenario
    sim_result <- simulate_4th_and_goal2(ytg, score_diff, time_remaining, n_sims)
    
    # Extract win probability data
    result_row <- data.frame(
      yards_to_go = ytg,
      score_diff = score_diff,
      time_remaining = time_remaining,
      go_wp_change = sim_result$go_for_it$wp_change,
      fg_wp_change = sim_result$field_goal$wp_change,
      wp_diff = sim_result$go_for_it$wp_change - sim_result$field_goal$wp_change,
      recommendation = sim_result$recommendation,
      go_success_prob = sim_result$go_for_it$success_prob,
      fg_success_prob = sim_result$field_goal$success_prob
    )
    
    # Append to results
    results <- rbind(results, result_row)
  }
  
  # Save results
  saveRDS(results, "win_probability_analysis_results.rds")
  
  return(results)
}

# Function to create win probability visualizations
create_wp_visualizations <- function(wp_results = NULL) {
  # Load results if not provided
  if (is.null(wp_results)) {
    wp_results <- readRDS("win_probability_analysis_results.rds")
  }
  
  # 1. Heat map of go vs fg by yards to go and score differential (for a specific time)
  for (time in unique(wp_results$time_remaining)) {
    time_data <- wp_results %>% filter(time_remaining == time)
    
    p1 <- ggplot(time_data, aes(x = yards_to_go, y = factor(score_diff), fill = wp_diff)) +
      geom_tile() +
      scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0,
                           name = "WP Difference\n(Go - FG)") +
      labs(title = paste("Win Probability Difference (Go - FG) with", time, "seconds remaining"),
           x = "Yards to Go", y = "Score Differential") +
      theme_minimal()
    
    ggsave(paste0("wp_heatmap_", time, "sec.png"), p1, width = 10, height = 8)
  }
  
  # 2. Line plot showing how win probability changes with time for different score scenarios
  wp_results %>%
    filter(yards_to_go == 3) %>%  # Common 4th and goal distance
    ggplot(aes(x = time_remaining, y = wp_diff, color = factor(score_diff))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    labs(title = "Win Probability Difference (Go - FG) Over Time",
         subtitle = "3 yards to go",
         x = "Time Remaining (seconds)", 
         y = "WP Difference (Go - FG)",
         color = "Score Diff") +
    theme_minimal()
  
  ggsave("wp_time_trends.png", width = 10, height = 8)
  
  # 3. Create critical situations analysis
  critical_data <- wp_results %>%
    filter((score_diff <= -4 & score_diff >= -8 & time_remaining <= 15) |  # Need TD when down 4-8
             (score_diff >= -3 & score_diff <= -1 & time_remaining <= 10))   # Field goal range when down by 1-3
  
  p3 <- ggplot(critical_data, aes(x = yards_to_go, y = wp_diff, color = factor(time_remaining))) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    facet_wrap(~score_diff, scales = "free_y") +
    labs(title = "Win Probability Analysis for Critical Game Situations",
         x = "Yards to Go", 
         y = "WP Difference (Go - FG)",
         color = "Time Remaining") +
    theme_minimal()
  
  ggsave("wp_critical_situations.png", p3, width = 12, height = 8)
  
  # 4. Recommendation summary
  recommendation_summary <- wp_results %>%
    group_by(score_diff, time_remaining) %>%
    summarize(
      go_count = sum(recommendation == "Go for it"),
      fg_count = sum(recommendation == "Kick field goal"),
      total = n(),
      go_pct = go_count / total * 100,
      .groups = "drop"
    )
  
  p4 <- ggplot(recommendation_summary, 
               aes(x = factor(time_remaining), y = factor(score_diff), fill = go_pct)) +
    geom_tile() +
    scale_fill_gradient(low = "yellow", high = "blue", name = "% Go For It") +
    labs(title = "Percentage of Scenarios Where 'Go For It' is Recommended",
         x = "Time Remaining (seconds)", 
         y = "Score Differential") +
    theme_minimal()
  
  ggsave("wp_recommendation_summary.png", p4, width = 10, height = 8)
  
  cat("Visualizations created and saved as PNG files\n")
  
  # Return data used for plots
  return(list(
    heat_map_data = wp_results,
    critical_data = critical_data,
    recommendation_summary = recommendation_summary
  ))
}

# Function to specifically analyze critical situations
analyze_critical_situations <- function(wp_results = NULL) {
  # Load results if not provided
  if (is.null(wp_results)) {
    wp_results <- readRDS("win_probability_analysis_results.rds")
  }
  
  # Define critical scenarios
  critical_scenarios <- list(
    down_by_touchdown = wp_results %>% filter(score_diff == -7),
    down_by_field_goal = wp_results %>% filter(score_diff %in% c(-1, -2, -3)),
    tied_game = wp_results %>% filter(score_diff == 0),
    final_minutes = wp_results %>% filter(time_remaining <= 5)
  )
  
  # Analyze common coaching decisions in these scenarios
  scenario_analysis <- data.frame()
  
  # For each critical scenario
  for (name in names(critical_scenarios)) {
    # Calculate percentage of "Go for it" recommendations
    data <- critical_scenarios[[name]]
    summary <- data %>%
      group_by(time_remaining) %>%
      summarize(
        scenario = name,
        total_scenarios = n(),
        go_for_it_count = sum(recommendation == "Go for it"),
        go_for_it_pct = go_for_it_count / total_scenarios * 100,
        avg_wp_diff = mean(wp_diff),
        .groups = "drop"
      )
    
    scenario_analysis <- rbind(scenario_analysis, summary)
  }
  
  # Save analysis
  write.csv(scenario_analysis, "critical_scenarios_analysis.csv", row.names = FALSE)
  
  # Create visual summary
  p <- ggplot(scenario_analysis, aes(x = time_remaining, y = go_for_it_pct, color = scenario)) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    labs(title = "Analysis of Critical Game Scenarios",
         subtitle = "Percentage of scenarios where 'Go for it' is recommended",
         x = "Time Remaining (seconds)",
         y = "% Go For It Recommended",
         color = "Scenario") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggsave("critical_scenarios_summary.png", p, width = 10, height = 8)
  
  cat("Critical situations analysis complete and saved\n")
  return(scenario_analysis)
}

# Function to compare win probability with expected points
compare_wp_vs_ep <- function() {
  # Run a set of simulations that track both metrics
  ytg_values <- 1:10
  score_diffs <- c(-7, -3, 0, 3, 7)
  time_values <- c(60, 30, 15, 5)
  
  # Initialize results
  comparison_results <- data.frame()
  
  # Run simulations
  for (ytg in ytg_values) {
    for (score_diff in score_diffs) {
      for (time in time_values) {
        # Run simulation for this scenario
        sim_result <- simulate_4th_and_goal2(ytg, score_diff, time, n_sims = 1000)
        
        # Calculate recommendations directly
        wp_recommendation <- ifelse(sim_result$go_for_it$wp_change > sim_result$field_goal$wp_change, 
                                    "Go for it", "Kick field goal")
        ep_recommendation <- ifelse(sim_result$go_for_it$expected_pts > sim_result$field_goal$expected_pts, 
                                    "Go for it", "Kick field goal")
        
        # Extract both EP and WP data
        result_row <- data.frame(
          yards_to_go = ytg,
          score_diff = score_diff,
          time_remaining = time,
          go_wp_change = sim_result$go_for_it$wp_change,
          fg_wp_change = sim_result$field_goal$wp_change,
          wp_diff = sim_result$go_for_it$wp_change - sim_result$field_goal$wp_change,
          go_expected_pts = sim_result$go_for_it$expected_pts,
          fg_expected_pts = sim_result$field_goal$expected_pts,
          ep_diff = sim_result$go_for_it$expected_pts - sim_result$field_goal$expected_pts,
          wp_recommendation = wp_recommendation,
          ep_recommendation = ep_recommendation,
          same_recommendation = wp_recommendation == ep_recommendation
        )
        
        # Append to results
        comparison_results <- rbind(comparison_results, result_row)
      }
    }
  }
  
  # Calculate agreement percentage
  agreement_pct <- mean(comparison_results$same_recommendation) * 100
  cat("Win probability and expected points agree on recommendations", 
      round(agreement_pct, 1), "% of the time\n")
  
  # Create visualization of differences
  p1 <- ggplot(comparison_results, aes(x = ep_diff, y = wp_diff)) +
    geom_point(aes(color = factor(time_remaining), shape = factor(score_diff)), alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(title = "Comparison of Expected Points vs Win Probability Differences",
         subtitle = paste0("Agreement: ", round(agreement_pct, 1), "% of scenarios"),
         x = "Expected Points Difference (Go - FG)",
         y = "Win Probability Difference (Go - FG)",
         color = "Time Remaining",
         shape = "Score Diff") +
    theme_minimal()
  
  ggsave("wp_vs_ep_comparison.png", p1, width = 10, height = 8)
  
  # Identify disagreement scenarios
  disagreement_scenarios <- comparison_results %>%
    filter(!same_recommendation) %>%
    arrange(time_remaining, score_diff, yards_to_go)
  
  write.csv(disagreement_scenarios, "wp_ep_disagreements.csv", row.names = FALSE)
  
  # Save all comparison results
  saveRDS(comparison_results, "wp_ep_comparison_results.rds")
  
  return(list(
    comparison_results = comparison_results,
    disagreement_scenarios = disagreement_scenarios,
    agreement_pct = agreement_pct
  ))
}

# Run a complete win probability analysis
run_win_probability_analysis <- function(run_analysis = TRUE, create_visualizations = TRUE,
                                         analyze_critical = TRUE, compare_with_ep = TRUE) {
  cat("Starting Win Probability Analysis for 4th and Goal Situations\n")
  cat("----------------------------------------------------------\n")
  
  # Step 1: Run the comprehensive analysis
  results <- NULL
  if (run_analysis) {
    cat("\nStep 1: Running comprehensive win probability analysis...\n")
    results <- analyze_win_probability()
  } else {
    cat("\nSkipping analysis step, attempting to load previous results...\n")
    tryCatch({
      results <- readRDS("win_probability_analysis_results.rds")
      cat("Loaded existing analysis results\n")
    }, error = function(e) {
      cat("Could not load existing results. Analysis must be run first.\n")
      return(NULL)
    })
  }
  
  # Step 2: Create visualizations
  if (create_visualizations && !is.null(results)) {
    cat("\nStep 2: Creating win probability visualizations...\n")
    viz_data <- create_wp_visualizations(results)
  } else if (create_visualizations) {
    cat("\nStep 2: Creating win probability visualizations from saved results...\n")
    viz_data <- create_wp_visualizations()
  } else {
    cat("\nSkipping visualization step...\n")
  }
  
  # Step 3: Analyze critical situations
  if (analyze_critical && !is.null(results)) {
    cat("\nStep 3: Analyzing critical game situations...\n")
    critical_analysis <- analyze_critical_situations(results)
  } else if (analyze_critical) {
    cat("\nStep 3: Analyzing critical game situations from saved results...\n")
    critical_analysis <- analyze_critical_situations()
  } else {
    cat("\nSkipping critical situations analysis...\n")
  }
  
  # Step 4: Compare win probability with expected points
  if (compare_with_ep) {
    cat("\nStep 4: Comparing win probability with expected points...\n")
    comparison <- compare_wp_vs_ep()
  } else {
    cat("\nSkipping comparison with expected points...\n")
  }
  
  cat("\nWin Probability Analysis Complete!\n")
  cat("Results and visualizations have been saved to your working directory.\n")
}

# Example usage:
run_win_probability_analysis()
