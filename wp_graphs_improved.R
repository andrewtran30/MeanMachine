# Enhanced win probability visualization functions for academic publications
# These functions maintain the original functionality while improving visual style

library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)  # For better axis formatting
library(viridis)  # For colorblind-friendly palettes (install if not available)

# Function to create win probability visualizations with academic styling
create_wp_visualizations <- function(wp_results = NULL) {
  # Load results if not provided
  if (is.null(wp_results)) {
    wp_results <- readRDS("win_probability_analysis_results.rds")
  }
  
  # Set up custom academic publication theme
  academic_theme <- theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", size = 0.3),
      panel.border = element_rect(fill = NA, color = "gray30", size = 0.5),
      plot.title = element_text(size = 11, face = "bold", hjust = 0),
      plot.subtitle = element_text(size = 10, hjust = 0, margin = margin(b = 10)),
      plot.caption = element_text(size = 8, color = "gray30", margin = margin(t = 10)),
      plot.margin = margin(15, 15, 10, 10),
      axis.title = element_text(size = 9, face = "bold"),
      axis.text = element_text(size = 8),
      legend.position = "right",
      legend.title = element_text(size = 9, face = "bold"),
      legend.text = element_text(size = 8),
      strip.background = element_rect(fill = "gray95", color = "gray50"),
      strip.text = element_text(size = 9, face = "bold")
    )
  
  # 1. Heat map of go vs fg by yards to go and score differential (for a specific time)
  for (time in unique(wp_results$time_remaining)) {
    time_data <- wp_results %>% filter(time_remaining == time)
    
    # Use viridis color scheme if available, otherwise use built-in gradient
    if ("viridis" %in% installed.packages()[,"Package"]) {
      p1 <- ggplot(time_data, aes(x = yards_to_go, y = factor(score_diff), fill = wp_diff)) +
        geom_tile(color = "white", size = 0.1) +
        scale_fill_viridis_c(
          option = "plasma", 
          name = "Win Probability\nDifference\n(Go - FG)", # Add extra line break for spacing
          guide = guide_colorbar(
            barwidth = 12, 
            barheight = 1.5,
            title.position = "top",
            title.hjust = 0.5,
            label.hjust = 0.5
          )
        ) +
        labs(title = "Fourth Down Decision Analysis",
             subtitle = paste0("Time remaining: ", time, " minutes"),
             x = "Yards to Goal",
             y = "Score Differential",
             caption = "Note: Blue indicates 'Go For It' is advantageous; Yellow indicates 'Field Goal' is advantageous.") +
        scale_x_continuous(breaks = unique(time_data$yards_to_go), expand = c(0, 0)) +
        academic_theme +
        # Add more space for the legend
        theme(
          legend.key.height = unit(0.5, "cm"),
          legend.margin = margin(t = 0, r = 10, b = 0, l = 0),
          legend.box.margin = margin(t = 0, r = 15, b = 0, l = 0)
        )
    } else {
      p1 <- ggplot(time_data, aes(x = yards_to_go, y = factor(score_diff), fill = wp_diff)) +
        geom_tile(color = "white", size = 0.1) +
        scale_fill_gradient2(
          colors = blue_gradient, 
          midpoint = 0,
          name = "Win Probability\nDifference\n(Go - FG)", # Add extra line break for spacing
          guide = guide_colorbar(
            barwidth = 12, 
            barheight = 4,
            title.position = "top",
            label.hjust = 0.5
          )
        ) +
        labs(title = "Fourth Down Decision Analysis",
             subtitle = paste0("Time remaining: ", time, " minutes"),
             x = "Yards to Goal",
             y = "Score Differential",
             caption = "Note: Blue indicates 'Go For It' is advantageous; Red indicates 'Field Goal' is advantageous.") +
        scale_x_continuous(breaks = unique(time_data$yards_to_go), expand = c(0, 0)) +
        academic_theme +
        # Add more space for the legend
        theme(
          legend.key.height = unit(0.5, "cm"),
          legend.margin = margin(t = 0, r = 10, b = 0, l = 0),
          legend.box.margin = margin(t = 0, r = 15, b = 0, l = 0)
        )
    }
    
    ggsave(paste0("wp_heatmap_", time, "min.png"), p1, width = 7, height = 5, dpi = 300)
  }
  
  # 2. Line plot showing how win probability changes with time for different score scenarios
  p2 <- wp_results %>%
    filter(yards_to_go == 3) %>%  # Common 4th and goal distance
    ggplot(aes(x = time_remaining, y = wp_diff, color = factor(score_diff))) +
    geom_line(size = 0.7) +
    geom_point(size = 1.5, alpha = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 0.4) +
    scale_color_brewer(palette = "Set1", name = "Score\nDifferential") +
    # Fix the jumbled labels with proper spacing and formatting
    scale_x_continuous(
      breaks = sort(unique(wp_results$time_remaining)),
      expand = c(0.03, 0) # Add more buffer space
    ) +
    labs(title = "Win Probability Difference Over Time",
         subtitle = "Analysis for 4th and goal with 3 yards to go",
         x = "Time Remaining (minutes)",
         y = "Win Probability Difference (Go - FG)",
         caption = "Note: Values above zero favor going for it; values below zero favor kicking a field goal.") +
    academic_theme
  
  ggsave("wp_time_trends.png", p2, width = 7, height = 5, dpi = 300)
  
  # 3. Create critical situations analysis
  critical_data <- wp_results %>%
    filter((score_diff <= -4 & score_diff >= -8 & time_remaining <= 15) |  # Need TD when down 4-8
             (score_diff >= -3 & score_diff <= -1 & time_remaining <= 10))   # Field goal range when down by 1-3
  
  p3 <- ggplot(critical_data, aes(x = yards_to_go, y = wp_diff, color = factor(time_remaining))) +
    geom_line(size = 0.7) +
    geom_point(size = 1.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 0.4) +
    scale_color_brewer(palette = "Dark2", name = "Time\nRemaining (min)") +
    facet_wrap(~score_diff, scales = "free_y", labeller = labeller(
      score_diff = function(x) paste0("Score Differential: ", x))) +
    labs(title = "Win Probability Analysis for Critical Game Situations",
         subtitle = "Decision impact by yards to goal in late-game scenarios",
         x = "Yards to Goal",
         y = "Win Probability Difference (Go - FG)",
         caption = "Note: Each panel represents a different score differential scenario.") +
    academic_theme +
    theme(legend.position = "bottom", 
          legend.direction = "horizontal")
  
  ggsave("wp_critical_situations.png", p3, width = 8, height = 6, dpi = 300)
  
  # 4. Recommendation summary with improved legend
  recommendation_summary <- wp_results %>%
    group_by(score_diff, time_remaining) %>%
    summarize(
      go_count = sum(recommendation == "Go for it"),
      fg_count = sum(recommendation == "Kick field goal"),
      total = n(),
      go_pct = go_count / total * 100,
      .groups = "drop"
    )
  
  # Use viridis color scheme if available, otherwise use built-in gradient
  if ("viridis" %in% installed.packages()[,"Package"]) {
    p4 <- ggplot(recommendation_summary, 
                 aes(x = factor(time_remaining), y = factor(score_diff), fill = go_pct)) +
      geom_tile(color = "white", size = 0.2) +
      scale_fill_viridis_c(
        option = "inferno", 
        name = "% 'Go For It'\nRecommended", 
        limits = c(0, 100),
        breaks = seq(0, 100, by = 20), # Use fewer, more spaced out breaks
        guide = guide_colorbar(
          barwidth = 12, 
          barheight = 1.5,
          title.position = "top",
          title.hjust = 0.5,
          label.hjust = 0.5
        )
      ) +
      labs(title = "Fourth Down Strategy Recommendations",
           subtitle = "Percentage of scenarios where 'Go For It' is recommended by yards to goal",
           x = "Time Remaining (minutes)",
           y = "Score Differential",
           caption = "Note: Analysis based on win probability simulations across all yards-to-goal scenarios.") +
      academic_theme +
      # Add more space for the legend
      theme(
        legend.key.height = unit(0.5, "cm"),
        legend.margin = margin(t = 0, r = 10, b = 0, l = 0),
        legend.box.margin = margin(t = 0, r = 15, b = 0, l = 0)
      )
  } else {
    p4 <- ggplot(recommendation_summary, 
                 aes(x = factor(time_remaining), y = factor(score_diff), fill = go_pct)) +
      geom_tile(color = "white", size = 0.2) +
      scale_fill_gradient(
        colors = blue_gradient, 
        name = "% 'Go For It'\nRecommended",
        limits = c(0, 100),
        breaks = seq(0, 100, by = 20), # Use fewer, more spaced out breaks
        guide = guide_colorbar(
          barwidth = 12, 
          barheight = 4,
          title.position = "top",
          label.hjust = 0.5
        )
      ) +
      labs(title = "Fourth Down Strategy Recommendations",
           subtitle = "Percentage of scenarios where 'Go For It' is recommended by yards to goal",
           x = "Time Remaining (minutes)",
           y = "Score Differential",
           caption = "Note: Analysis based on win probability simulations across all yards-to-goal scenarios.") +
      academic_theme +
      # Add more space for the legend
      theme(
        legend.key.height = unit(0.5, "cm"),
        legend.margin = margin(t = 0, r = 10, b = 0, l = 0),
        legend.box.margin = margin(t = 0, r = 15, b = 0, l = 0)
      )
  }
  
  ggsave("wp_recommendation_summary.png", p4, width = 7, height = 5, dpi = 300)
  
  cat("Enhanced academic-style visualizations created and saved as PNG files\n")
  
  # Return data used for plots
  return(list(
    heat_map_data = wp_results,
    critical_data = critical_data,
    recommendation_summary = recommendation_summary
  ))
}

# Enhanced function to analyze critical situations with academic styling
analyze_critical_situations <- function(wp_results = NULL) {
  # Load results if not provided
  if (is.null(wp_results)) {
    wp_results <- readRDS("win_probability_analysis_results.rds")
  }
  
  # Academic publication theme
  academic_theme <- theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", size = 0.3),
      panel.border = element_rect(fill = NA, color = "gray30", size = 0.5),
      plot.title = element_text(size = 11, face = "bold", hjust = 0),
      plot.subtitle = element_text(size = 10, hjust = 0, margin = margin(b = 10)),
      plot.caption = element_text(size = 8, color = "gray30", margin = margin(t = 10)),
      plot.margin = margin(15, 15, 10, 10),
      axis.title = element_text(size = 9, face = "bold"),
      axis.text = element_text(size = 8),
      legend.position = "bottom",
      legend.title = element_text(size = 9, face = "bold"),
      legend.text = element_text(size = 8)
    )
  
  # Define critical scenarios
  critical_scenarios <- list(
    down_by_touchdown = wp_results %>% filter(score_diff == -7),
    down_by_field_goal = wp_results %>% filter(score_diff %in% c(-1, -2, -3)),
    tied_game = wp_results %>% filter(score_diff == 0),
    final_minutes = wp_results %>% filter(time_remaining <= 5)
  )
  
  # Create more descriptive scenario labels
  scenario_labels <- c(
    "down_by_touchdown" = "Down by 7 (Touchdown)",
    "down_by_field_goal" = "Down by 1-3 (Field Goal Range)",
    "tied_game" = "Tied Game",
    "final_minutes" = "Final 5 Minutes"
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
        scenario_label = scenario_labels[name],
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
  
  # Create visual summary with improved x-axis
  p <- ggplot(scenario_analysis, aes(x = time_remaining, y = go_for_it_pct, color = scenario_label)) +
    geom_line(size = 0.8) +
    geom_point(size = 2, alpha = 0.8) +
    scale_color_brewer(palette = "Set1", name = "Game Scenario") +
    # Fix the jumbled labels with proper spacing
    scale_x_continuous(
      breaks = sort(unique(wp_results$time_remaining)),
      expand = c(0.03, 0) # Add more buffer space
    ) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20), 
                       labels = function(x) paste0(x, "%")) +
    labs(title = "Fourth Down Decision Analysis in Critical Game Situations",
         subtitle = "Recommended strategy by scenario and time remaining",
         x = "Time Remaining (minutes)",
         y = "Percentage Where 'Go For It' is Recommended",
         caption = "Note: Analysis based on win probability simulations for various yards-to-goal scenarios.") +
    academic_theme
  
  ggsave("critical_scenarios_summary.png", p, width = 7.5, height = 5, dpi = 300)
  
  cat("Enhanced critical situations analysis complete and saved\n")
  return(scenario_analysis)
}

# Enhanced function to compare win probability with expected points
compare_wp_vs_ep <- function(comparison_results = NULL) {
  if (is.null(comparison_results)) {
    # Check if we can load saved comparison results
    comparison_results_path <- "wp_ep_comparison_results.rds"
    if (file.exists(comparison_results_path)) {
      comparison_results <- readRDS(comparison_results_path)
      cat("Loaded existing comparison results\n")
    } else {
      cat("No comparison results found. Please run compare_wp_vs_ep() first.\n")
      return(NULL)
    }
  }
  
  # Academic publication theme
  academic_theme <- theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", size = 0.3),
      panel.border = element_rect(fill = NA, color = "gray30", size = 0.5),
      plot.title = element_text(size = 11, face = "bold", hjust = 0),
      plot.subtitle = element_text(size = 10, hjust = 0, margin = margin(b = 10)),
      plot.caption = element_text(size = 8, color = "gray30", margin = margin(t = 10)),
      plot.margin = margin(15, 15, 10, 10),
      axis.title = element_text(size = 9, face = "bold"),
      axis.text = element_text(size = 8),
      legend.position = "right",
      legend.title = element_text(size = 9, face = "bold"),
      legend.text = element_text(size = 8)
    )
  
  # Calculate agreement percentage
  agreement_pct <- mean(comparison_results$same_recommendation) * 100
  
  # Create enhanced visualization of differences
  p1 <- ggplot(comparison_results, aes(x = ep_diff, y = wp_diff)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 0.3) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", size = 0.3) +
    geom_point(aes(color = factor(time_remaining), shape = factor(score_diff)), 
               size = 2, alpha = 0.7) +
    stat_smooth(method = "lm", color = "darkgray", linetype = "solid", 
                fill = "lightgray", alpha = 0.3) +
    scale_color_brewer(palette = "YlOrRd", name = "Time\nRemaining (min)") +
    scale_shape_discrete(name = "Score\nDifferential") +
    labs(title = "Win Probability vs. Expected Points Analysis",
         subtitle = paste0("Agreement between metrics: ", round(agreement_pct, 1), "% of scenarios"),
         x = "Expected Points Difference (Go - FG)",
         y = "Win Probability Difference (Go - FG)",
         caption = "Note: Points in quadrants I and III indicate agreement; quadrants II and IV indicate disagreement.") +
    annotate("text", x = max(comparison_results$ep_diff) * 0.7, 
             y = max(comparison_results$wp_diff) * 0.7, 
             label = "Both metrics\nfavor going for it", 
             size = 3, fontface = "italic", color = "darkgreen") +
    annotate("text", x = min(comparison_results$ep_diff) * 0.7, 
             y = min(comparison_results$wp_diff) * 0.7, 
             label = "Both metrics\nfavor field goal", 
             size = 3, fontface = "italic", color = "darkred") +
    academic_theme
  
  ggsave("wp_vs_ep_comparison.png", p1, width = 7.5, height = 6, dpi = 300)
  
  # Create a second plot showing disagreement by time remaining
  disagreement_data <- comparison_results %>%
    group_by(time_remaining, score_diff) %>%
    summarize(
      total = n(),
      disagreements = sum(!same_recommendation),
      disagreement_pct = disagreements / total * 100,
      .groups = "drop"
    )
  
  p2 <- ggplot(disagreement_data, aes(x = factor(time_remaining), y = disagreement_pct, 
                                      fill = factor(score_diff))) +
    geom_bar(stat = "identity", position = "dodge", color = "white", size = 0.2) +
    scale_fill_brewer(palette = "Set2", name = "Score\nDifferential") +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20),
                       labels = function(x) paste0(x, "%")) +
    labs(title = "Disagreement Between Win Probability and Expected Points",
         subtitle = "Percentage of scenarios where metrics lead to different recommendations",
         x = "Time Remaining (minutes)",
         y = "Disagreement Percentage",
         caption = "Note: Higher values indicate situations where coaches should prioritize win probability over expected points.") +
    academic_theme +
    theme(legend.position = "bottom")
  
  ggsave("wp_vs_ep_disagreement.png", p2, width = 7.5, height = 5, dpi = 300)
  
  cat("Enhanced win probability vs. expected points visualizations created\n")
  
  return(list(
    disagreement_data = disagreement_data,
    agreement_pct = agreement_pct
  ))
}

# Function to generate a summary figure combining key insights
create_summary_figure <- function(wp_results = NULL) {
  # Load results if not provided
  if (is.null(wp_results)) {
    wp_results_path <- "win_probability_analysis_results.rds"
    if (file.exists(wp_results_path)) {
      wp_results <- readRDS(wp_results_path)
      cat("Loaded existing win probability results\n")
    } else {
      cat("No win probability results found. Please run analyze_win_probability() first.\n")
      return(NULL)
    }
  }
  
  # Academic publication theme
  academic_theme <- theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", size = 0.3),
      panel.border = element_rect(fill = NA, color = "gray30", size = 0.5),
      plot.title = element_text(size = 10, face = "bold", hjust = 0),
      plot.subtitle = element_text(size = 9, hjust = 0),
      axis.title = element_text(size = 9, face = "bold"),
      axis.text = element_text(size = 8),
      legend.position = "bottom",
      legend.title = element_text(size = 8, face = "bold"),
      legend.text = element_text(size = 7),
      strip.background = element_rect(fill = "gray95", color = "gray50"),
      strip.text = element_text(size = 8, face = "bold")
    )
  
  # Panel 1: Win probability difference by yards to go for critical scenarios
  critical_data <- wp_results %>%
    filter(score_diff %in% c(-7, -3, 0, 3)) %>%
    filter(time_remaining %in% c(15, 5))
  
  p1 <- ggplot(critical_data, aes(x = yards_to_go, y = wp_diff, color = factor(time_remaining))) +
    geom_line(size = 0.6) +
    geom_point(size = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 0.3) +
    scale_color_brewer(palette = "Dark2", name = "Time Remaining (min)") +
    facet_wrap(~score_diff, nrow = 1, scales = "free_y", 
               labeller = labeller(score_diff = function(x) paste0("Score Diff: ", x))) +
    scale_x_continuous(breaks = seq(1, 10, by = 2)) +
    labs(title = "Win Probability Analysis by Score Differential",
         x = "Yards to Goal", 
         y = "WP Diff (Go - FG)") +
    academic_theme +
    theme(legend.position = "bottom")
  
  # Panel 2: Success probability relation to recommendation
  p2 <- wp_results %>%
    filter(time_remaining == 15, yards_to_go <= 5) %>%
    ggplot(aes(x = go_success_prob, y = fg_success_prob, color = recommendation)) +
    geom_point(alpha = 0.7, size = 1.5) +
    scale_color_manual(values = c("Go for it" = "blue", "Kick field goal" = "red"),
                       name = "Recommendation") +
    labs(title = "Decision Analysis by Success Probabilities",
         subtitle = "Time remaining: 15 minutes, ≤ 5 yards to goal",
         x = "Touchdown Success Probability", 
         y = "Field Goal Success Probability") +
    academic_theme
  
  # Panel 3: Recommendation summary by score differential
  recommendation_summary <- wp_results %>%
    group_by(score_diff) %>%
    summarize(
      go_count = sum(recommendation == "Go for it"),
      fg_count = sum(recommendation == "Kick field goal"),
      total = n(),
      go_pct = go_count / total * 100,
      .groups = "drop"
    )
  
  p3 <- ggplot(recommendation_summary, aes(x = factor(score_diff), y = go_pct)) +
    geom_bar(stat = "identity", fill = "steelblue", color = "white", size = 0.2) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20),
                       labels = function(x) paste0(x, "%")) +
    labs(title = "Fourth Down Strategy by Score Differential",
         x = "Score Differential", 
         y = "'Go For It' Recommendation %") +
    academic_theme
  
  # Check if patchwork is available for combining plots
  if ("patchwork" %in% installed.packages()[,"Package"]) {
    library(patchwork)
    
    combined_figure <- (p1 / (p2 | p3)) + 
      plot_layout(heights = c(1.2, 1)) +
      plot_annotation(
        title = "Fourth Down Decision Analysis in American Football",
        subtitle = "Win probability-based strategy recommendations",
        caption = "Note: Analysis based on simulation of win probability across multiple game scenarios.",
        theme = theme(
          plot.title = element_text(size = 12, face = "bold"),
          plot.subtitle = element_text(size = 10),
          plot.caption = element_text(size = 8, color = "gray30")
        )
      )
    
    ggsave("fourth_down_decision_summary.png", combined_figure, width = 8, height = 8, dpi = 300)
    cat("Summary figure for publication created\n")
    return(combined_figure)
  } else {
    # If patchwork is not available, save plots individually
    ggsave("summary_wp_diff.png", p1, width = 8, height = 3, dpi = 300)
    ggsave("summary_success_probs.png", p2, width = 4, height = 4, dpi = 300)
    ggsave("summary_recommendations.png", p3, width = 4, height = 4, dpi = 300)
    cat("Individual summary plots created (patchwork package not available for combined figure)\n")
    return(list(p1 = p1, p2 = p2, p3 = p3))
  }
}

# Modified run_win_probability_analysis function to use the enhanced visualization functions
run_win_probability_analysis <- function(run_analysis = TRUE, create_visualizations = TRUE,
                                         analyze_critical = TRUE, compare_with_ep = TRUE,
                                         create_summary = TRUE) {
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
  
  # Step 2: Create visualizations with academic styling
  if (create_visualizations && !is.null(results)) {
    cat("\nStep 2: Creating enhanced win probability visualizations...\n")
    viz_data <- create_wp_visualizations(results)
  } else if (create_visualizations) {
    cat("\nStep 2: Creating enhanced win probability visualizations from saved results...\n")
    viz_data <- create_wp_visualizations()
  } else {
    cat("\nSkipping visualization step...\n")
  }
  
  # Step 3: Analyze critical situations with academic styling
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
    if (run_analysis) {
      # Run the full comparison if we're doing a fresh analysis
      comparison <- compare_wp_vs_ep()
    } else {
      # Otherwise, just create the enhanced visualizations
      tryCatch({
        comparison_results <- readRDS("wp_ep_comparison_results.rds")
        enhanced_comparison <- compare_wp_vs_ep(comparison_results)
      }, error = function(e) {
        cat("Could not load existing comparison results. Skipping enhanced visualization.\n")
      })
    }
  } else {
    cat("\nSkipping comparison with expected points...\n")
  }
  
  # Step 5: Create summary figure
  if (create_summary && !is.null(results)) {
    cat("\nStep 5: Creating summary figure for publication...\n")
    summary_fig <- create_summary_figure(results)
  } else if (create_summary) {
    cat("\nStep 5: Creating summary figure from saved results...\n")
    summary_fig <- create_summary_figure()
  } else {
    cat("\nSkipping summary figure creation...\n")
  }
  
  cat("\nWin Probability Analysis Complete!\n")
  cat("Enhanced visualizations have been saved to your working directory.\n")
}


# Example usage:
# Just create enhanced visualizations from existing results:
run_win_probability_analysis(run_analysis = FALSE)