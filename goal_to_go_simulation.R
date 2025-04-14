# goal_to_go_simulation.R
# Simulation for 4th and goal decision-making

# Load required packages
library(dplyr)
library(ggplot2)
library(tidyr)

# Source required files
source("goal_to_go_sampling.R")
source("goal_to_go_fg_model.R")

# Function to simulate a specific 4th and goal scenario
simulate_4th_and_goal <- function(ytg, score_diff = 0, time_remaining = 60, n_sims = 1000) {
  # Load goal-to-go models
  goal_models <- NULL
  tryCatch({
    goal_models <- readRDS("goal_to_go_models.rds")
  }, error = function(e) {
    cat("Could not load goal models, using defaults\n")
  })
  
  # Load field goal model
  fg_model <- NULL
  tryCatch({
    fg_model <- readRDS("goal_to_go_fg_model.rds")
  }, error = function(e) {
    cat("Could not load field goal model, using defaults\n")
  })
  
  # Simulate going for it
  go_for_it_results <- numeric(n_sims)
  for (i in 1:n_sims) {
    # Sample yards gained
    result <- sample_goal_to_go_yards(ytg, goal_models)
    
    if (result$yards >= ytg) {
      # Touchdown + extra point (simplified)
      go_for_it_results[i] <- 7
    } else if (result$event_type %in% c("fumble_lost", "interception")) {
      # Turnover - opponent gets the ball close to their goal line
      # This is slightly negative in expected points
      go_for_it_results[i] <- -2
    } else {
      # Failed attempt, turnover on downs
      go_for_it_results[i] <- 0
    }
  }
  
  # Simulate field goal
  fg_results <- numeric(n_sims)
  fg_prob <- predict_fg_success_goal_to_go(ytg, fg_model)
  
  for (i in 1:n_sims) {
    if (runif(1) < fg_prob) {
      # Made field goal
      fg_results[i] <- 3
    } else {
      # Missed field goal - opponent gets the ball
      fg_results[i] <- -1
    }
  }
  
  # Calculate expected points
  go_expected_pts <- mean(go_for_it_results)
  fg_expected_pts <- mean(fg_results)
  
  # Calculate win probability changes
  go_wp_change <- wp_change(score_diff, time_remaining, go_expected_pts)
  fg_wp_change <- wp_change(score_diff, time_remaining, fg_expected_pts)
  
  # Return results
  return(list(
    go_for_it = list(
      expected_pts = go_expected_pts,
      wp_change = go_wp_change,
      success_prob = mean(go_for_it_results == 7),
      results = go_for_it_results
    ),
    field_goal = list(
      expected_pts = fg_expected_pts,
      wp_change = fg_wp_change,
      success_prob = fg_prob,
      results = fg_results
    ),
    recommendation = ifelse(go_wp_change > fg_wp_change, "Go for it", "Kick field goal"),
    score_diff = score_diff,
    time_remaining = time_remaining,
    ytg = ytg
  ))
}

# Win probability change function (simplified model)
wp_change <- function(score_diff, time_remaining, expected_pts) {
  # Base model parameters (simplified)
  base_wp <- 0.5  # Start at 50% for neutral game state
  
  # Impact of score differential (each point worth about 4% win probability)
  score_impact <- score_diff * 0.04
  
  # Time remaining factor (points worth more later in the game)
  time_factor <- max(0.5, min(2, (60 - time_remaining) / 30))
  
  # Calculate impact of expected points
  pts_impact <- expected_pts * 0.04 * time_factor
  
  # Combine all factors (bound between 0 and 1)
  new_wp <- max(0, min(1, base_wp + score_impact + pts_impact))
  
  # Return the change in win probability
  return(new_wp - (base_wp + score_impact))
}

# Function to run a comprehensive analysis across all scenarios
run_comprehensive_goal_to_go_analysis <- function() {
  # Setup grid of scenarios
  distances <- 1:10  # 1 to 10 yards to go
  score_diffs <- c(-14, -10, -7, -4, -3, -2, -1, 0, 1, 2, 3, 4, 7, 10, 14)  # Various score differences
  time_periods <- c(55, 40, 20, 10, 5, 2)  # Minutes remaining (various game situations)
  
  # Store results
  all_results <- list()
  
  # Loop through all scenarios
  for (ytg in distances) {
    ytg_results <- list()
    cat("Analyzing yards to go:", ytg, "\n")
    
    for (diff in score_diffs) {
      diff_results <- list()
      
      for (time in time_periods) {
        # Run simulation
        result <- simulate_4th_and_goal(ytg, diff, time)
        diff_results[[paste("time", time)]] <- result
      }
      
      ytg_results[[paste("diff", diff)]] <- diff_results
    }
    
    all_results[[paste("ytg", ytg)]] <- ytg_results
  }
  
  # Save all results
  saveRDS(all_results, "4th_and_goal_analysis.rds")
  
  return(all_results)
}

# Create visualizations of analysis results
create_goal_to_go_visualizations <- function(results = NULL) {
  # Load results if not provided
  if (is.null(results)) {
    tryCatch({
      results <- readRDS("4th_and_goal_analysis.rds")
    }, error = function(e) {
      cat("Could not load analysis results:", e$message, "\n")
      return(FALSE)
    })
  }
  
  # Extract and organize data for plotting
  plot_data <- data.frame()
  
  # Loop through all scenarios and extract recommendations
  for (ytg_key in names(results)) {
    ytg <- as.numeric(gsub("ytg ", "", ytg_key))
    
    for (diff_key in names(results[[ytg_key]])) {
      score_diff <- as.numeric(gsub("diff ", "", diff_key))
      
      for (time_key in names(results[[ytg_key]][[diff_key]])) {
        time_remaining <- as.numeric(gsub("time ", "", time_key))
        
        # Get result for this scenario
        result <- results[[ytg_key]][[diff_key]][[time_key]]
        
        # Add row to plot data
        plot_data <- rbind(plot_data, data.frame(
          ytg = ytg,
          score_diff = score_diff,
          time_remaining = time_remaining,
          go_expected_pts = result$go_for_it$expected_pts,
          fg_expected_pts = result$field_goal$expected_pts,
          go_wp_change = result$go_for_it$wp_change,
          fg_wp_change = result$field_goal$wp_change,
          go_success_prob = result$go_for_it$success_prob,
          fg_success_prob = result$field_goal$success_prob,
          recommendation = result$recommendation
        ))
      }
    }
  }
  
  # Create decision charts
  # 1. Chart by yards to go and score differential
  for (time in unique(plot_data$time_remaining)) {
    time_data <- subset(plot_data, time_remaining == time)
    
    p1 <- ggplot(time_data, aes(x = ytg, y = score_diff, fill = recommendation)) +
      geom_tile() +
      scale_fill_manual(values = c("Go for it" = "darkgreen", "Kick field goal" = "darkblue")) +
      labs(title = paste("4th & Goal Decision Chart - Time Remaining:", time, "min"),
           x = "Yards to Go",
           y = "Score Differential",
           fill = "Recommendation") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    # Save the plot
    ggsave(paste0("decision_chart_time_", time, ".png"), p1, width = 10, height = 8)
  }
  
  # 2. Chart for different score scenarios
  for (diff in c(-7, -3, 0, 3, 7)) {
    diff_data <- subset(plot_data, score_diff == diff)
    
    p2 <- ggplot(diff_data, aes(x = ytg, y = time_remaining, fill = recommendation)) +
      geom_tile() +
      scale_fill_manual(values = c("Go for it" = "darkgreen", "Kick field goal" = "darkblue")) +
      labs(title = paste("4th & Goal Decision Chart - Score Differential:", diff),
           x = "Yards to Go",
           y = "Time Remaining (minutes)",
           fill = "Recommendation") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    # Save the plot
    ggsave(paste0("decision_chart_diff_", diff, ".png"), p2, width = 10, height = 8)
  }
  
  # 3. Expected points comparison chart
  avg_data <- plot_data %>%
    group_by(ytg) %>%
    summarize(
      go_expected_pts = mean(go_expected_pts),
      fg_expected_pts = mean(fg_expected_pts),
      go_success_prob = mean(go_success_prob),
      fg_success_prob = mean(fg_success_prob)
    )
  
  # Reshape for plotting
  pts_data <- avg_data %>%
    select(ytg, go_expected_pts, fg_expected_pts) %>%
    pivot_longer(cols = c(go_expected_pts, fg_expected_pts),
                 names_to = "strategy", 
                 values_to = "expected_pts")
  
  p3 <- ggplot(pts_data, aes(x = ytg, y = expected_pts, color = strategy, group = strategy)) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    scale_color_manual(values = c("go_expected_pts" = "darkgreen", "fg_expected_pts" = "darkblue"),
                       labels = c("Go for it", "Field goal")) +
    labs(title = "Expected Points by Strategy and Distance",
         x = "Yards to Go",
         y = "Expected Points",
         color = "Strategy") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggsave("expected_points_comparison.png", p3, width = 10, height = 6)
  
  # Return plot data for further analysis
  return(plot_data)
}

# Run a simple test of the system
run_simple_test <- function() {
  cat("Testing 4th and goal decision model...\n")
  
  # Test a few scenarios
  scenarios <- list(
    list(ytg = 1, score_diff = 0, time = 10),
    list(ytg = 2, score_diff = 0, time = 10),
    list(ytg = 5, score_diff = 0, time = 10),
    list(ytg = 10, score_diff = 0, time = 10),
    list(ytg = 3, score_diff = -7, time = 5),
    list(ytg = 3, score_diff = 7, time = 5)
  )
  
  for (s in scenarios) {
    cat("\nScenario: 4th and goal from the", s$ytg, "yard line, score differential:", 
        s$score_diff, ", time remaining:", s$time, "minutes\n")
    
    result <- simulate_4th_and_goal(s$ytg, s$score_diff, s$time)
    
    cat("Expected points if going for it:", round(result$go_for_it$expected_pts, 2), "\n")
    cat("TD success probability:", round(result$go_for_it$success_prob * 100, 1), "%\n")
    cat("Expected points if kicking field goal:", round(result$field_goal$expected_pts, 2), "\n")
    cat("FG success probability:", round(result$field_goal$success_prob * 100, 1), "%\n")
    cat("Recommendation:", result$recommendation, "\n")
  }
  
  cat("\nTest complete!\n")
}

# If this script is run directly, perform a simple test
if (!interactive()) {
  run_simple_test()
}