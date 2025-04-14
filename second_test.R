# Test the improved 4th and goal simulation model with realistic game contexts

# Update the simulation function with our improved version
source("improved_simulation.R")

# Test critical scenarios
test_critical_scenarios <- function() {
  cat("Testing Critical Game Scenarios\n")
  cat("------------------------------\n\n")
  
  # Scenario 1: Down by 4 late in game (should go for TD)
  cat("Scenario 1: Down by 4 points with 1 minute left on the 3 yard line\n")
  result1 <- simulate_4th_and_goal(ytg = 3, score_diff = -4, time_remaining = 1)
  cat("Expected points (go for it):", round(result1$go_for_it$expected_pts, 2), "\n")
  cat("Expected points (field goal):", round(result1$field_goal$expected_pts, 2), "\n")
  cat("Win probability change (go for it):", round(result1$go_for_it$wp_change, 4), "\n")
  cat("Win probability change (field goal):", round(result1$field_goal$wp_change, 4), "\n")
  cat("Recommendation:", result1$recommendation, "\n\n")
  
  # Scenario 2: Down by 2 with very little time (should kick FG to take lead)
  cat("Scenario 2: Down by 2 points with 30 seconds left on the 2 yard line\n")
  result2 <- simulate_4th_and_goal(ytg = 2, score_diff = -2, time_remaining = 0.5)
  cat("Expected points (go for it):", round(result2$go_for_it$expected_pts, 2), "\n")
  cat("Expected points (field goal):", round(result2$field_goal$expected_pts, 2), "\n")
  cat("Win probability change (go for it):", round(result2$go_for_it$wp_change, 4), "\n")
  cat("Win probability change (field goal):", round(result2$field_goal$wp_change, 4), "\n")
  cat("Recommendation:", result2$recommendation, "\n\n")
  
  # Scenario 3: Down by 10 in 4th quarter (should go for TD)
  cat("Scenario 3: Down by 10 points with 12 minutes left on the 5 yard line\n")
  result3 <- simulate_4th_and_goal(ytg = 5, score_diff = -10, time_remaining = 12)
  cat("Expected points (go for it):", round(result3$go_for_it$expected_pts, 2), "\n")
  cat("Expected points (field goal):", round(result3$field_goal$expected_pts, 2), "\n")
  cat("Win probability change (go for it):", round(result3$go_for_it$wp_change, 4), "\n")
  cat("Win probability change (field goal):", round(result3$field_goal$wp_change, 4), "\n")
  cat("Recommendation:", result3$recommendation, "\n\n")
  
  # Scenario, 4: Up by 2 late in game (should kick FG to extend lead)
  cat("Scenario 4: Up by 2 points with 40 seconds left on the 2 yard line\n")
  result4 <- simulate_4th_and_goal(ytg = 2, score_diff = 2, time_remaining = 0.67)
  cat("Expected points (go for it):", round(result4$go_for_it$expected_pts, 2), "\n")
  cat("Expected points (field goal):", round(result4$field_goal$expected_pts, 2), "\n")
  cat("Win probability change (go for it):", round(result4$go_for_it$wp_change, 4), "\n")
  cat("Win probability change (field goal):", round(result4$field_goal$wp_change, 4), "\n")
  cat("Recommendation:", result4$recommendation, "\n\n")
  
  # Scenario 5: Neutral situation, 1st quarter (should just use WP)
  cat("Scenario 5: Tied game in 1st quarter on the 1 yard line\n")
  result5 <- simulate_4th_and_goal(ytg = 1, score_diff = 0, time_remaining = 50)
  cat("Expected points (go for it):", round(result5$go_for_it$expected_pts, 2), "\n")
  cat("Expected points (field goal):", round(result5$field_goal$expected_pts, 2), "\n")
  cat("Win probability change (go for it):", round(result5$go_for_it$wp_change, 4), "\n")
  cat("Win probability change (field goal):", round(result5$field_goal$wp_change, 4), "\n")
  cat("Recommendation:", result5$recommendation, "\n\n")
}

# Run the tests
test_critical_scenarios()

# Create a function to compare old and new models
compare_models <- function() {
  cat("\nComparing Original vs. Improved Model\n")
  cat("----------------------------------\n\n")
  
  # Reference to the original simulation function
  original_simulate <- function(ytg, score_diff, time_remaining) {
    # Load the original wp_change function
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
    
    # Run simulation with minimal calculations (for comparison only)
    result <- list(
      go_for_it = list(expected_pts = 4.2 * exp(-ytg/7)),  # Approximate
      field_goal = list(expected_pts = 3 * (0.95 - 0.02 * ytg))  # Approximate
    )
    
    # Calculate WP changes
    result$go_for_it$wp_change <- wp_change(score_diff, time_remaining, result$go_for_it$expected_pts)
    result$field_goal$wp_change <- wp_change(score_diff, time_remaining, result$field_goal$expected_pts)
    
    # Determine recommendation based on expected points
    if (result$go_for_it$expected_pts > result$field_goal$expected_pts) {
      result$recommendation <- "Go for it"
    } else {
      result$recommendation <- "Kick field goal"
    }
    
    return(result)
  }
  
  # Test scenarios where we expect differences
  scenarios <- list(
    list(name = "Down by 4, late game", ytg = 3, score_diff = -4, time = 1),
    list(name = "Down by 2, very late game", ytg = 2, score_diff = -2, time = 0.5),
    list(name = "Down by 3, medium distance", ytg = 5, score_diff = -3, time = 30),
    list(name = "Up by 1, late game", ytg = 4, score_diff = 1, time = 1),
    list(name = "Down by 9, 4th quarter", ytg = 3, score_diff = -9, time = 12)
  )
  
  for (s in scenarios) {
    cat("Scenario:", s$name, "\n")
    cat("  Yards to go:", s$ytg, ", Score diff:", s$score_diff, ", Time:", s$time, "min\n")
    
    # Get recommendations from both models
    orig <- simulate_4th_and_goal(s$ytg, s$score_diff, s$time)
    improved <- simulate_4th_and_goal2(s$ytg, s$score_diff, s$time)
    
    cat("  Original model recommendation:", orig$recommendation, "\n")
    cat("  Improved model recommendation:", improved$recommendation, "\n")
    
    if (orig$recommendation != improved$recommendation) {
      cat("  DIFFERENCE DETECTED!\n")
    }
    cat("\n")
  }
}

# Run the comparison
compare_models()