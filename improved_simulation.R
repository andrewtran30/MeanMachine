# Function to simulate a specific 4th and goal scenario with improved game context
simulate_4th_and_goal2 <- function(ytg, score_diff = 0, time_remaining = 60, n_sims = 1000) {
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
  
  # Calculate win probability changes using improved model
  go_wp_change <- improved_wp_change(score_diff, time_remaining, go_for_it_results)
  fg_wp_change <- improved_wp_change(score_diff, time_remaining, fg_results)
  
  # Game context logic
  recommendation <- determine_recommendation(score_diff, time_remaining, 
                                             go_wp_change, fg_wp_change, 
                                             go_expected_pts, fg_expected_pts,
                                             go_for_it_results, fg_results)
  
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
    recommendation = recommendation,
    score_diff = score_diff,
    time_remaining = time_remaining,
    ytg = ytg
  ))
}

# Improved win probability change function with better game context
improved_wp_change <- function(score_diff, time_remaining, result_points) {
  # Calculate current win probability
  current_wp <- base_win_probability(score_diff, time_remaining)
  
  # For each possible outcome, calculate new win probability
  new_wps <- numeric(length(result_points))
  for (i in seq_along(result_points)) {
    # New score differential after play
    new_score_diff <- score_diff + result_points[i]
    
    # New time remaining (approximate using average play duration)
    new_time_remaining <- max(0, time_remaining - 0.5)  # Deduct 30 seconds for the play
    
    # Calculate new win probability
    new_wps[i] <- base_win_probability(new_score_diff, new_time_remaining)
  }
  
  # Return average change in win probability
  return(mean(new_wps) - current_wp)
}

# Base win probability function - more sophisticated model
base_win_probability <- function(score_diff, time_remaining) {
  # Start with neutral win probability
  base_wp <- 0.5
  
  # Impact of score differential
  if (time_remaining <= 0) {
    # Game is over, win probability is 0 or 1
    return(ifelse(score_diff > 0, 1, ifelse(score_diff < 0, 0, 0.5)))
  }
  
  # Time factor - score difference matters more as time decreases
  time_factor <- min(5, 1 + (60 - time_remaining) / 15)
  
  # Calculate score impact
  # More sophisticated than linear relationship - diminishing returns for large leads
  score_impact <- (1 - exp(-abs(score_diff) / 7)) * sign(score_diff) * 0.5
  
  # Adjust for end-game scenarios
  if (time_remaining < 5) {
    # Very late game - score differential is critical
    if (score_diff > 0) {
      # Leading - higher win probability
      score_impact <- min(0.5, score_impact * 1.5)
    } else if (score_diff < 0) {
      # Trailing - lower win probability
      score_impact <- max(-0.5, score_impact * 1.5)
    }
  }
  
  # Combine factors and ensure between 0 and 1
  wp <- max(0, min(1, base_wp + score_impact))
  
  return(wp)
}

# Function to determine recommendation based on game context
determine_recommendation <- function(score_diff, time_remaining, 
                                     go_wp_change, fg_wp_change, 
                                     go_expected_pts, fg_expected_pts,
                                     go_results, fg_results) {
  # First consider critical game situations
  
  # 1. Down by 4-8 points late in game - need a touchdown
  if (score_diff <= -4 && score_diff >= -8 && time_remaining <= 5) {
    return("Go for it")  # Must go for TD when down by 4-8 points late
  }
  
  # 2. Down by 1-2 points with very little time - field goal to take the lead
  if (score_diff >= -2 && score_diff <= -1 && time_remaining <= 2) {
    # Calculate FG success probability (made a field goal)
    fg_success_prob <- mean(fg_results == 3)
    
    # If high probability of success, kick it
    if (fg_success_prob > 0.8) {
      return("Kick field goal")
    }
  }
  
  
  # For non-critical situations, use win probability
  if (go_wp_change > fg_wp_change) {
    return("Go for it")
  } else {
    return("Kick field goal")
  }
}