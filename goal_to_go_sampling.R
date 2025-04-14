# goal_to_go_sampling.R
# Functions for sampling yards gained in goal-to-go situations

# Main function to sample yards gained in a goal-to-go situation
sample_goal_to_go_yards <- function(ytg, models = NULL) {
  # Load models if not provided
  if (is.null(models)) {
    tryCatch({
      models <- readRDS("goal_to_go_models.rds")
    }, error = function(e) {
      cat("Could not load goal to go models:", e$message, "\n")
      cat("Using default parameters\n")
      models <- NULL
    })
  }
  
  # Step 1: Sample play type (run vs pass)
  play_type <- sample_goal_to_go_play_type(ytg, models)
  
  # Step 2: Check for special events
  event <- check_goal_to_go_special_events(play_type, ytg, models)
  
  if (event$type != "normal") {
    return(list(
      yards = event$yards,
      event_type = event$type,
      play_type = play_type
    ))
  }
  
  # Step 3: Sample yards gained
  yards <- sample_goal_to_go_from_mixture(play_type, ytg, models)
  
  # Ensure yards don't exceed ytg (can't go beyond goal line)
  yards <- min(yards, ytg)
  
  return(list(
    yards = yards,
    event_type = "normal",
    play_type = play_type
  ))
}

# Sample play type (run vs pass) for goal-to-go situations
sample_goal_to_go_play_type <- function(ytg, models) {
  # Default pass probabilities by yards to go
  default_pass_probs <- c(
    0.3,  # 1 yard to go
    0.4,  # 2 yards to go
    0.5,  # 3 yards to go
    0.6,  # 4 yards to go
    0.7,  # 5 yards to go
    0.75, # 6 yards to go
    0.8,  # 7 yards to go
    0.8,  # 8 yards to go
    0.85, # 9 yards to go
    0.9   # 10 yards to go
  )
  
  # Get pass probability for this ytg
  if (!is.null(models) && "event_rates" %in% names(models)) {
    rate_key <- paste("pass_prob_ytg", min(ytg, 10))
    if (rate_key %in% names(models[["event_rates"]])) {
      pass_prob <- models[["event_rates"]][[rate_key]]
    } else {
      pass_prob <- default_pass_probs[min(ytg, 10)]
    }
  } else {
    pass_prob <- default_pass_probs[min(ytg, 10)]
  }
  
  # Sample play type
  if (runif(1) < pass_prob) {
    return("pass")
  } else {
    return("run")
  }
}

# Check for special events (turnovers, incompletions) in goal-to-go situations
check_goal_to_go_special_events <- function(play_type, ytg, models) {
  # Default rates
  default_rates <- list(
    fumble_rate = 0.02,
    interception_rate = 0.03,
    incompletion_rate = 0.4
  )
  
  # Get actual rates if available
  rates <- default_rates
  if (!is.null(models) && "event_rates" %in% names(models)) {
    for (rate_name in names(default_rates)) {
      if (rate_name %in% names(models[["event_rates"]])) {
        rates[[rate_name]] <- models[["event_rates"]][[rate_name]]
      }
    }
  }
  
  # Check for events based on play type
  if (play_type == "run") {
    # Check for fumble
    if (runif(1) < rates$fumble_rate) {
      return(list(type = "fumble_lost", yards = 0))
    }
  } else {  # pass play
    # Check for interception
    if (runif(1) < rates$interception_rate) {
      return(list(type = "interception", yards = 0))
    }
    
    # Check for incompletion
    if (runif(1) < rates$incompletion_rate) {
      return(list(type = "incompletion", yards = 0))
    }
  }
  
  # No special event
  return(list(type = "normal", yards = 0))
}

# Sample yards gained from mixture model for goal-to-go situations
sample_goal_to_go_from_mixture <- function(play_type, ytg, models) {
  # Try to use fitted models if available
  ytg <- min(ytg, 10)  # Cap at 10 yards
  model_key <- paste(play_type, "ytg", ytg)
  
  if (!is.null(models) && model_key %in% names(models)) {
    model <- models[[model_key]]
    
    # Sample from mixture model
    component <- sample(1:length(model$lambda), 1, prob = model$lambda)
    yards <- round(rnorm(1, model$mu[component], model$sigma[component]))
    
    # Ensure yards are within reasonable bounds
    yards <- max(-5, min(ytg, yards))
    return(yards)
  } else {
    # Use defaults if no model available
    if (play_type == "run") {
      # Default run distribution for goal to go
      components <- c(0.4, 0.4, 0.2)
      means <- c(-1, 2, ytg * 0.7)  # Scale based on ytg
      sds <- c(1, 1.5, 2)
    } else {  # pass play
      # Default pass distribution for goal to go
      components <- c(0.5, 0.3, 0.2)
      means <- c(0, ytg * 0.4, ytg * 0.8)  # Scale based on ytg
      sds <- c(1.5, 2, 3)
    }
    
    # Sample from mixture
    component <- sample(1:length(components), 1, prob = components)
    yards <- round(rnorm(1, means[component], sds[component]))
    
    # Ensure yards are within reasonable bounds
    yards <- max(-5, min(ytg, yards))
    return(yards)
  }
}
