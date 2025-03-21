# Implementation of mixture models for yards gained using NFL data

# Try to load fitted models and rates
tryCatch({
  fitted_models <- readRDS("nfl_mixture_models.rds")
  event_rates <- readRDS("nfl_event_rates.rds")
  cat("Loaded fitted mixture models and event rates from NFL data\n")
  have_fitted_models <- TRUE
}, error = function(e) {
  cat("Fitted models not found, using default parameters\n")
  have_fitted_models <- FALSE
})

#' Sample yards gained using NFL data-based mixture models
#'
#' @param down Current down (1-4)
#' @param ytg Yards to go
#' @param fp Field position (0-120)
#' @return List containing yards gained and event type
sample_yards_gained <- function(down, ytg, fp) {
  # Determine field zone
  zone <- determine_zone(fp)
  
  # Step 1: Determine play type (run vs pass)
  play_type <- sample_play_type(down, ytg, fp, zone)
  
  # Step 2: Check for special events (turnovers, incompletions)
  special_event <- check_special_events(play_type, down, ytg, fp, zone)
  
  if (special_event$event != "none") {
    return(list(
      yards = special_event$yards,
      event_type = special_event$event
    ))
  }
  
  # Step 3: Sample yards gained from appropriate mixture model
  yards <- sample_from_mixture(play_type, down, zone, fp)
  
  return(list(
    yards = yards,
    event_type = "normal"
  ))
}

#' Determine field zone based on field position
#'
#' @param fp Field position (0-120)
#' @return Character string indicating zone
determine_zone <- function(fp) {
  yardline_100 <- 100 - fp  # Convert to yards from opponent's end zone
  
  if (yardline_100 <= 20) {
    return("red_zone")
  } else if (yardline_100 >= 80) {
    return("own_redzone")
  } else {
    return("middle_field")
  }
}

#' Sample play type (run or pass) based on down, distance, field position
#'
#' @param down Current down (1-4)
#' @param ytg Yards to go
#' @param fp Field position
#' @param zone Field zone
#' @return Character string indicating play type
sample_play_type <- function(down, ytg, fp, zone) {
  # Use actual pass probabilities from NFL data if available
  if (exists("event_rates") && exists("have_fitted_models") && have_fitted_models) {
    rate_key <- paste("pass_prob_down", down, "_zone_", zone)
    
    if (rate_key %in% names(event_rates)) {
      pass_prob <- event_rates[[rate_key]]
    } else {
      # Fallback to generic model
      pass_prob <- 0.35 + (down * 0.08) + (min(ytg, 15) * 0.01)
      
      # Adjust for field position
      if (zone == "red_zone") {
        pass_prob <- pass_prob + 0.05  # More passing in red zone
      } else if (zone == "own_redzone") {
        pass_prob <- pass_prob - 0.1   # More conservative near own goal
      }
    }
  } else {
    # Fallback to generic model
    pass_prob <- 0.35 + (down * 0.08) + (min(ytg, 15) * 0.01)
    
    # Adjust for field position
    if (zone == "red_zone") {
      pass_prob <- pass_prob + 0.05  # More passing in red zone
    } else if (zone == "own_redzone") {
      pass_prob <- pass_prob - 0.1   # More conservative near own goal
    }
  }
  
  # Ensure probability is in valid range
  pass_prob <- max(0.2, min(0.75, pass_prob))
  
  # Sample play type
  if (runif(1) < pass_prob) {
    return("pass")
  } else {
    return("run")
  }
}

#' Check for special events (turnovers, incompletions)
#'
#' @param play_type Play type (run or pass)
#' @param down Current down
#' @param ytg Yards to go
#' @param fp Field position
#' @param zone Field zone
#' @return List with event type and yards
check_special_events <- function(play_type, down, ytg, fp, zone) {
  if (play_type == "run") {
    # Check for fumble
    fumble_rate_key <- paste("fumble_rate_down", down)
    
    # Use actual fumble rates if available, otherwise use defaults
    if (exists("event_rates") && exists("have_fitted_models") && have_fitted_models && 
        fumble_rate_key %in% names(event_rates)) {
      fumble_prob <- event_rates[[fumble_rate_key]]
    } else {
      fumble_prob <- 0.01 + (down * 0.003)  # Default: increases with down
    }
    
    if (runif(1) < fumble_prob) {
      return(list(
        event = "fumble_lost",
        yards = sample(-3:2, 1)  # Small range of yards on fumble
      ))
    }
  } else {  # pass play
    # Check for interception
    int_rate_key <- paste("interception_rate_down", down)
    
    # Use actual interception rates if available, otherwise use defaults
    if (exists("event_rates") && exists("have_fitted_models") && have_fitted_models && 
        int_rate_key %in% names(event_rates)) {
      int_prob <- event_rates[[int_rate_key]]
    } else {
      int_prob <- 0.01 + (down * 0.005) + (min(ytg, 20) * 0.001)  # Default: increases with down and ytg
    }
    
    if (runif(1) < int_prob) {
      return(list(
        event = "interception",
        yards = 0
      ))
    }
    
    # Check for incompletion
    comp_rate_key <- paste("completion_rate_down", down)
    
    # Use actual completion rates if available, otherwise use defaults
    if (exists("event_rates") && exists("have_fitted_models") && have_fitted_models && 
        comp_rate_key %in% names(event_rates)) {
      comp_prob <- event_rates[[comp_rate_key]]
    } else {
      comp_prob <- 0.6 - (down * 0.05)  # Default: decreases with down
    }
    
    # Adjust for field position and yards to go
    if (zone == "red_zone") {
      comp_prob <- comp_prob - 0.05  # Harder to complete in red zone
    }
    if (ytg > 10) {
      comp_prob <- comp_prob - (min(ytg - 10, 10) * 0.01)  # Harder to complete on longer throws
    }
    
    # Ensure probability is in valid range
    comp_prob <- max(0.3, min(0.8, comp_prob))
    
    if (runif(1) > comp_prob) {  # If not completed
      return(list(
        event = "incompletion",
        yards = 0
      ))
    }
  }
  
  # No special event
  return(list(
    event = "none",
    yards = 0
  ))
}

#' Sample yards from fitted NFL mixture model
#'
#' @param play_type Play type (run or pass)
#' @param down Current down
#' @param zone Field zone
#' @param fp Field position
#' @return Numeric yards gained
sample_from_mixture <- function(play_type, down, zone, fp) {
  # Try to use fitted models if available
  if (exists("fitted_models") && exists("have_fitted_models") && have_fitted_models) {
    # Construct the key for the fitted model
    model_key <- paste(play_type, "down", down, zone)
    
    # Check if we have a fitted model for this specific combination
    if (model_key %in% names(fitted_models) && !is.null(fitted_models[[model_key]])) {
      model <- fitted_models[[model_key]]
      
      # Sample from fitted model
      component <- sample(1:length(model$lambda), 1, prob = model$lambda)
      yards <- round(rnorm(1, model$mu[component], model$sigma[component]))
      
      # Ensure yards are within reasonable bounds
      yards <- max(-10, min(min(99, 100 - fp), yards))
      return(yards)
    }
    
    # If no specific model, try just play type and down
    model_key <- paste(play_type, "down", down)
    if (model_key %in% names(fitted_models) && !is.null(fitted_models[[model_key]])) {
      model <- fitted_models[[model_key]]
      
      # Sample from fitted model
      component <- sample(1:length(model$lambda), 1, prob = model$lambda)
      yards <- round(rnorm(1, model$mu[component], model$sigma[component]))
      
      # Ensure yards are within reasonable bounds
      yards <- max(-10, min(min(99, 100 - fp), yards))
      return(yards)
    }
  }
  
  # If we don't have fitted models or couldn't find an appropriate one,
  # use default parameters based on play type and zone
  
  # Define mixture components for different scenarios
  if (play_type == "run") {
    if (zone == "red_zone") {
      # Red zone runs: mostly short gains, occasional TD
      components <- c(0.35, 0.45, 0.2)
      means <- c(-1, 3, 8)
      sds <- c(1.5, 2, 3)
    } else if (zone == "own_redzone") {
      # Own red zone runs: conservative
      components <- c(0.3, 0.5, 0.2)
      means <- c(1, 4, 8)
      sds <- c(1.5, 2, 4)
    } else {  # middle field
      # Middle field runs: standard distribution
      components <- c(0.25, 0.5, 0.25)
      means <- c(0, 4, 12)
      sds <- c(2, 3, 6)
    }
  } else {  # pass play
    if (zone == "red_zone") {
      # Red zone passes: shorter, precise
      components <- c(0.4, 0.4, 0.2)
      means <- c(2, 7, 15)
      sds <- c(2, 3, 4)
    } else if (zone == "own_redzone") {
      # Own red zone passes: mix of safe and medium
      components <- c(0.3, 0.5, 0.2)
      means <- c(4, 10, 20)
      sds <- c(2, 4, 8)
    } else {  # middle field
      # Middle field passes: wide range, including deep passes
      components <- c(0.2, 0.5, 0.3)
      means <- c(3, 12, 25)
      sds <- c(3, 5, 10)
    }
  }
  
  # Sample from mixture
  component <- sample(1:length(components), 1, prob = components)
  yards <- round(rnorm(1, means[component], sds[component]))
  
  # Ensure yards are within reasonable bounds
  yards <- max(-10, min(min(99, 100 - fp), yards))
  
  return(yards)
}