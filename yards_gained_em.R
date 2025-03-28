library(mixtools)  # For EM algorithm if needed

# Global variables to prevent repeated loading
fitted_models <- NULL
has_fitted_models <- FALSE
has_tried_loading <- FALSE

#' Sample yards gained using a mixture model approach
#'
#' @param down Current down (1-4)
#' @param ytg Yards to go
#' @param fp Field position (0-120)
#' @return List containing yards gained and event type
sample_yards_gained <- function(down, ytg, fp) {
  # Load fitted models if needed (only try once)
  if (!has_tried_loading) {
    tryCatch({
      fitted_models <<- readRDS("fitted_mixture_models.rds")
      cat("Loaded fitted mixture models\n")
      has_fitted_models <<- TRUE
    }, error = function(e) {
      cat("Could not load fitted models:", e$message, "\n")
      cat("Using default parameters\n")
      has_fitted_models <<- FALSE
    })
    has_tried_loading <<- TRUE
  }
  
  # Step 1: Determine field zone
  zone <- determine_zone(fp)
  
  # Step 2: Determine play type (run vs pass)
  play_type <- sample_play_type(down, ytg, fp, zone)
  
  # Step 3: Check for special events (turnovers, incompletions)
  special_event <- check_special_events(play_type, down, ytg, fp, zone)
  
  if (special_event$event != "none") {
    return(list(
      yards = special_event$yards,
      event_type = special_event$event
    ))
  }
  
  # Step 4: Sample yards gained from appropriate mixture model
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
  # Try to use fitted rates if available
  rate_key <- paste("pass_prob_down", down, "_zone_", zone)
  
  if (has_fitted_models && !is.null(fitted_models) && 
      "event_rates" %in% names(fitted_models) && 
      rate_key %in% names(fitted_models[["event_rates"]])) {
    pass_prob <- fitted_models[["event_rates"]][[rate_key]]
  } else {
    # Default behavior
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
    # Check for fumble (increases with down)
    fumble_rate_key <- paste("fumble_rate_down", down)
    
    if (has_fitted_models && !is.null(fitted_models) && 
        "event_rates" %in% names(fitted_models) && 
        fumble_rate_key %in% names(fitted_models[["event_rates"]])) {
      fumble_prob <- fitted_models[["event_rates"]][[fumble_rate_key]]
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
    # Check for interception (increases with down and ytg)
    int_rate_key <- paste("interception_rate_down", down)
    
    if (has_fitted_models && !is.null(fitted_models) && 
        "event_rates" %in% names(fitted_models) && 
        int_rate_key %in% names(fitted_models[["event_rates"]])) {
      int_prob <- fitted_models[["event_rates"]][[int_rate_key]]
    } else {
      int_prob <- 0.01 + (down * 0.005) + (min(ytg, 20) * 0.001)  # Default
    }
    
    if (runif(1) < int_prob) {
      return(list(
        event = "interception",
        yards = 0
      ))
    }
    
    # Check for incompletion (decreases with down)
    comp_rate_key <- paste("completion_rate_down", down)
    
    if (has_fitted_models && !is.null(fitted_models) && 
        "event_rates" %in% names(fitted_models) && 
        comp_rate_key %in% names(fitted_models[["event_rates"]])) {
      comp_prob <- fitted_models[["event_rates"]][[comp_rate_key]]
    } else {
      comp_prob <- 0.6 - (down * 0.05)  # Default: decreases with down
    }
    
    # Adjust for field position and yards to go
    if (zone == "red_zone") {
      comp_prob <- comp_prob - 0.05  # Harder to complete in red zone
    }
    if (ytg > 10) {
      comp_prob <- comp_prob - (min(ytg - 10, 10) * 0.01)  # Harder on longer throws
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

#' Sample yards from fitted or default mixture model
#'
#' @param play_type Play type (run or pass)
#' @param down Current down
#' @param zone Field zone
#' @param fp Field position
#' @return Numeric yards gained
sample_from_mixture <- function(play_type, down, zone, fp) {
  # Try to use fitted models if available
  if (has_fitted_models && !is.null(fitted_models)) {
    # First try specific model for this combination
    model_key <- paste(play_type, "down", down, zone)
    
    if (model_key %in% names(fitted_models)) {
      model <- fitted_models[[model_key]]
      
      # Sample from fitted model
      component <- sample(1:length(model$lambda), 1, prob = model$lambda)
      yards <- round(rnorm(1, model$mu[component], model$sigma[component]))
      
      # Ensure yards are within reasonable bounds
      yards <- max(-10, min(99 - fp, yards))
      return(yards)
    }
    
    # If no specific model, try just play type and down
    model_key <- paste(play_type, "down", down)
    if (model_key %in% names(fitted_models)) {
      model <- fitted_models[[model_key]]
      
      # Sample from fitted model
      component <- sample(1:length(model$lambda), 1, prob = model$lambda)
      yards <- round(rnorm(1, model$mu[component], model$sigma[component]))
      
      # Ensure yards are within reasonable bounds
      yards <- max(-10, min(99 - fp, yards))
      return(yards)
    }
  }
  
  # Fall back to predefined mixture parameters
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
    
    # Adjust for down
    if (down >= 3) {
      # Later downs - shorter runs on average
      means <- means * 0.9
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
    
    # Adjust for down
    if (down >= 3) {
      # Later downs - longer passes on average
      means <- means * 1.1
      # But more variability
      sds <- sds * 1.2
    }
  }
  
  # Sample from mixture
  component <- sample(1:length(components), 1, prob = components)
  yards <- round(rnorm(1, means[component], sds[component]))
  
  # Ensure yards are within reasonable bounds
  yards <- max(-10, min(99 - fp, yards))
  
  return(yards)
}