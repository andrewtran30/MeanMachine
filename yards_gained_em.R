# yards_gained_em.R
# Mixture model implementation for football yards gained using EM algorithm when possible

library(mixtools)  # For EM algorithm

# Global variable to store fitted models
fitted_models <- list()
has_fitted_models <- FALSE

#' Try to fit mixture model to NFL data and cache the results
#' This is run once at the beginning to try to fit models
init_mixture_models <- function() {
  # Check if we've already tried to fit models
  if (length(fitted_models) > 0) {
    return(has_fitted_models)
  }
  
  # Try to load NFL data
  data_loaded <- FALSE
  tryCatch({
    football_data <- readRDS("football_data.rds")
    data_loaded <- TRUE
    cat("Loaded football data for EM fitting\n")
  }, error = function(e) {
    cat("Could not load football data: ", e$message, "\n")
    data_loaded <- FALSE
  })
  
  if (!data_loaded) {
    cat("No data available for EM fitting, using predefined parameters\n")
    return(FALSE)
  }
  
  # Extract only run and pass plays
  if (!"play_type" %in% colnames(football_data)) {
    cat("No play type column found in data, using predefined parameters\n")
    return(FALSE)
  }
  
  run_pass_plays <- subset(football_data, play_type %in% c("run", "pass"))
  
  # Create field position zones if yardline_100 is available
  if ("yardline_100" %in% colnames(run_pass_plays)) {
    run_pass_plays$field_zone <- cut(run_pass_plays$yardline_100, 
                                     breaks = c(0, 20, 80, 100),
                                     labels = c("red_zone", "middle_field", "own_redzone"))
  } else {
    cat("No yardline_100 column found, using predefined parameters\n")
    return(FALSE)
  }
  
  # Function to fit a single mixture model
  fit_single_model <- function(data, name, k = 3, max_attempts = 3) {
    if (length(data) < 20) {
      cat("Not enough data for", name, "\n")
      return(NULL)
    }
    
    # Remove extreme outliers
    data <- data[data > quantile(data, 0.001) & data < quantile(data, 0.999)]
    
    # Add tiny noise to prevent identical values
    data <- data + rnorm(length(data), 0, 0.01)
    
    for (attempt in 1:max_attempts) {
      tryCatch({
        # Use custom starting values
        lambdas <- rep(1/k, k)
        mus <- quantile(data, seq(0.1, 0.9, length.out = k))
        sigmas <- rep(sd(data)/sqrt(k), k)
        
        # Run EM algorithm
        model <- normalmixEM(data, k = k, lambda = lambdas, mu = mus, sigma = sigmas,
                             epsilon = 1e-3, maxit = 50, verb = FALSE)
        
        cat("Successfully fit model for", name, "\n")
        return(model)
      }, error = function(e) {
        cat("EM fitting failed on attempt", attempt, "for", name, ":", e$message, "\n")
      }, warning = function(w) {
        cat("Warning on attempt", attempt, "for", name, ":", w$message, "\n")
      })
    }
    
    cat("All fitting attempts failed for", name, "\n")
    return(NULL)
  }
  
  # Try to fit models for different combinations
  cat("Attempting to fit mixture models using EM algorithm...\n")
  success_count <- 0
  
  # For each play type
  for (play_type in c("run", "pass")) {
    # For each down
    for (down in 1:4) {
      # Get data for this play type and down
      down_data <- subset(run_pass_plays, play_type == play_type & down == down)
      
      if (nrow(down_data) < 20) {
        cat("Not enough data for", play_type, "plays on down", down, "\n")
        next
      }
      
      # Try to fit overall model for this play type and down
      model_key <- paste(play_type, "down", down)
      model <- fit_single_model(down_data$yards_gained, model_key)
      
      if (!is.null(model)) {
        fitted_models[[model_key]] <- list(
          lambda = model$lambda,
          mu = model$mu,
          sigma = model$sigma
        )
        success_count <- success_count + 1
      }
      
      # Try for each field zone if we have enough data
      for (zone in c("red_zone", "middle_field", "own_redzone")) {
        zone_data <- subset(down_data, field_zone == zone)
        
        if (nrow(zone_data) < 20) {
          cat("Not enough data for", play_type, "plays on down", down, "in", zone, "\n")
          next
        }
        
        model_key <- paste(play_type, "down", down, zone)
        model <- fit_single_model(zone_data$yards_gained, model_key)
        
        if (!is.null(model)) {
          fitted_models[[model_key]] <- list(
            lambda = model$lambda,
            mu = model$mu,
            sigma = model$sigma
          )
          success_count <- success_count + 1
        }
      }
    }
  }
  
  # Extract event rates
  extract_event_rates <- function() {
    rates <- list()
    
    # Try to extract fumble rates
    for (down in 1:4) {
      # Get fumble rate for runs
      run_data <- subset(run_pass_plays, play_type == "run" & down == down)
      if ("fumble_lost" %in% colnames(run_data)) {
        rates[[paste("fumble_rate_down", down)]] <- mean(run_data$fumble_lost, na.rm = TRUE)
      } else if ("fumble" %in% colnames(run_data)) {
        rates[[paste("fumble_rate_down", down)]] <- mean(run_data$fumble, na.rm = TRUE)
      } else {
        rates[[paste("fumble_rate_down", down)]] <- 0.01 + (down * 0.003)
      }
      
      # Get interception rate for passes
      pass_data <- subset(run_pass_plays, play_type == "pass" & down == down)
      if ("interception" %in% colnames(pass_data)) {
        rates[[paste("interception_rate_down", down)]] <- mean(pass_data$interception, na.rm = TRUE)
      } else {
        rates[[paste("interception_rate_down", down)]] <- 0.01 + (down * 0.005)
      }
      
      # Get completion rate for passes
      if ("complete_pass" %in% colnames(pass_data)) {
        rates[[paste("completion_rate_down", down)]] <- mean(pass_data$complete_pass, na.rm = TRUE)
      } else if ("incomplete_pass" %in% colnames(pass_data)) {
        rates[[paste("completion_rate_down", down)]] <- 1 - mean(pass_data$incomplete_pass, na.rm = TRUE)
      } else {
        rates[[paste("completion_rate_down", down)]] <- 0.6 - (down * 0.05)
      }
      
      # Get play type probabilities for each field zone
      for (zone in c("red_zone", "middle_field", "own_redzone")) {
        zone_data <- subset(run_pass_plays, down == down & field_zone == zone)
        if (nrow(zone_data) > 0) {
          rates[[paste("pass_prob_down", down, "_zone_", zone)]] <- 
            mean(zone_data$play_type == "pass", na.rm = TRUE)
        }
      }
    }
    
    fitted_models[["event_rates"]] <- rates
  }
  
  extract_event_rates()
  
  cat("Successfully fit", success_count, "mixture models\n")
  has_fitted_models <- success_count > 0
  return(has_fitted_models)
}

# Try to initialize models
has_fitted_models <- init_mixture_models()

#' Sample yards gained using a mixture model approach
#'
#' @param down Current down (1-4)
#' @param ytg Yards to go
#' @param fp Field position (0-120)
#' @return List containing yards gained and event type
sample_yards_gained <- function(down, ytg, fp) {
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
  
  if (has_fitted_models && "event_rates" %in% names(fitted_models) && 
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
    # Check for fumble
    fumble_rate_key <- paste("fumble_rate_down", down)
    
    if (has_fitted_models && "event_rates" %in% names(fitted_models) && 
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
    # Check for interception
    int_rate_key <- paste("interception_rate_down", down)
    
    if (has_fitted_models && "event_rates" %in% names(fitted_models) && 
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
    
    # Check for incompletion
    comp_rate_key <- paste("completion_rate_down", down)
    
    if (has_fitted_models && "event_rates" %in% names(fitted_models) && 
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
  if (has_fitted_models) {
    # First try specific model for this combination
    model_key <- paste(play_type, "down", down, zone)
    
    if (model_key %in% names(fitted_models)) {
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
    if (model_key %in% names(fitted_models)) {
      model <- fitted_models[[model_key]]
      
      # Sample from fitted model
      component <- sample(1:length(model$lambda), 1, prob = model$lambda)
      yards <- round(rnorm(1, model$mu[component], model$sigma[component]))
      
      # Ensure yards are within reasonable bounds
      yards <- max(-10, min(min(99, 100 - fp), yards))
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
  yards <- max(-10, min(min(99, 100 - fp), yards))
  
  return(yards)
}