# Step 1: Fit the mixture models to your football data
# Save this as fit_mixture_models.R

library(mixtools)
library(ggplot2)

fit_mixture_models <- function() {
  # Load the football data
  football_data <- readRDS("football_data.rds")
  cat("Loaded football data with", nrow(football_data), "rows\n")
  
  # Extract run and pass plays
  run_pass_plays <- subset(football_data, play_type %in% c("run", "pass"))
  cat("Found", nrow(run_pass_plays), "run and pass plays\n")
  
  # Create field zones based on yardline_100
  if ("yardline_100" %in% colnames(run_pass_plays)) {
    run_pass_plays$field_zone <- cut(run_pass_plays$yardline_100, 
                                     breaks = c(0, 20, 80, 100),
                                     labels = c("red_zone", "middle_field", "own_redzone"))
  } else {
    # Default if no field position info
    run_pass_plays$field_zone <- "middle_field"
  }
  
  # Function to fit a single mixture model safely
  fit_safely <- function(data, name, k = 3, max_attempts = 3) {
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
        
        # Run EM algorithm with protection against small variances
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
  
  # Function to visualize a fitted model
  visualize_model <- function(data, model, name) {
    if (is.null(model)) return(NULL)
    
    # Create data frame for plotting
    df <- data.frame(yards = data)
    
    # Create plot
    p <- ggplot(df, aes(x = yards)) +
      geom_histogram(aes(y = ..density..), binwidth = 1, fill = "lightblue", color = "black") +
      stat_function(fun = function(x) {
        result <- 0
        for (i in 1:length(model$lambda)) {
          result <- result + model$lambda[i] * dnorm(x, model$mu[i], model$sigma[i])
        }
        return(result)
      }, color = "red", size = 1) +
      labs(title = paste("Fitted Mixture Model for", name),
           x = "Yards Gained",
           y = "Density") +
      theme_minimal() +
      xlim(-10, 50)
    
    # Add individual components
    for (i in 1:length(model$lambda)) {
      p <- p + stat_function(fun = function(x) {
        model$lambda[i] * dnorm(x, model$mu[i], model$sigma[i])
      }, color = "blue", alpha = 0.5, linetype = "dashed")
    }
    
    # Save plot
    filename <- paste0(gsub(" ", "_", tolower(name)), "_fitted.png")
    ggsave(filename, p, width = 8, height = 6)
    cat("Plot saved as", filename, "\n")
    
    return(p)
  }
  
  # Store fitted models
  fitted_models <- list()
  
  # Fit models for different combinations
  for (play_type in c("run", "pass")) {
    for (down in 1:4) {
      # Get data for this play type and down
      this_data <- subset(run_pass_plays, 
                          play_type == play_type & 
                            down == down)
      
      if (nrow(this_data) < 20) {
        cat("Not enough data for", play_type, "on down", down, "\n")
        next
      }
      
      # Fit overall model
      name <- paste(play_type, "on down", down)
      model <- fit_safely(this_data$yards_gained, name)
      
      if (!is.null(model)) {
        fitted_models[[paste(play_type, "down", down)]] <- list(
          lambda = model$lambda,
          mu = model$mu,
          sigma = model$sigma
        )
        visualize_model(this_data$yards_gained, model, name)
      }
      
      # Try for each field zone
      for (zone in unique(this_data$field_zone)) {
        zone_data <- subset(this_data, field_zone == zone)
        
        if (nrow(zone_data) < 20) {
          cat("Not enough data for", play_type, "on down", down, "in", zone, "\n")
          next
        }
        
        name <- paste(play_type, "on down", down, "in", zone)
        model <- fit_safely(zone_data$yards_gained, name)
        
        if (!is.null(model)) {
          fitted_models[[paste(play_type, "down", down, zone)]] <- list(
            lambda = model$lambda,
            mu = model$mu,
            sigma = model$sigma
          )
          visualize_model(zone_data$yards_gained, model, name)
        }
      }
    }
  }
  
  # Extract event rates
  event_rates <- list()
  
  # Play type probabilities
  for (down in 1:4) {
    for (zone in unique(run_pass_plays$field_zone)) {
      zone_data <- subset(run_pass_plays, down == down & field_zone == zone)
      
      if (nrow(zone_data) > 0) {
        event_rates[[paste("pass_prob_down", down, "_zone_", zone)]] <- 
          mean(zone_data$play_type == "pass", na.rm = TRUE)
      }
    }
  }
  
  # Turnover rates
  if ("interception" %in% colnames(run_pass_plays)) {
    for (down in 1:4) {
      pass_data <- subset(run_pass_plays, play_type == "pass" & down == down)
      if (nrow(pass_data) > 0) {
        event_rates[[paste("interception_rate_down", down)]] <- 
          mean(pass_data$interception, na.rm = TRUE)
      }
    }
  }
  
  if ("fumble_lost" %in% colnames(run_pass_plays)) {
    for (down in 1:4) {
      run_data <- subset(run_pass_plays, play_type == "run" & down == down)
      if (nrow(run_data) > 0) {
        event_rates[[paste("fumble_rate_down", down)]] <- 
          mean(run_data$fumble_lost, na.rm = TRUE)
      }
    }
  }
  
  # Completion rates
  if ("complete_pass" %in% colnames(run_pass_plays)) {
    for (down in 1:4) {
      pass_data <- subset(run_pass_plays, play_type == "pass" & down == down)
      if (nrow(pass_data) > 0) {
        event_rates[[paste("completion_rate_down", down)]] <- 
          mean(pass_data$complete_pass, na.rm = TRUE)
      }
    }
  } else if ("incomplete_pass" %in% colnames(run_pass_plays)) {
    for (down in 1:4) {
      pass_data <- subset(run_pass_plays, play_type == "pass" & down == down)
      if (nrow(pass_data) > 0) {
        event_rates[[paste("completion_rate_down", down)]] <- 
          1 - mean(pass_data$incomplete_pass, na.rm = TRUE)
      }
    }
  }
  
  # Save the fitted models and rates
  fitted_models[["event_rates"]] <- event_rates
  saveRDS(fitted_models, "fitted_mixture_models.rds")
  
  cat("\nFitted", length(fitted_models) - 1, "mixture models\n")
  cat("Models saved to fitted_mixture_models.rds\n")
  
  return(fitted_models)
}

# Run the fitting process
fitted_models <- fit_mixture_models()

# Print summary of fitted models
cat("\nSummary of fitted models:\n")
for (name in names(fitted_models)) {
  if (name == "event_rates") next
  
  model <- fitted_models[[name]]
  cat("\n", name, ":\n", sep = "")
  cat("  Components:", length(model$lambda), "\n")
  cat("  Weights:", paste(round(model$lambda, 3), collapse = ", "), "\n")
  cat("  Means:", paste(round(model$mu, 1), collapse = ", "), "\n")
  cat("  SDs:", paste(round(model$sigma, 1), collapse = ", "), "\n")
}

# Print summary of event rates
event_rates <- fitted_models[["event_rates"]]
cat("\nSummary of event rates:\n")
for (name in names(event_rates)) {
  cat("  ", name, ": ", round(event_rates[[name]], 3), "\n", sep = "")
}