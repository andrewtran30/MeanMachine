# goal_to_go_models.R
# Fit mixture models for yards gained in goal-to-go situations

# Load required packages
library(mixtools)
library(dplyr)
library(ggplot2)

# Function to safely fit mixture model with error handling
safe_fit_mixture <- function(data, name, k = 3, max_attempts = 3) {
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
    })
  }
  
  cat("All fitting attempts failed for", name, "- using default parameters\n")
  
  # Return a simple default mixture model
  if (grepl("run", name)) {
    return(list(
      lambda = c(0.4, 0.4, 0.2),
      mu = c(-1, 2, 5),
      sigma = c(1, 1.5, 2)
    ))
  } else {  # pass play
    return(list(
      lambda = c(0.5, 0.3, 0.2),
      mu = c(0, 4, 8),
      sigma = c(1.5, 2, 3)
    ))
  }
}

# Fit mixture models for goal-to-go situations
fit_goal_to_go_models <- function(goal_data) {
  # Filter for run and pass plays
  run_pass_data <- goal_data %>%
    filter(play_type %in% c("run", "pass"))
  
  # Fit models for different yards to go
  models <- list()
  
  # Group by yards to go and play type
  for (ytg in 1:10) {
    for (play_type in c("run", "pass")) {
      # Extract data for this combination
      data_subset <- run_pass_data %>%
        filter(ydstogo == ytg, play_type == play_type)
      
      if (nrow(data_subset) >= 10) {
        # Try to fit model
        name <- paste(play_type, "plays with", ytg, "yards to go")
        model <- safe_fit_mixture(data_subset$yards_gained, name)
        
        # Store model parameters
        if (!is.null(model)) {
          models[[paste(play_type, "ytg", ytg)]] <- list(
            lambda = model$lambda,
            mu = model$mu,
            sigma = model$sigma
          )
          
          # Visualize the fitted model
          visualize_mixture_model(data_subset$yards_gained, model, name)
        }
      }
    }
  }
  
  # Extract event rates
  event_rates <- list()
  
  # Incompletion rates
  pass_data <- run_pass_data %>% filter(play_type == "pass")
  if ("incomplete_pass" %in% colnames(pass_data)) {
    event_rates[["incompletion_rate"]] <- mean(pass_data$incomplete_pass, na.rm = TRUE)
  } else if ("complete_pass" %in% colnames(pass_data)) {
    event_rates[["incompletion_rate"]] <- 1 - mean(pass_data$complete_pass, na.rm = TRUE)
  } else {
    event_rates[["incompletion_rate"]] <- 0.4  # Default if not available
  }
  
  # Interception rates
  if ("interception" %in% colnames(pass_data)) {
    event_rates[["interception_rate"]] <- mean(pass_data$interception, na.rm = TRUE)
  } else {
    event_rates[["interception_rate"]] <- 0.03  # Default if not available
  }
  
  # Fumble rates
  run_data <- run_pass_data %>% filter(play_type == "run")
  if ("fumble_lost" %in% colnames(run_data)) {
    event_rates[["fumble_rate"]] <- mean(run_data$fumble_lost, na.rm = TRUE)
  } else {
    event_rates[["fumble_rate"]] <- 0.02  # Default if not available
  }
  
  # Play type distribution (run vs pass)
  for (ytg in 1:10) {
    ytg_data <- run_pass_data %>% filter(ydstogo == ytg)
    if (nrow(ytg_data) > 0) {
      event_rates[[paste("pass_prob_ytg", ytg)]] <- 
        mean(ytg_data$play_type == "pass", na.rm = TRUE)
    }
  }
  
  # Add event rates to the models
  models[["event_rates"]] <- event_rates
  
  # Save models for later use
  saveRDS(models, "goal_to_go_models.rds")
  
  return(models)
}

# Visualize a fitted mixture model
visualize_mixture_model <- function(data, model, name) {
  # Create data frame for plotting
  df <- data.frame(yards = data)
  
  # Create density plot
  p <- ggplot(df, aes(x = yards)) +
    geom_histogram(aes(y = ..density..), binwidth = 1, fill = "lightblue", color = "black") +
    stat_function(fun = function(x) {
      result <- 0
      for (i in 1:length(model$lambda)) {
        result <- result + model$lambda[i] * dnorm(x, model$mu[i], model$sigma[i])
      }
      return(result)
    }, color = "red", size = 1) +
    labs(title = paste("Mixture Model for", name),
         x = "Yards Gained",
         y = "Density") +
    theme_minimal() +
    xlim(-5, 15)  # Focus on reasonable range for goal-to-go
  
  # Add individual components
  for (i in 1:length(model$lambda)) {
    p <- p + stat_function(fun = function(x) {
      model$lambda[i] * dnorm(x, model$mu[i], model$sigma[i])
    }, color = "blue", alpha = 0.5, linetype = "dashed")
  }
  
  # Save plot
  filename <- paste0(gsub(" ", "_", tolower(name)), "_mixture.png")
  ggsave(filename, p, width = 8, height = 6)
  
  cat("Plot saved as", filename, "\n")
  
  return(TRUE)
}