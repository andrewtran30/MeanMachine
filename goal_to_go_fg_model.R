# goal_to_go_fg_model.R
# Field goal model specifically for goal-to-go situations

# Load required packages
library(dplyr)
library(ggplot2)

# Fit a field goal model for goal-to-go situations
fit_goal_to_go_fg_model <- function(fg_data) {
  # Simple logistic regression for field goal success
  if (nrow(fg_data) > 0 && "field_goal_result" %in% colnames(fg_data)) {
    fg_data$success <- ifelse(fg_data$field_goal_result == "made", 1, 0)
    
    # Fit logistic regression model
    tryCatch({
      fg_model <- glm(success ~ ydstogo, data = fg_data, family = binomial())
      cat("Successfully fit field goal model\n")
      
      # Visualize the model
      visualize_fg_model(fg_model, fg_data)
      
      # Save the model
      saveRDS(fg_model, "goal_to_go_fg_model.rds")
      
      # Return the model
      return(fg_model)
    }, error = function(e) {
      cat("Error fitting field goal model:", e$message, "\n")
      cat("Using default parameters\n")
      
      # Return NULL to indicate failure - we'll use defaults
      return(NULL)
    })
  } else {
    cat("Insufficient field goal data\n")
    return(NULL)
  }
}

# Function to predict field goal success probability
predict_fg_success_goal_to_go <- function(ytg, fg_model = NULL) {
  # Try to load the model if not provided
  if (is.null(fg_model)) {
    tryCatch({
      fg_model <- readRDS("goal_to_go_fg_model.rds")
    }, error = function(e) {
      fg_model <- NULL
    })
  }
  
  if (!is.null(fg_model)) {
    # Use fitted model if available
    prob <- predict(fg_model, newdata = data.frame(ydstogo = ytg), type = "response")
    return(as.numeric(prob))
  } else {
    # Default model based on typical NFL success rates for short kicks
    # Very accurate from close range, decreasing slightly with distance
    base_prob <- 0.98  # Base probability for very short kicks
    return(max(0.75, base_prob - (ytg * 0.01)))
  }
}

# Visualize the field goal model
visualize_fg_model <- function(model, data) {
  # Create a sequence of yards to go
  ytg_seq <- 1:20
  
  # Predict probabilities
  probs <- predict(model, newdata = data.frame(ydstogo = ytg_seq), type = "response")
  
  # Create data frame for plotting
  pred_df <- data.frame(
    ydstogo = ytg_seq,
    success_prob = probs
  )
  
  # Create actual data points if available
  if (!is.null(data) && "success" %in% colnames(data) && "ydstogo" %in% colnames(data)) {
    actual_rates <- data %>%
      group_by(ydstogo) %>%
      summarize(
        attempts = n(),
        success_rate = mean(success, na.rm = TRUE),
        .groups = 'drop'
      )
  } else {
    actual_rates <- NULL
  }
  
  # Create plot
  p <- ggplot(pred_df, aes(x = ydstogo, y = success_prob)) +
    geom_line(color = "blue", size = 1.2) +
    labs(title = "Field Goal Success Probability by Distance",
         x = "Yards to Goal",
         y = "Success Probability") +
    ylim(0, 1) +
    theme_minimal()
  
  # Add actual data points if available
  if (!is.null(actual_rates)) {
    p <- p + geom_point(data = actual_rates, 
                        aes(x = ydstogo, y = success_rate, size = attempts),
                        color = "red", alpha = 0.7) +
      scale_size_continuous(name = "Attempts")
  }
  
  # Save the plot
  ggsave("field_goal_model.png", p, width = 8, height = 6)
  
  cat("Field goal model visualization saved as field_goal_model.png\n")
  
  return(TRUE)
}