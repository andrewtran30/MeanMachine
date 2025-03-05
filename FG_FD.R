# pbp2014-2024 <- readRDS("C:/Users/naili/Downloads/pbp2014-2024.rds")

# Step 1: Field Goal Success Model using Logistic Regression

# Filter data for field goal attempts
field_goal_data <- pbp2014-2024[pbp2014-2024$play_type == "field_goal", ]
field_goal_data$success <- as.integer(field_goal_data$field_goal_result == "made")
field_goal_data$distance <- 120 - field_goal_data$yardline_100

# Fit logistic regression model
field_goal_model <- glm(success ~ distance, data = field_goal_data, family = binomial)
summary(field_goal_model)

# Function to predict field goal success probability
predict_fg_success <- function(fp) {
  distance <- 120 - fp
  prob <- predict(field_goal_model, newdata = data.frame(distance = distance), type = "response")
  return(prob)
}

# Step 2: Fourth Down Decision Model using Multinomial Regression

library(nnet)

# Filter data for fourth down plays
fourth_down_data <- pbp2014-2024[pbp2014-2024$down == 4, ]

# Create decision column
fourth_down_data$decision <- case_when(
  fourth_down_data$play_type == "punt" ~ "punt",
  fourth_down_data$play_type == "field_goal" ~ "field_goal",
  TRUE ~ "go_for_it"
)

# Fit multinomial regression model
fourth_down_model <- multinom(decision ~ yardline_100 + ydstogo, data = fourth_down_data)
summary(fourth_down_model)

# Function to predict fourth down decision probabilities
predict_4th_down_decision <- function(fp, ytg) {
  probs <- predict(fourth_down_model, newdata = data.frame(yardline_100 = fp, ydstogo = ytg), type = "probs")
  
  # If probs is a matrix, convert to vector by extracting the row
  if (is.matrix(probs)) {
    probs <- probs[1, ]
  }
  
  # Determine decision based on highest probability
  decision <- sample(c("go_for_it", "punt", "field_goal"), 1, prob = probs)
  return(decision)
}
