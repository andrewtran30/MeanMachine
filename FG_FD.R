# Field Goal and Fourth Down Decision Models

# Step 1: Field Goal Success Model using Logistic Regression

# Generate synthetic field goal data
set.seed(123)
n_samples <- 1000
fp <- sample(50:95, n_samples, replace = TRUE)
distance <- 120 - fp  # Distance from goal
success <- rbinom(n_samples, 1, pmax(0.1, pmin(0.95, 1 - distance/100)))

field_goal_data <- data.frame(
  fp = fp,
  distance = distance,
  success = success
)

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
# Using nnet package for multinomial regression
library(nnet)

# Generate synthetic fourth down decision data
n_samples <- 5000
fp_4th <- sample(1:99, n_samples, replace = TRUE)
ytg_4th <- sample(1:15, n_samples, replace = TRUE)

# Create decision probabilities based on field position and yards to go
p_field_goal <- pmax(0, pmin(1, (fp_4th - 50)/50))
p_punt <- pmax(0, pmin(1, 1 - (fp_4th - 30)/70)) + ytg_4th/30
p_go_for_it <- pmax(0, pmin(1, 0.2 + (1 - ytg_4th/10) * (fp_4th/100)))

# Normalize probabilities
sum_probs <- p_field_goal + p_punt + p_go_for_it
p_field_goal <- p_field_goal / sum_probs
p_punt <- p_punt / sum_probs
p_go_for_it <- p_go_for_it / sum_probs

# Generate decisions based on probabilities
set.seed(456)
decision_probs <- cbind(p_go_for_it, p_punt, p_field_goal)
decisions <- apply(decision_probs, 1, function(x) sample(c("go_for_it", "punt", "field_goal"), 1, prob = x))

fourth_down_data <- data.frame(
  fp = fp_4th,
  ytg = ytg_4th,
  decision = decisions
)

# Fit multinomial regression model
fourth_down_model <- multinom(decision ~ fp + ytg, data = fourth_down_data)
summary(fourth_down_model)

# Function to predict fourth down decision probabilities
predict_4th_down_decision <- function(fp, ytg) {
  probs <- predict(fourth_down_model, newdata = data.frame(fp = fp, ytg = ytg), type = "probs")
  
  # If probs is a matrix, convert to vector by extracting the row
  if (is.matrix(probs)) {
    probs <- probs[1, ]
  }
  
  # Determine decision based on highest probability
  decision <- sample(c("go_for_it", "punt", "field_goal"), 1, prob = probs)
  return(decision)
}