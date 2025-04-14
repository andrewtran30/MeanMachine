install.packages(c("dplyr", "ggplot2", "tidyr", "mixtools"))

source("run_goal_to_go_analysis.R")
run_complete_analysis()

# Load packages
library(dplyr)
library(ggplot2)

# Extract data
source("goal_to_go_data.R")
goal_data <- extract_4th_and_goal_data()
analyze_goal_to_go_data(goal_data)

# Fit models
source("goal_to_go_models.R")
goal_models <- fit_goal_to_go_models(goal_data$goal_to_go)

# Run simulations
source("goal_to_go_simulation.R")
result <- simulate_4th_and_goal(ytg = 3, score_diff = -7, time_remaining = 1)
print(result$recommendation)

source("goal_to_go_simulation.R")
run_simple_test()

source("improved_simulation.R")
result <- simulate_4th_and_goal2(ytg = 3, score_diff = -4, time_remaining = 2)
print(result$recommendation)