# Source all required files
source("check_score.R")
source("Updated_Run_Drive.R")
source("run_epoch.R")
source("expected_points.R")

# Test the functions
cat("Running a single drive simulation:\n")
result <- run_drive(1, 10, 25)
print(result)

cat("\nRunning a full game simulation:\n")
score <- run_epoch(1, 10, 20, max_drives = 10)
cat(sprintf("Final score: %d\n", score))

cat("\nCalculating expected points:\n")
for (fp in c(20, 50, 75, 90)) {
  ep <- expected_points(1, 10, fp, n_sims = 100)
  cat(sprintf("Field Position %d: Expected Points = %.2f\n", fp, ep))
}

# Visualize field goal probability by field position
library(ggplot2)

positions <- 50:95
probabilities <- sapply(positions, predict_fg_success)

fg_data <- data.frame(
  FieldPosition = positions,
  SuccessProbability = probabilities
)

ggplot(fg_data, aes(x = FieldPosition, y = SuccessProbability)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = "Field Goal Success Probability by Field Position",
       x = "Field Position",
       y = "Probability of Success") +
  theme_minimal()

# Save the plot
ggsave("field_goal_probability.png", width = 8, height = 6)
