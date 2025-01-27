# Source all function files
source("check_score.R")
source("run_drive.R")
source("run_epoch.R")
source("expected_points.R")

# Test the functions
result <- expected_points(1, 10, 75)
print(paste("Expected points:", result))
