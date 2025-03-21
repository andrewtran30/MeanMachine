# Load and explore the football dataset from data.rds

# Load the dataset
football_data <- readRDS("football_data.rds")

# Display basic information
cat("Dataset dimensions:", dim(football_data), "\n")
cat("Column names:", colnames(football_data), "\n\n")

# Look at first few rows
cat("First few rows:\n")
print(head(football_data))

# Basic summary statistics
cat("\nSummary statistics:\n")
print(summary(football_data))

# Check for missing values
cat("\nMissing values per column:\n")
print(colSums(is.na(football_data)))

# Create some basic visualizations
library(ggplot2)

# Histogram of yards gained
yards_hist <- ggplot(football_data, aes(x = yards_gained)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Yards Gained",
       x = "Yards Gained",
       y = "Frequency") +
  theme_minimal()

ggsave("yards_gained_distribution.png", yards_hist, width = 8, height = 6)

# Yards gained by play type (if available)
if("play_type" %in% colnames(football_data)) {
  play_type_boxplot <- ggplot(football_data, aes(x = play_type, y = yards_gained, fill = play_type)) +
    geom_boxplot() +
    labs(title = "Yards Gained by Play Type",
         x = "Play Type",
         y = "Yards Gained") +
    theme_minimal()
  
  ggsave("yards_by_play_type.png", play_type_boxplot, width = 8, height = 6)
  
  # Count of play types
  cat("\nPlay type counts:\n")
  print(table(football_data$play_type))
}

# Yards gained by down
if("down" %in% colnames(football_data)) {
  down_boxplot <- ggplot(football_data, aes(x = as.factor(down), y = yards_gained, fill = as.factor(down))) +
    geom_boxplot() +
    labs(title = "Yards Gained by Down",
         x = "Down",
         y = "Yards Gained",
         fill = "Down") +
    theme_minimal()
  
  ggsave("yards_by_down.png", down_boxplot, width = 8, height = 6)
  
  # Mean yards by down
  cat("\nMean yards gained by down:\n")
  print(aggregate(yards_gained ~ down, data = football_data, FUN = mean))
}

# Field position distribution
if("field_position" %in% colnames(football_data) || "fp" %in% colnames(football_data)) {
  fp_col <- ifelse("field_position" %in% colnames(football_data), "field_position", "fp")
  
  fp_hist <- ggplot(football_data, aes_string(x = fp_col)) +
    geom_histogram(binwidth = 5, fill = "lightgreen", color = "black", alpha = 0.7) +
    labs(title = "Distribution of Field Position",
         x = "Field Position",
         y = "Frequency") +
    theme_minimal()
  
  ggsave("field_position_distribution.png", fp_hist, width = 8, height = 6)
}