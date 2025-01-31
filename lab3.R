# Read the data
game_data <- readRDS("data.rds")
library(ggplot2)
library(dplyr)

# Helper function to create transition matrix
create_transition_matrix <- function(data) {
  # Get unique teams
  teams <- unique(c(data$Visiting_Team, data$Home_Team))
  n_teams <- length(teams)
  
  # Initialize matrix
  trans_matrix <- matrix(0, nrow = n_teams, ncol = n_teams)
  rownames(trans_matrix) <- colnames(trans_matrix) <- teams
  
  # Fill matrix with losses
  for(i in 1:nrow(data)) {
    home_team <- data$Home_Team[i]
    visit_team <- data$Visiting_Team[i]
    home_score <- data$Home_Score[i]
    visit_score <- data$Visiting_Score[i]
    
    if(home_score > visit_score) {
      # Visiting team lost to home team
      trans_matrix[visit_team, home_team] <- trans_matrix[visit_team, home_team] + 1
    } else {
      # Home team lost to visiting team
      trans_matrix[home_team, visit_team] <- trans_matrix[home_team, visit_team] + 1
    }
  }
  
  # Normalize rows
  row_sums <- rowSums(trans_matrix)
  trans_matrix <- trans_matrix / ifelse(row_sums > 0, row_sums, 1)
  
  return(trans_matrix)
}

# Function to compute steady state using power iteration
compute_steady_state <- function(trans_matrix, n_iter = 10000) {
  n <- nrow(trans_matrix)
  state <- rep(1/n, n)  # Initial uniform distribution
  names(state) <- rownames(trans_matrix)
  
  for(i in 1:n_iter) {
    state <- state %*% trans_matrix
  }
  
  return(as.vector(state))
}

# Part 1: Analysis over all years
all_years_matrix <- create_transition_matrix(game_data)
steady_state <- compute_steady_state(all_years_matrix)

# Sort teams by ranking
rankings <- sort(steady_state, decreasing = TRUE)
print("Rankings across all years:")
print(rankings)

# Part 2: Year-by-year analysis
# Question: How have team rankings changed over the years?

yearly_rankings <- list()
for(year in unique(game_data$season)) {
  year_data <- game_data[game_data$season == year,]
  year_matrix <- create_transition_matrix(year_data)
  year_steady <- compute_steady_state(year_matrix)
  yearly_rankings[[as.character(year)]] <- sort(year_steady, decreasing = TRUE)
}

# Create visualization of top 5 teams' ranking changes
library(ggplot2)
library(tidyr)
library(dplyr)

# Prepare data for plotting
rankings_df <- data.frame(
  do.call(rbind, lapply(names(yearly_rankings), function(year) {
    data.frame(
      Year = as.numeric(year),
      Team = names(yearly_rankings[[year]]),
      Rank = rank(-yearly_rankings[[year]])
    )
  }))
)

# Plot top teams' ranking changes
top_teams <- names(head(rankings, 5))
plot_data <- rankings_df %>%
  filter(Team %in% top_teams)

ggplot(plot_data, aes(x = Year, y = Rank, color = Team)) +
  geom_line() +
  geom_point() +
  scale_y_reverse() +
  theme_minimal() +
  labs(title = "Top 5 Teams' Ranking Changes Over Time",
       y = "Rank",
       x = "Season")

ggsave("ranking_changes.png")
