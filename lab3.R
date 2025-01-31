# Read the data
game_data <- readRDS("data.rds")
library(ggplot2)
library(dplyr)

# Helper function to create transition matrix 
create_transition_matrix <- function(data) {
  teams <- unique(c(data$Visiting_Team, data$Home_Team))
  n_teams <- length(teams)
  
  trans_matrix <- matrix(0, nrow = n_teams, ncol = n_teams)
  rownames(trans_matrix) <- colnames(trans_matrix) <- teams
  
  for(i in 1:nrow(data)) {
    if(data$Home_Score[i] > data$Visiting_Score[i]) {
      trans_matrix[data$Visiting_Team[i], data$Home_Team[i]] <- 
        trans_matrix[data$Visiting_Team[i], data$Home_Team[i]] + 1
    } else {
      trans_matrix[data$Home_Team[i], data$Visiting_Team[i]] <- 
        trans_matrix[data$Home_Team[i], data$Visiting_Team[i]] + 1
    }
  }
  
  row_sums <- rowSums(trans_matrix)
  trans_matrix <- trans_matrix / ifelse(row_sums > 0, row_sums, 1)
  return(trans_matrix)
}

# Function to compute steady state
compute_steady_state <- function(trans_matrix, n_iter = 10000) {
  state <- rep(1/nrow(trans_matrix), nrow(trans_matrix))
  names(state) <- rownames(trans_matrix)
  
  for(i in 1:n_iter) {
    state <- as.vector(state %*% trans_matrix)
    names(state) <- rownames(trans_matrix)
  }
  return(state)
}

# Part 1: All years analysis
all_matrix <- create_transition_matrix(game_data)
steady_state <- compute_steady_state(all_matrix)
rankings <- sort(steady_state, decreasing = TRUE)
print("Rankings across all years:")
print(rankings)

# Part 2: Year-by-year analysis
yearly_rankings <- lapply(unique(game_data$season), function(yr) {
  yr_data <- game_data[game_data$season == yr,]
  yr_matrix <- create_transition_matrix(yr_data)
  ss <- compute_steady_state(yr_matrix)
  names(ss) <- rownames(yr_matrix)
  ss
})
names(yearly_rankings) <- unique(game_data$season)

# Prepare plot data
plot_data <- data.frame(
  Year = rep(names(yearly_rankings), each = length(steady_state)),
  Team = rep(names(steady_state), times = length(yearly_rankings)),
  stringsAsFactors = FALSE
)

plot_data$Rank <- sapply(1:nrow(plot_data), function(i) {
  year_rankings <- yearly_rankings[[plot_data$Year[i]]]
  rank(-year_rankings)[plot_data$Team[i]]
})

# Plot top 5 teams
top_teams <- names(head(sort(steady_state, decreasing = TRUE), 5))
plot_data_filtered <- plot_data[plot_data$Team %in% top_teams,]

ggplot(plot_data_filtered, aes(x = Year, y = Rank, color = Team, group = Team)) +
  geom_line() +
  geom_point() +
  scale_y_reverse() +
  theme_minimal() +
  labs(title = "Top 5 Teams Rankings Over Time")

ggsave("ranking_changes.png")

