# Read the data
game_data <- readRDS("data.rds")
library(ggplot2)
library(dplyr)

# Helper function to create transition matrix 
# Create transition matrix where entry (i,j) represents 
#number of times team i lost to team j, normalized by row
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
# Top teams based on steady state probabilities:
# 1. Los Angeles Dodgers (LAN): 0.0395
# 2. New York Yankees (NYA): 0.0383
# 3. Houston Astros (HOU): 0.0372

# Steady state vector for all years:
# LAN: 0.03947963 SEA: 0.03394105 BOS: 0.03584663 MIN: 0.03187726 KCA: 0.02999318 
# CLE: 0.03454188 TOR: 0.03532068 PHI: 0.03243757 SFN: 0.03400406 SLN: 0.03536552 
# COL: 0.03045314 ATL: 0.03445209 WAS: 0.03267680 CHN: 0.03492772 NYA: 0.03830793 
# MIL: 0.03498187 BAL: 0.03316002 ANA: 0.03219737 CHA: 0.02930721 TEX: 0.03184557 
# ARI: 0.03200381 SDN: 0.03235007 CIN: 0.03064438 TBA: 0.03660489 OAK: 0.03197639 
#HOU: 0.03721197 NYN: 0.03341620 PIT: 0.03138704 DET: 0.02959693 MIA: 0.02969114 

# Analysis Findings:
# - The LA Dodgers had consistent performance across the years
# - Houston Astros had notable improvement while the Detroit Tigers had a notable decline
# - The rankings are significantly impacted by interleague play 

# Our question we'll be answering is how do the rankings of the 
# top 5 teams (based on overall performance) change year by year? 
# This analysis helps understand the consistency of team performance over 
# time and identify dynasties or decline periods.

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
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_y_reverse(breaks = 1:30) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    title = "Top 5 Teams Rankings Over Time",
    y = "Rank (1 = Best)",
    x = "Season"
  )


ggsave("ranking_change.jpg")

