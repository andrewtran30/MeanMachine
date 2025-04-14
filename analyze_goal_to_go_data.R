analyze_goal_to_go_data <- function(goal_data) {
  # Print summary statistics
  cat("\nSummary of 4th and Goal Situations:\n")
  cat("----------------------------------\n")
  
  # Analyze success rates by distance
  if (nrow(goal_data$success_rates) > 0) {
    cat("\nTouchdown success rates by distance:\n")
    print(goal_data$success_rates)
  }
  
  # Analyze field goal success rates
  if (nrow(goal_data$fg_success_rates) > 0) {
    cat("\nField goal success rates by distance:\n")
    print(goal_data$fg_success_rates)
  }
  
  # Analyze play type selection
  play_types <- goal_data$goal_to_go %>%
    filter(play_type %in% c("run", "pass", "field_goal")) %>%
    group_by(ydstogo, play_type) %>%
    summarize(count = n(), .groups = 'drop') %>%
    group_by(ydstogo) %>%
    mutate(percentage = count / sum(count) * 100)
  
  cat("\nPlay type selection by distance:\n")
  print(play_types)
  
  # Create visualizations
  if (nrow(goal_data$success_rates) > 0) {
    p1 <- ggplot(goal_data$success_rates, aes(x = ydstogo, y = success_rate)) +
      geom_bar(stat = "identity", fill = "darkgreen") +
      labs(title = "Touchdown Success Rate by Distance", 
           x = "Yards to Go", 
           y = "Success Rate") +
      theme_minimal()
    
    ggsave("td_success_by_distance.png", p1, width = 8, height = 6)
  }
  
  if (nrow(goal_data$fg_success_rates) > 0) {
    p2 <- ggplot(goal_data$fg_success_rates, aes(x = ydstogo, y = success_rate)) +
      geom_bar(stat = "identity", fill = "darkblue") +
      labs(title = "Field Goal Success Rate by Distance", 
           x = "Yards to Go", 
           y = "Success Rate") +
      theme_minimal()
    
    ggsave("fg_success_by_distance.png", p2, width = 8, height = 6)
  }
  
  return(TRUE)
}