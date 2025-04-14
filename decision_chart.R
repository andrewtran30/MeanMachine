# Generate comprehensive 4th & Goal decision charts
library(dplyr)
library(ggplot2)
library(gridExtra)

# Source improved simulation
source("improved_simulation.R")
source("goal_to_go_sampling.R")
source("goal_to_go_fg_model.R")


generate_decision_chart <- function() {
  # Define parameter ranges
  ytg_range <- 1:10
  score_diff_range <- -7:7  # Extending to both positive and negative
  time_remaining_range <- c(1, 2, 3, 4, 5)
  
  # Initialize results dataframe
  results <- data.frame()
  
  # Generate all combinations and get recommendations
  total_combinations <- length(ytg_range) * length(score_diff_range) * length(time_remaining_range)
  counter <- 0
  
  cat("Generating decision chart...\n")
  cat("Total scenarios to simulate:", total_combinations, "\n")
  
  # Progress bar setup
  pb <- txtProgressBar(min = 0, max = total_combinations, style = 3)
  
  for (time in time_remaining_range) {
    for (score_diff in score_diff_range) {
      for (ytg in ytg_range) {
        # Update progress
        counter <- counter + 1
        setTxtProgressBar(pb, counter)
        
        # Simulate the scenario
        result <- simulate_4th_and_goal2(ytg = ytg, score_diff = score_diff, 
                                        time_remaining = time)
        
        # Add to results dataframe
        results <- rbind(results, data.frame(
          ytg = ytg,
          score_diff = score_diff,
          time_remaining = time,
          recommendation = result$recommendation,
          go_expected_pts = result$go_for_it$expected_pts,
          fg_expected_pts = result$field_goal$expected_pts,
          go_wp_change = result$go_for_it$wp_change,
          fg_wp_change = result$field_goal$wp_change
        ))
      }
    }
  }
  
  close(pb)
  cat("Simulation complete!\n")
  
  # Save full results for later use
  saveRDS(results, "4th_and_goal_decision_matrix.rds")
  
  # Create charts for each time remaining value
  for (time in time_remaining_range) {
    cat("Creating chart for time remaining =", time, "minutes...\n")
    
    # Filter data for this time
    time_data <- subset(results, time_remaining == time)
    
    # Create heatmap
    p <- ggplot(time_data, aes(x = ytg, y = score_diff, fill = recommendation)) +
      geom_tile(color = "white", size = 0.1) +
      scale_fill_manual(values = c("Go for it" = "#2E8B57", "Kick field goal" = "#104E8B")) +
      labs(
        title = paste("4th & Goal Decision Chart -", time, "Minutes Remaining"),
        subtitle = "Green = Go for Touchdown, Blue = Kick Field Goal",
        x = "Yards to Go",
        y = "Score Differential (+ = Leading, - = Trailing)",
        fill = "Recommendation"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 10),
        legend.position = "bottom",
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank()
      ) +
      scale_x_continuous(breaks = 1:10, labels = 1:10) +
      scale_y_continuous(breaks = seq(-14, 14, by = 2))
    
    # Save the chart
    filename <- paste0("4th_goal_chart_", time, "min.png")
    ggsave(filename, p, width = 10, height = 10, dpi = 300)
    cat("Saved chart to", filename, "\n")
  }
  
  # Create combo chart with all time values
  cat("Creating combo chart with all time values...\n")
  
  # Create a list to hold individual plots
  plots_list <- list()
  
  # Generate small multiples for each time
  for (i in seq_along(time_remaining_range)) {
    time <- time_remaining_range[i]
    time_data <- subset(results, time_remaining == time)
    
    p <- ggplot(time_data, aes(x = ytg, y = score_diff, fill = recommendation)) +
      geom_tile(color = "white", size = 0.1) +
      scale_fill_manual(values = c("Go for it" = "#2E8B57", "Kick field goal" = "#104E8B")) +
      labs(title = paste(time, "Minutes Remaining")) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.position = "none"
      ) +
      scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
      scale_y_continuous(breaks = seq(-14, 14, by = 7))
    
    if (i == 1) {
      p <- p + ylab("Score Differential")
    } else {
      p <- p + ylab(NULL)
    }
    
    if (i == 3) {
      p <- p + xlab("Yards to Go")
    } else {
      p <- p + xlab(NULL)
    }
    
    plots_list[[i]] <- p
  }
  
  # Arrange all plots in a grid with shared legend
  combo_chart <- arrangeGrob(
    grobs = plots_list,
    ncol = 3,
    top = textGrob("4th & Goal Decision Matrix by Time Remaining", 
                   gp = gpar(fontsize = 16, fontface = "bold"))
  )
  
  # Save the combo chart
  ggsave("4th_goal_combo_chart.png", combo_chart, width = 15, height = 10, dpi = 300)
  cat("Saved combo chart to 4th_goal_combo_chart.png\n")
  
  # Also create single-page comprehensive version
  # This will show one row of charts for selected important score differentials
  cat("Creating comprehensive single-page decision guide...\n")
  
  # Select key score differentials to show
  key_diffs <- c(-7, -3, 0, 3, 7)
  
  # Generate comprehensive plot for each key score differential
  comp_plots <- list()
  for (i in seq_along(key_diffs)) {
    diff <- key_diffs[i]
    diff_data <- subset(results, score_diff == diff)
    
    # Generate plot with time vs ytg
    p <- ggplot(diff_data, aes(x = ytg, y = time_remaining, fill = recommendation)) +
      geom_tile(color = "white", size = 0.1) +
      scale_fill_manual(values = c("Go for it" = "#2E8B57", "Kick field goal" = "#104E8B")) +
      labs(title = paste("Score Diff:", diff)) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.position = "none"
      ) +
      scale_x_continuous(breaks = c(1, 3, 5, 7, 9)) +
      scale_y_continuous(breaks = 1:5)
    
    if (i == 1) {
      p <- p + ylab("Minutes Remaining")
    } else {
      p <- p + ylab(NULL)
    }
    
    if (i == 3) {
      p <- p + xlab("Yards to Go")
    } else {
      p <- p + xlab(NULL)
    }
    
    comp_plots[[i]] <- p
  }
  
  # Arrange comprehensive plots
  comp_chart <- arrangeGrob(
    grobs = comp_plots,
    ncol = length(key_diffs),
    top = textGrob("4th & Goal Decision Guide by Score Differential", 
                   gp = gpar(fontsize = 16, fontface = "bold"))
  )
  
  # Save the comprehensive chart
  ggsave("4th_goal_comprehensive_guide.png", comp_chart, width = 15, height = 5, dpi = 300)
  cat("Saved comprehensive guide to 4th_goal_comprehensive_guide.png\n")
  
  cat("All charts generated successfully!\n")
  return(results)
}

# Run the chart generation
decision_data <- generate_decision_chart()