source("FG_FD.R")  # Load the models

run_play <- function(down, ytg, fp) {
  # Generate yards gained
  YG <- sample(-5:20, 1)
  new_fp <- max(0, min(120, fp + YG))
  
  if (down < 4) {
    # Check for turnover (2% chance)
    if (runif(1) < 0.02) {
      return(list(down = 1, ytg = min(10, 100 - new_fp), fp = new_fp, exit_drive = 1))
    }
    
    if (YG < ytg) {
      # Didn't make first down - increment down
      list(down = down + 1, ytg = ytg - YG, fp = new_fp, exit_drive = 0)
    } else {
      # Made first down - reset to 1st & 10
      list(down = 1, ytg = 10, fp = new_fp, exit_drive = 0)
    }
  } else {
    # 4th down decision using our statistical model
    decision <- predict_4th_down_decision(fp, ytg)
    
    if (decision == "field_goal") {
      # Use logistic regression model to determine success probability
      success_prob <- predict_fg_success(fp)
      if (runif(1) < success_prob) {
        list(down = 1, ytg = 10, fp = 115, exit_drive = 1)  # Made field goal
      } else {
        list(down = 1, ytg = min(10, 100 - fp), fp = fp, exit_drive = 1)  # Missed
      }
    } else if (decision == "go_for_it") {
      if (YG >= ytg) {
        list(down = 1, ytg = 10, fp = new_fp, exit_drive = 0)  # Converted
      } else {
        list(down = 1, ytg = min(10, 100 - new_fp), fp = new_fp, exit_drive = 1)  # Failed
      }
    } else {  # punt
      punt_distance <- sample(30:60, 1)
      new_fp <- max(0, min(120, 120 - (fp + punt_distance)))
      list(down = 1, ytg = min(10, 100 - new_fp), fp = new_fp, exit_drive = 1)
    }
  }
}