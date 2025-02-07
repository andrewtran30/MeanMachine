run_play <- function(down, ytg, fp) {
  # Generate yards gained
  YG <- sample(-5:20, 1)
  new_fp <- max(0, min(120, fp + YG))
  
  if (down < 4) {
    # Check for turnover (10% chance)
    if (runif(1) < 0.1) {
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
    # 4th down logic - unchanged
    if (fp >= 65) {
      success_prob <- max(0.2, min(0.95, 1 - (120 - fp) / 100))
      if (runif(1) < success_prob) {
        list(down = 1, ytg = 10, fp = 115, exit_drive = 1)
      } else {
        list(down = 1, ytg = min(10, 100 - fp), fp = fp, exit_drive = 1)
      }
    } else if (ytg <= 2 && fp > 40) {
      if (YG >= ytg) {
        list(down = 1, ytg = 10, fp = new_fp, exit_drive = 0)
      } else {
        list(down = 1, ytg = min(10, 100 - new_fp), fp = new_fp, exit_drive = 1)
      }
    } else {
      punt_distance <- sample(30:60, 1)
      new_fp <- max(0, min(120, 120 - (fp + punt_distance)))
      list(down = 1, ytg = min(10, 100 - new_fp), fp = new_fp, exit_drive = 1)
    }
  }
}