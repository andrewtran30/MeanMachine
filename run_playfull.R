# Helper function to generate yards gained
generate_yards <- function(down, ytg, fp) {
  sample(-5:20, 1)
}

# Handle turnovers
check_turnover <- function() {
  runif(1) < 0.02  # 2% chance of turnover
}

down_one <- function(ytg, fp) {
  YG <- generate_yards(1, ytg, fp)
  new_fp <- max(0, min(120, fp + YG))
  
  if (check_turnover()) {
    return(list(down = 1, ytg = min(10, 100 - new_fp), 
                fp = new_fp, exit_drive = 1))
  }
  
  if (YG >= ytg) {
    list(down = 1, ytg = 10, fp = new_fp, exit_drive = 0)
  } else {
    list(down = 2, ytg = ytg - YG, fp = new_fp, exit_drive = 0)
  }
}

down_two <- function(ytg, fp) {
  YG <- generate_yards(2, ytg, fp)
  new_fp <- max(0, min(120, fp + YG))
  
  if (check_turnover()) {
    return(list(down = 1, ytg = min(10, 100 - new_fp), 
                fp = new_fp, exit_drive = 1))
  }
  
  if (YG >= ytg) {
    list(down = 1, ytg = 10, fp = new_fp, exit_drive = 0)
  } else {
    list(down = 3, ytg = ytg - YG, fp = new_fp, exit_drive = 0)
  }
}

down_three <- function(ytg, fp) {
  YG <- generate_yards(3, ytg, fp)
  new_fp <- max(0, min(120, fp + YG))
  
  if (check_turnover()) {
    return(list(down = 1, ytg = min(10, 100 - new_fp), 
                fp = new_fp, exit_drive = 1))
  }
  
  if (YG >= ytg) {
    list(down = 1, ytg = 10, fp = new_fp, exit_drive = 0)
  } else {
    list(down = 4, ytg = ytg - YG, fp = new_fp, exit_drive = 0)
  }
}

down_four <- function(ytg, fp) {
  # Field position regions and play selection
  if (fp >= 65) {  # Field goal territory
    success_prob <- max(0.2, min(0.95, 1 - (120 - fp) / 100))
    if (runif(1) < success_prob) {
      return(list(down = 1, ytg = 10, fp = 115, exit_drive = 1))  # Made FG
    } else {
      return(list(down = 1, ytg = min(10, 100 - fp), 
                  fp = fp, exit_drive = 1))  # Missed FG
    }
  } else if (ytg <= 2 && fp > 40) {  # Go for it situation
    YG <- generate_yards(4, ytg, fp)
    new_fp <- max(0, min(120, fp + YG))
    
    if (YG >= ytg) {
      list(down = 1, ytg = 10, fp = new_fp, exit_drive = 0)
    } else {
      list(down = 1, ytg = min(10, 100 - new_fp), 
           fp = new_fp, exit_drive = 1)
    }
  } else {  # Punt
    punt_distance <- sample(30:60, 1)
    new_fp <- max(0, min(120, 120 - (fp + punt_distance)))
    list(down = 1, ytg = min(10, 100 - new_fp), 
         fp = new_fp, exit_drive = 1)
  }
}

run_play <- function(down, ytg, fp) {
  # Handle touchdown
  if (fp >= 100) {
    return(list(down = 1, ytg = 10, fp = 105, exit_drive = 1))
  }
  
  # Call appropriate down function
  switch(as.character(down),
         "1" = down_one(ytg, fp),
         "2" = down_two(ytg, fp),
         "3" = down_three(ytg, fp),
         "4" = down_four(ytg, fp))
}
