# Top-level expected points function
expected_points <- function(down, ytg, fp, n_sims = 1) {
  # Run n simulations and average the results
  scores <- numeric(n_sims)
  for(i in 1:n_sims) {
    scores[i] <- run_epoch(down, ytg, fp)
  }
  return(mean(scores))
}

# Run a single epoch until score or max drives reached
run_epoch <- function(down, ytg, fp, max_drives = 10) {
  team <- 1  # 1 for reference team, -1 for opponent
  drive_count <- 0
  
  while(drive_count < max_drives) {
    # Run drive and get new state
    new_state <- run_drive(down, ytg, fp)
    
    # Check if drive resulted in score
    score <- check_score(new_state$fp)
    if(!is.na(score)) {
      return(score * team)  # Return signed score based on team
    }
    
    # Update state and switch possession
    down <- new_state$down
    ytg <- new_state$ytg
    fp <- new_state$fp
    team <- -team
    drive_count <- drive_count + 1
    
    # Optional: Print drive results for debugging
    cat(sprintf("Drive %d: Team %d - Down: %d, YTG: %d, FP: %d\n", 
                drive_count, team, down, ytg, fp))
  }
  
  return(0)  # Return 0 if max drives reached without score
}

# Simple drive function - returns new game state
run_drive <- function(down, ytg, fp) {
  # Simple implementation: randomly select new field position
  new_fp <- sample(0:120, 1)  # Including extended field for scoring
  
  # Return new state
  list(
    down = 1,  # Always first down for opponent
    ytg = min(10, new_fp),  # Handle goal-to-go situations
    fp = new_fp
  )
}

# Check if current state results in score
check_score <- function(fp) {
  if(fp > 100 && fp <= 110) {
    return(7)  # Touchdown
  } else if(fp > 110 && fp <= 120) {
    return(3)  # Field goal
  }
  return(NA)  # No score
}

# Example usage
result <- expected_points(1, 10, 75)
print(paste("Expected points:", result))