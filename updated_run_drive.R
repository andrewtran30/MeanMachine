source("Updated_Run_Play.R")

run_drive <- function(down, ytg, fp) {
  cat(sprintf("Drive starts: Down %d, YTG %d, FP %d\n", down, ytg, fp))
  
  # Use recursion to handle multiple plays within a drive
  drive_recursive <- function(down, ytg, fp) {
    cat(sprintf("  Play: Down %d, YTG %d, FP %d\n", down, ytg, fp))
    
    # Get new state from run_play
    new_state <- run_play(down, ytg, fp)
    
    # Check if we should continue the drive or exit
    if (new_state$exit_drive == 0) {
      # Stay with current drive - continue recursion
      drive_recursive(new_state$down, new_state$ytg, new_state$fp)
    } else {
      # Return current state to run_epoch
      cat(sprintf("  Drive ends: Down %d, YTG %d, FP %d\n", 
                  new_state$down, new_state$ytg, new_state$fp))
      list(
        down = new_state$down,
        ytg = new_state$ytg,
        fp = new_state$fp
      )
    }
  }
  
  # Start the recursive process
  drive_recursive(down, ytg, fp)
}