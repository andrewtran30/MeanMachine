source("updated_run_play.R")

run_drive <- function(down, ytg, fp) {
  cat(sprintf("Drive starts: Down %d, YTG %d, FP %d\n", down, ytg, fp))
  
  # Initialize state
  current_state <- list(
    down = down,
    ytg = ytg,
    fp = fp,
    exit_drive = 0
  )
  
  # Use a while loop instead of recursion
  while (current_state$exit_drive == 0) {
    cat(sprintf("  Play: Down %d, YTG %d, FP %d\n", 
                current_state$down, current_state$ytg, current_state$fp))
    
    # Get new state from run_play
    current_state <- run_play(current_state$down, current_state$ytg, current_state$fp)
  }
  
  # Drive has ended
  cat(sprintf("  Drive ends: Down %d, YTG %d, FP %d\n", 
              current_state$down, current_state$ytg, current_state$fp))
  
  # Return final state
  return(list(
    down = current_state$down,
    ytg = current_state$ytg,
    fp = current_state$fp
  ))
}