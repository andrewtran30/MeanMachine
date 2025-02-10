# run_drive.R

# Source the run_play functions
source("run_play.R")

run_drive <- function(down, ytg, fp) {
  # Use the run_play function from the sourced file
  result <- run_play(down, ytg, fp)
  cat(sprintf("  Start FP: %d, Yards gained: %d, End FP: %d\n", 
              fp, result$yards, result$fp))
  list(
    down = result$down,
    ytg = result$ytg,
    fp = result$fp
  )
}
