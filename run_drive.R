run_drive <- function(down, ytg, fp) {
  yards_gained <- sample(-5:20, 1)
  new_fp <- fp + yards_gained
  new_fp <- max(0, min(120, new_fp))
  
  cat(sprintf("  Start FP: %d, Yards gained: %d, End FP: %d\n", 
              fp, yards_gained, new_fp))
  
  list(
    down = 1,
    ytg = min(10, 100 - new_fp),
    fp = new_fp
  )
}