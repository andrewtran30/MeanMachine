run_epoch <- function(down, ytg, fp, max_drives = 10) {
  team <- 1  # 1 for reference team, -1 for opponent
  drive_count <- 0
  
  while(drive_count < max_drives) {
    new_state <- run_drive(down, ytg, fp)
    score <- check_score(new_state$fp)
    
    if(!is.na(score)) {
      return(score * team)
    }
    
    down <- new_state$down
    ytg <- new_state$ytg
    fp <- new_state$fp
    team <- -team
    drive_count <- drive_count + 1
    
    cat(sprintf("Drive %d: Team %d - Down: %d, YTG: %d, FP: %d\n", 
                drive_count, team, down, ytg, fp))
  }
  
  return(0)
}