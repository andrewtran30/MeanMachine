check_score <- function(fp) {
  if(fp > 100 && fp <= 110) {
    return(7)  # Touchdown
  } else if(fp > 110 && fp <= 120) {
    return(3)  # Field goal
  }
  return(NA)  # No score
}