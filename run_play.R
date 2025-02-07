run_play <- function(down,ytg,fp){
  YG <- round(runif(1,-5,20))
  if (down < 4){
    if (YG < ytg){
      down = down + 1
      ytg = ytg - YG
      fp = fp + YG
    }
    else{
      down = 1
      ytg = 10
      fp = fp + YG
    }
  list(down,ytg,fp)
  }
}
