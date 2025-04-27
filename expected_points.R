expected_points <- function(down, ytg, fp, n_sims = 1) {
  scores <- numeric(n_sims)
  for(i in 1:n_sims) {
    scores[i] <- run_epoch(down, ytg, fp)
  }
  return(mean(scores))
}