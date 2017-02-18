
SimulateResult <- function() {
  result.sim <- runif(1)
}

SimulateMargin <- function(result.sim, param.margin) {
  margin.sim <- -(1/param.margin) * log10(1/result.sim - 1) # check/reconsider this negative sign
}


SimulateScoreLoss <- function(mu, s) {
  score.loss.sim <- rlogis(1, mu, s) # Logistic distribution like Elo
}


SimulateScoreWin <- function(score.loss.sim, margin.sim) {
  score.win.sim <- score.loss.sim + abs(margin.sim)
}

