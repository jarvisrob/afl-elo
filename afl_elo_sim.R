

# Losing scores and margin errors are normally distributed with
# Losing score ~ N(mu = 75.1988545 +/- 0.2903956, sigma = 19.1859019 +/- 0.2053407)
# Margin error ~ N(mu = 0, sigma = 38.5)




SimulateMargin <- function(margin.exp, norm.sigma = 38.5) {
  #margin.sim <- round(margin.exp + rnorm(1, mean = 0, sd = norm.sigma), 0)
  margin.sim <- margin.exp + rnorm(1, mean = 0, sd = norm.sigma)
  margin.sim
}

SimulateLoseScore <- function(norm.mu = 75.2, norm.sigma = 19.2) {
  #lose.score <- round(rnorm(1, mean = norm.mu, sd = norm.sigma), 0)
  lose.score <- rnorm(1, mean = norm.mu, sd = norm.sigma)
  lose.score
}

SimulateGameScore <- function(margin.exp.home, margin.error.sigma = 38.5, lose.score.mu = 75.2, lose.score.sigma = 19.2) {
  margin.sim.home <- SimulateMargin(margin.exp.home, margin.error.sigma)
  lose.score.sim <- SimulateLoseScore(lose.score.mu, lose.score.sigma)

  if (margin.sim.home < 0) {
    score.points.home <- lose.score.sim
    score.points.away <- lose.score.sim + (-margin.sim.home)
  } else {
    score.points.away <- lose.score.sim
    score.points.home <- lose.score.sim + margin.sim.home
  }

  writeLines(paste0("margin.home = ", margin.sim.home, " | lose.score = ", lose.score.sim, " | home = ", score.points.home, " | away = ", score.points.away))

  score.sim <- list(margin.home = margin.sim.home, lose = lose.score.sim, points.home = score.points.home, points.away = score.points.away)
}