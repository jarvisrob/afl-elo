

# Losing scores and margin errors are normally distributed with
# Losing score ~ N(mu = 75.1988545 +/- 0.2903956, sigma = 19.1859019 +/- 0.2053407)
# Margin error ~ N(mu = 0, sigma = 38.70854937 +/- 0.41428554), note mean calc was -0.08213818 +/- 0.58588823



#SimulateMargin <- function(margin.exp, logis.scale = 22.04785123) {
SimulateMargin <- function(margin.exp, norm.sd = 38.6573089) {
  margin.sim <- round(margin.exp + rnorm(1, mean = 0, sd = norm.sd), 0)
  #margin.sim <- margin.exp + rlogis(1, location = 0, scale = logis.scale)
  margin.sim
}

SimulateSumPoints <- function(norm.mean = 186.3551913, norm.sd = 34.6759915) {
  game.sum.points <- round(rnorm(1, mean = norm.mean, sd = norm.sd), 0)
  game.sum.points
}

SimulateGameScore <- function(margin.exp.home) {
  margin.sim.home <- SimulateMargin(margin.exp.home)
  sum.points <- SimulateSumPoints()

  score.points.home.sim <- (margin.sim.home + sum.points) / 2
  score.points.away.sim <- score.points.home.sim - margin.sim.home

  writeLines(paste0("margin = ", margin.sim.home, " | sum = ", sum.points, " | home = ", score.points.home.sim, " | away = ", score.points.away.sim))

  score.sim <- list(points.home = score.points.home.sim, points.away = score.points.away.sim)
}