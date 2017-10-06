

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

SimulateSeasonElo <- function(fixture.season, 
                              team.data, ground.data, ground.location, travel.distance,
                              margin.error.sigma = 38.5, lose.score.mu = 75.2, lose.score.sigma = 19.2) {
  
  n.rounds <- max(fixture.season[, "rnd"])
  
  for (i in 1 : n.rounds) {
    fixture.round <- fixture.season %>% filter(rnd == i)
    n.games <- nrow(fixture.round)
    for (j in 1 : n.games) {
      game.info <- list()
      game.info$team.home <- team.dictionary[[fixture.round[j, "team.home"]]]
      game.info$team.away <- team.dictionary[[fixture.round[j, "team.away"]]]
      game.info$ground <- fixture.round[j, "ground"]
      writeLines(paste0("R", i, " G", j, ": ", game.info$team.home, " vs ", game.info$team.away, " at ", game.info$ground))
      pred <- PredictGame(game.info$team.home, game.info$team.away, game.info$ground,
                          team.data, ground.data, ground.location, travel.distance, 
                          commission = 0, 
                          param.spread = 400, 
                          param.margin = 0.02395478,
                          param.coeff.travel = 17.70182, param.power.travel = 0.2377348)
      score.sim <- SimulateGameScore(pred$margin.exp.home)
      game.info$score.points.home <- score.sim$points.home
      game.info$score.points.away <- score.sim$points.away
      
      writeLines(paste0("rating.home = ", team.data[game.info$team.home, 'rating'], " | rating.away = ", team.data[game.info$team.away, 'rating']))
      writeLines(paste0("gnd.adj.home = ", CalculateGroundAdj(game.info$team.home, game.info$ground, ground.data), 
                        " | gnd.adj.away = ", CalculateGroundAdj(game.info$team.away, game.info$ground, ground.data)))
      
      elo.game <- DoGameElo(game.info, 
                            team.data, 
                            ground.location, ground.data, travel,distance,
                            param.spread = 400,
                            param.margin = 0.02395478,
                            param.coeff.rating.update = 70.34218,
                            param.coeff.ground.update = 2.9224607,
                            param.coeff.travel = 17.70182, param.power.travel = 0.2377348)
      
      writeLines(paste0("new.rating.home = ", elo.game$new.rating.home, " | new.rating.away = ", elo.game$new.rating.away))
      writeLines(paste0("new.gnd.adj.home = ", elo.game$new.rating.ground.adj.home, " | new.gnd.adj.away = ", elo.game$new.rating.ground.adj.away))
      
      writeLines("---")
    }
  }
  
  # for (i in 1 : n.games) {
  #   team.home <- fixture.round[i, "team.home"]
  #   team.away <- fixture.round[i, "team.away"]
  # }
}

