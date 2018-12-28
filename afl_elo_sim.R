

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

  # writeLines(paste0("margin.home = ", margin.sim.home, " | lose.score = ", lose.score.sim, " | home = ", score.points.home, " | away = ", score.points.away))

  score.sim <- list(margin.home = margin.sim.home, lose = lose.score.sim, points.home = score.points.home, points.away = score.points.away)
}

SimulateSeasonElo <- function(season, fixture.season, 
                              team.data, ground.data, ground.location, travel.distance,
                              margin.error.sigma = 38.5, lose.score.mu = 75.2, lose.score.sigma = 19.2) {
  
  prem.pts.win <- 4
  prem.pts.draw <- 2
  prem.pts.loss <- 0
  
  yes.active <- (team.data$season.start <= season) & (team.data$season.end >= season)
  teams.active <- rownames(team.data[yes.active, ])
  n.teams <- length(teams.active)
  zero.vector <- rep(0, n.teams)
  ladder.data <- data.frame(team = teams.active, played = zero.vector, won = zero.vector, lost = zero.vector, drawn = zero.vector, 
                            prem.pts = zero.vector, score.for = zero.vector, score.against = zero.vector, percentage = zero.vector,
                            ladder.posn = zero.vector, stringsAsFactors = FALSE)
  
  # !!! NEED TO REGRESS RATINGS BEFORE COMMENCING SEASON SIM !!!
  # BUt only do so if simualting from season start
  
  
  n.rounds <- max(fixture.season[, "rnd"])
  
  txt <- c("")
  
  for (i in 1 : n.rounds) {
    fixture.round <- fixture.season %>% filter(rnd == i)
    n.games <- nrow(fixture.round)
    for (j in 1 : n.games) {
      game.info <- list(season = season, rnd = i, game = j)
      game.info$team.home <- team.dictionary[[fixture.round[j, "team.home"]]]
      game.info$team.away <- team.dictionary[[fixture.round[j, "team.away"]]]
      game.info$ground <- fixture.round[j, "ground"]
      txt <- append(txt, paste0("R", i, " G", j, ": ", game.info$team.home, " vs ", game.info$team.away, " at ", game.info$ground))
      # writeLines(paste0("R", i, " G", j, ": ", game.info$team.home, " vs ", game.info$team.away, " at ", game.info$ground))
      pred <- PredictGame(game.info$team.home, game.info$team.away, game.info$ground,
                          team.data, ground.data, ground.location, travel.distance, 
                          param.spread = 400, 
                          param.margin = 0.02395478,
                          param.coeff.travel = 17.70182, param.power.travel = 0.2377348)
      score.sim <- SimulateGameScore(pred$margin.exp.home)
      game.info$score.points.home <- score.sim$points.home
      game.info$score.points.away <- score.sim$points.away
      
      txt <- append(txt, paste0("rating.home = ", team.data[game.info$team.home, 'rating'], " | rating.away = ", team.data[game.info$team.away, 'rating']))
      txt <- append(txt, paste0("gnd.adj.home = ", CalculateGroundAdj(game.info$team.home, game.info$ground, ground.data), 
                                " | gnd.adj.away = ", CalculateGroundAdj(game.info$team.away, game.info$ground, ground.data)))
      # writeLines(paste0("rating.home = ", team.data[game.info$team.home, 'rating'], " | rating.away = ", team.data[game.info$team.away, 'rating']))
      # writeLines(paste0("gnd.adj.home = ", CalculateGroundAdj(game.info$team.home, game.info$ground, ground.data), 
      #                   " | gnd.adj.away = ", CalculateGroundAdj(game.info$team.away, game.info$ground, ground.data)))
      
      txt <- append(txt, paste0("home = ", game.info$score.points.home, " | away = ", game.info$score.points.away))
      
      elo.game <- DoGameElo(game.info, 
                            team.data, 
                            ground.location, ground.data, travel,distance,
                            param.spread = 400,
                            param.margin = 0.02395478,
                            param.coeff.rating.update = 70.34218,
                            param.coeff.ground.update = 2.9224607,
                            param.coeff.travel = 17.70182, param.power.travel = 0.2377348)
      
      elo.data <- UpdateEloRatings(team.data, ground.data, rating.time.series, game.info, elo.game)
      team.data <- elo.data$team.data
      ground.data <- elo.data$ground.data
      rating.time.series <- elo.data$rating.time.series
      
      txt <- append(txt, paste0("new.rating.home = ", elo.game$new.rating.home, " | new.rating.away = ", elo.game$new.rating.away))
      txt <- append(txt, paste0("new.gnd.adj.home = ", elo.game$new.rating.ground.adj.home, " | new.gnd.adj.away = ", elo.game$new.rating.ground.adj.away))
      
      # writeLines(paste0("new.rating.home = ", elo.game$new.rating.home, " | new.rating.away = ", elo.game$new.rating.away))
      # writeLines(paste0("new.gnd.adj.home = ", elo.game$new.rating.ground.adj.home, " | new.gnd.adj.away = ", elo.game$new.rating.ground.adj.away))
      
      txt <- append(txt, "---")
      # writeLines("---")
      
      yes.team.home.row <- ladder.data$team == game.info$team.home
      yes.team.away.row <- ladder.data$team == game.info$team.away
      ladder.data[yes.team.home.row, "played"] <- ladder.data[yes.team.home.row, "played"] + 1
      ladder.data[yes.team.away.row, "played"] <- ladder.data[yes.team.away.row, "played"] + 1
      
      
      if (game.info$score.points.home > game.info$score.points.away) {
        ladder.data[yes.team.home.row, "won"] <- ladder.data[yes.team.home.row, "won"] + 1
        ladder.data[yes.team.home.row, "prem.pts"] <- ladder.data[yes.team.home.row, "prem.pts"] + prem.pts.win
        ladder.data[yes.team.away.row, "lost"] <- ladder.data[yes.team.away.row, "lost"] + 1
        ladder.data[yes.team.away.row, "prem.pts"] <- ladder.data[yes.team.away.row, "prem.pts"] + prem.pts.loss
      } else if (game.info$score.points.home < game.info$score.points.away) {
        ladder.data[yes.team.home.row, "lost"] <- ladder.data[yes.team.home.row, "lost"] + 1
        ladder.data[yes.team.home.row, "prem.pts"] <- ladder.data[yes.team.home.row, "prem.pts"] + prem.pts.loss
        ladder.data[yes.team.away.row, "won"] <- ladder.data[yes.team.away.row, "won"] + 1
        ladder.data[yes.team.away.row, "prem.pts"] <- ladder.data[yes.team.away.row, "prem.pts"] + prem.pts.win
      } else {
        ladder.data[yes.team.home.row, "drawn"] <- ladder.data[yes.team.home.row, "drawn"] + 1
        ladder.data[yes.team.home.row, "prem.pts"] <- ladder.data[yes.team.home.row, "prem.pts"] + prem.pts.draw
        ladder.data[yes.team.away.row, "drawn"] <- ladder.data[yes.team.away.row, "drawn"] + 1
        ladder.data[yes.team.away.row, "prem.pts"] <- ladder.data[yes.team.away.row, "prem.pts"] + prem.pts.draw
      }
      ladder.data[yes.team.home.row, "score.for"] <- ladder.data[yes.team.home.row, "score.for"] + game.info$score.points.home
      ladder.data[yes.team.away.row, "score.for"] <- ladder.data[yes.team.away.row, "score.for"] + game.info$score.points.away
      ladder.data[yes.team.home.row, "score.against"] <- ladder.data[yes.team.home.row, "score.against"] + game.info$score.points.away
      ladder.data[yes.team.away.row, "score.against"] <- ladder.data[yes.team.away.row, "score.against"] + game.info$score.points.home
      ladder.data[, "percentage"] <- ladder.data[, "score.for"] / ladder.data[, "score.against"] * 100
      
      ladder.order <- with(ladder.data, order(prem.pts, percentage, decreasing = TRUE))
      ladder.data$ladder.posn[ladder.order] <- seq(1 : nrow(ladder.data))
      
    }
  }
  
  # writeLines(txt, "temp_sim_out.txt")
  
  
  
  sim.data <- list(team.data = team.data, ground.data = ground.data, rating.time.series = rating.time.series, ladder.data = ladder.data)
  sim.data
}

SimulateFinalsElo <- function(ladder.data) {
  
  finals.rounds <- c("EF1", "EF2", "QF1", "QF2", "SF1", "SF2", "PF1", "PF2", "GF")
  n.games <- length(finals)
  
  zeros.vector <- rep(0, n.games)
  finals <- data.frame(rnd = finals.rounds, team.home = character(n.games), team.away = character(n.games), ground = character(n.games),
                       team.home.score = zeros.vector, team.away.score = zeros.vector,
                       winner = character(n.games), loser = character(n.games))
  
  
  # Elimination final 1: 5th vs 8th
  this.final <- finals$rnd == "EF1"
  team.home <- ladder.data %>% filter(ladder.posn == 5) %>% .[["team"]]
  team.away <- ladder.data %>% filter(ladder.posn == 8) %>% .[["team"]]
  finals[this.final, "team.home"] <- team.home
  finals[this.final, "team.away"] <- team.away
  finals[this.final, "ground"] <- team.data[team.home, "home.ground"]  #TODO: what if home ground should be something else, e.g. Cats at MCG
  game.info <- finals[this.final, ]
  pred <- PredictGame(game.info$team.home, game.info$team.away, game.info$ground,
                      team.data, ground.data, ground.location, travel.distance, 
                      param.spread = 400, 
                      param.margin = 0.02395478,
                      param.coeff.travel = 17.70182, param.power.travel = 0.2377348)
  score.sim <- SimulateGameScore(pred$margin.exp.home)
  finals$score.points.home <- score.sim$points.home
  finals$score.points.away <- score.sim$points.away
  game.info <- finals[this.final, ]
  elo.game <- DoGameElo(game.info, 
                        team.data, 
                        ground.location, ground.data, travel,distance,
                        param.spread = 400,
                        param.margin = 0.02395478,
                        param.coeff.rating.update = 70.34218,
                        param.coeff.ground.update = 2.9224607,
                        param.coeff.travel = 17.70182, param.power.travel = 0.2377348)
  elo.data <- UpdateEloRatings(team.data, ground.data, rating.time.series, game.info, elo.game)
  team.data <- elo.data$team.data
  ground.data <- elo.data$ground.data
  rating.time.series <- elo.data$rating.time.series
  if (game.info$score.points.home > game.info$score.points.away) {
    finals[this.final, "winner"] <- team.home
    finals[this.final, "loser"] <- team.away
  } else if (game.info$score.points.away > score.points.home) {
    finals[this.final, "winner"] <- team.away
    finals[this.final, "loser"] <- team.home
  } else {
    print("ERROR: Draw")  # TODO: what happens in a draw??? Re-sim, but with different mean, sd, and add extra points to orig score?
  }
  
  # Elimination final 2: 6th vs 7th
  # Inputs: game.info (home, away), team.data (so ground can get determined by func)
  #         team.data, ground,data, ground.location, travel.distance
  #         param.* (i.e. all of them, since predicting and elo calculating)
  #         AND any exceptions to ground location, e.g. Cats playing at MCG, GF always at MCG
  
  # Qualifying final 1: 1st vs 4th
  
  # Qualifying final 2: 2nd vs 3rd
  
  # Semi final 1: Loser QF1 vs Winner EF1
  game.info$team.home <- finals %>% filter(rnd == "QF1") %>% .[["loser"]]
  game.info$team.away <- finals %>% filter(rnd == "EF1") %>% .[["winner"]]
  
  # Semi final 2: Loser QF2 vs Winner EF2
  
  # Preliminary final 1: Winner QF1 vs Winner SF2
  
  # Preliminary final 2: Winner Winner QF2 vs Winner SF1
  
  # Grand final: Winner PF1 vs Winner PF2 (MCG)
  
}



SimulateSeasonEloMany <- function(season, fixture.season, n.itns,
                                  team.data, ground.data, ground.location, travel.distance,
                                  margin.error.sigma = 38.5, lose.score.mu = 75.2, lose.score.sigma = 19.2) {
  
  n.rounds <- fixture.season %>% unique() %>% length()
  n.teams <- append(fixture.season$team.home, fixture.season$team.away) %>% unique() %>% length()
  
  ladder.many <- data.frame(sim.itn = integer(),
                            team = character(), played = integer(), won = integer(), lost = integer(), drawn = integer(), 
                            prem.pts = integer(), score.for = numeric(), score.against = numeric(), percentage = numeric(),
                            ladder.posn = integer(), stringsAsFactors = FALSE)
  
  for (itn in 1 : n.itns) {
    sim.data <- SimulateSeasonElo(season, fixture.season, team.data, ground.data, ground.location, travel.distance)
    ladder.many <- rbind(ladder.many, cbind(sim.itn = rep(itn, n.teams), sim.data$ladder.data))
  }
  
  ladder.many
}

# tic()
# ladder.many <- SimulateSeasonEloMany(2018, fixture.season, 5000, team.data.run, ground.data.run, ground.location, travel.distance)
# toc()
# ladder.many %>% filter(team == "hawthorn") %>% group_by(ladder.posn) %>% count() %>% mutate(p = n/5000) %>% ggplot(aes(ladder.posn, p)) + geom_col()


