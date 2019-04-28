

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

SimulateGameScore <- function(margin.exp.home, 
                              margin.error.sigma = 38.5, 
                              lose.score.mu = 75.2, 
                              lose.score.sigma = 19.2) {
  
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

  score.sim <- 
    list(
      margin.home = margin.sim.home, 
      lose = lose.score.sim, 
      points.home = score.points.home, 
      points.away = score.points.away
    )
}

SimulateRegularSeasonElo <- function(season,
                                     fixture.season, 
                                     team.data,
                                     ground.data,
                                     team.dictionary,
                                     ground.location,
                                     travel.distance,
                                     elo.params,
                                     rating.time.series,
                                     margin.error.sigma = 38.5, 
                                     lose.score.mu = 75.2, 
                                     lose.score.sigma = 19.2) {
  
  yes.active <- (team.data$season.start <= season) & (team.data$season.end >= season)
  teams.active <- rownames(team.data[yes.active, ])
  
  n.teams <- length(teams.active)
  zero.vector <- rep(0, n.teams)
  
  ladder.data <- 
    data.frame(
      team = teams.active,
      played = zero.vector,
      won = zero.vector,
      lost = zero.vector,
      drawn = zero.vector, 
      prem.pts = zero.vector,
      score.for = zero.vector,
      score.against = zero.vector,
      percentage = zero.vector,
      ladder.posn = zero.vector,
      stringsAsFactors = FALSE
    )
  
  n.rounds <- 
    fixture.season %>%
      pull(rnd) %>%
      unique() %>% 
      length()
  
  # Set-up the season if simulating a whole new season, which is determined by 
  # checking to see if the first game to be simulated is Game 1 of Round 1
  if (fixture.season[[1, "rnd"]] == 1 && fixture.season[[1, "game"]] == 1) {
    
    setup <- 
      SetupSeason(
        team.data, 
        rating.time.series, 
        season, 
        elo.params
      )
    
    team.data <- setup$team.data
    rating.time.series <- setup$rating.time.series
    
    # team.data <- RegressRatings(season.current, team.data, elo.params)
    # 
    # yes.active <- 
    #   (team.data$season.start <= season) & (team.data$season.end >= season)
    # 
    # rating.time.series[paste(season, 'start', sep = ' '), yes.active] <- 
    #   team.data$rating[yes.active]
    
  }
  
  # TODO: If starting halfway though season, load current laddeer

  txt <- c(paste0("Season: ", season), "---")
  
  for (i in 1 : n.rounds) {
    
    fixture.round <- fixture.season %>% filter(rnd == i)
    n.games <- nrow(fixture.round)
    
    for (j in 1 : n.games) {
      
      game.info <- list(
        season = season,
        round = paste0("R", i)
      )
      
      sim.game.result <- 
        SimulateRegularSeasonGame(
          game.info,
          fixture.round,
          game.number = j,
          team.dictionary,
          team.data,
          ground.data,
          ground.location,
          travel.distance,
          elo.params,
          rating.time.series,
          ladder.data
        )
      
      game.info <- sim.game.result$game.info
      # pred <- sim.game.result$pred
      # score.sim <- sim.game.result$score.sim
      elo.game <- sim.game.result$elo.game
      team.data <- sim.game.result$team.data
      ground.data <- sim.game.result$ground.data
      rating.time.series <- sim.game.result$rating.time.series
      ladder.data <- sim.game.result$ladder.data

      txt <- append(txt, paste0("R", i, " G", j, ": ", game.info$team.home, " vs ", game.info$team.away, " at ", game.info$ground))
      txt <- append(txt, paste0("rating.home = ", team.data[game.info$team.home, 'rating'], " | rating.away = ", team.data[game.info$team.away, 'rating']))
      txt <- append(txt, paste0("gnd.adj.home = ", CalculateGroundAdj(game.info$team.home, game.info$ground, ground.data), 
                                " | gnd.adj.away = ", CalculateGroundAdj(game.info$team.away, game.info$ground, ground.data)))
      
      txt <- append(txt, paste0("home = ", game.info$score.points.home, " | away = ", game.info$score.points.away))
            
      txt <- append(txt, paste0("new.rating.home = ", elo.game$new.rating.home, " | new.rating.away = ", elo.game$new.rating.away))
      txt <- append(txt, paste0("new.gnd.adj.home = ", elo.game$new.rating.ground.adj.home, " | new.gnd.adj.away = ", elo.game$new.rating.ground.adj.away))
      
      txt <- append(txt, "---")
      
    }
    
    ladder.order <- 
      with(
        ladder.data, 
        order(
          prem.pts,
          percentage,
          decreasing = TRUE
        )
      )
    
    ladder.data$ladder.posn[ladder.order] <- seq(1 : nrow(ladder.data))
    
  }
  
  # writeLines(txt, "temp_sim_out.txt")
  
  sim.data <- 
    list(
      team.data = team.data, 
      ground.data = ground.data, 
      rating.time.series = rating.time.series, 
      ladder.data = ladder.data
    )
  
}



SetupRegularSeasonGame <- function(game.info,
                                   fixture.round, 
                                   game.number, 
                                   team.dictionary) {
  
  # game.info <-
  #   list(
  #     team.home = team.dictionary[[fixture.round[game.number, "team.home"]]],
  #     team.away = team.dictionary[[fixture.round[game.number, "team.away"]]],
  #     ground = fixture.round[game.number, "ground"]
  #   )
  
  game.info$team.home <-
    team.dictionary[[fixture.round[game.number, "team.home"]]]

  game.info$team.away <-
    team.dictionary[[fixture.round[game.number, "team.away"]]]

  game.info$ground <- fixture.round[game.number, "ground"]

  game.info
  
}


GetFinalsGround <- function(this.final, game.info, team.data) {
  
  if (this.final == "GF") {
    ground <- "M.C.G."
  } else {
    ground <- team.data[game.info$team.home, "home.ground"] 
  }
  #TODO: what if home ground should be something else, e.g. Cats at MCG
  
  ground
  
}


ProcessOutputsRegularSeasonGame <- function(game.info, ladder.data) {
  
  prem.pts.win <- 4
  prem.pts.draw <- 2
  prem.pts.loss <- 0
  
  yes.team.home.row <- ladder.data$team == game.info$team.home
  yes.team.away.row <- ladder.data$team == game.info$team.away
  
  ladder.data[yes.team.home.row, "played"] <- 
    ladder.data[yes.team.home.row, "played"] + 1
  ladder.data[yes.team.away.row, "played"] <- 
    ladder.data[yes.team.away.row, "played"] + 1
  
  if (game.info$score.points.home > game.info$score.points.away) {
    ladder.data[yes.team.home.row, "won"] <- 
      ladder.data[yes.team.home.row, "won"] + 1
    ladder.data[yes.team.home.row, "prem.pts"] <- 
      ladder.data[yes.team.home.row, "prem.pts"] + prem.pts.win
    ladder.data[yes.team.away.row, "lost"] <- 
      ladder.data[yes.team.away.row, "lost"] + 1
    ladder.data[yes.team.away.row, "prem.pts"] <-
      ladder.data[yes.team.away.row, "prem.pts"] + prem.pts.loss
    
  } else if (game.info$score.points.home < game.info$score.points.away) {
    ladder.data[yes.team.home.row, "lost"] <-
      ladder.data[yes.team.home.row, "lost"] + 1
    ladder.data[yes.team.home.row, "prem.pts"] <-
      ladder.data[yes.team.home.row, "prem.pts"] + prem.pts.loss
    ladder.data[yes.team.away.row, "won"] <- 
      ladder.data[yes.team.away.row, "won"] + 1
    ladder.data[yes.team.away.row, "prem.pts"] <-
      ladder.data[yes.team.away.row, "prem.pts"] + prem.pts.win
    
  } else {
    ladder.data[yes.team.home.row, "drawn"] <- 
      ladder.data[yes.team.home.row, "drawn"] + 1
    ladder.data[yes.team.home.row, "prem.pts"] <- 
      ladder.data[yes.team.home.row, "prem.pts"] + prem.pts.draw
    ladder.data[yes.team.away.row, "drawn"] <- 
      ladder.data[yes.team.away.row, "drawn"] + 1
    ladder.data[yes.team.away.row, "prem.pts"] <-
      ladder.data[yes.team.away.row, "prem.pts"] + prem.pts.draw
    
  }
  
  ladder.data[yes.team.home.row, "score.for"] <-
    ladder.data[yes.team.home.row, "score.for"] + game.info$score.points.home
  ladder.data[yes.team.away.row, "score.for"] <- 
    ladder.data[yes.team.away.row, "score.for"] + game.info$score.points.away
  
  ladder.data[yes.team.home.row, "score.against"] <- 
    ladder.data[yes.team.home.row, "score.against"] +
    game.info$score.points.away
  ladder.data[yes.team.away.row, "score.against"] <- 
    ladder.data[yes.team.away.row, "score.against"] + 
    game.info$score.points.home
  ladder.data[, "percentage"] <-
    ladder.data[, "score.for"] / ladder.data[, "score.against"] * 100
  
  ladder.data
  
}


ProcessOutputsFinalsGame <- function(game.info, finals, this.final) {
  
  # print(game.info)
  # print(finals)
  # print(this.final)
  
  yes.this.final <- finals$rnd == this.final
  
  finals[yes.this.final, "team.home"] <- game.info$team.home
  finals[yes.this.final, "team.away"] <- game.info$team.away
  finals[yes.this.final, "ground"] <- game.info$ground
  
  finals[yes.this.final, "team.home.score"] <- game.info$score.points.home
  finals[yes.this.final, "team.away.score"] <- game.info$score.points.away
  
  if (game.info$score.points.home > game.info$score.points.away) {
    finals[yes.this.final, "winner"] <- game.info$team.home
    finals[yes.this.final, "loser"] <- game.info$team.away
    
  } else if (game.info$score.points.away > game.info$score.points.home) {
    finals[yes.this.final, "winner"] <- game.info$team.away
    finals[yes.this.final, "loser"] <- game.info$team.home
    
  } else {
    print("ERROR: Draw")  # TODO: what happens in a draw??? Re-sim, but with different mean, sd, and add extra points to orig score?
  
  }
  
  # print(finals)
  
  finals
  
}



SimulateGame <- function(game.info,
                         team.data,
                         ground.data,
                         ground.location,
                         travel.distance,
                         elo.params,
                         rating.time.series) {
  
  pred <- 
    PredictGame(
      game.info$team.home,
      game.info$team.away,
      game.info$ground,
      team.data, 
      ground.data, 
      ground.location, 
      travel.distance, 
      commission = 0,
      param.spread = elo.params$spread, 
      param.margin = elo.params$margin,
      param.coeff.travel = elo.params$coeff.travel,
      param.power.travel = elo.params$power.travel
    )
  
  score.sim <- SimulateGameScore(pred$margin.exp.home)
  game.info$score.points.home <- score.sim$points.home
  game.info$score.points.away <- score.sim$points.away
  
  elo.game <- 
    DoGameElo(
      game.info, 
      team.data, 
      ground.location, 
      ground.data, 
      travel,distance,
      param.spread = elo.params$spread,
      param.margin = elo.params$margin,
      param.coeff.rating.update = elo.params$coeff.rating.update,
      param.coeff.ground.update = elo.params$coeff.ground.update,
      param.coeff.travel = elo.params$coeff.travel, 
      param.power.travel = elo.params$power.travel
    )
  
  elo.data <-
    UpdateEloRatings(
      team.data, 
      ground.data, 
      rating.time.series, 
      game.info, 
      elo.game
    )
  
  team.data <- elo.data$team.data
  ground.data <- elo.data$ground.data
  rating.time.series <- elo.data$rating.time.series
  
  result <-
    list(
      game.info = game.info,
      pred = pred,
      score.sim = score.sim,
      elo.game = elo.game,
      team.data = team.data,
      ground.data = ground.data,
      rating.time.series = rating.time.series
    )
  
}


SimulateRegularSeasonGame <- function(game.info,
                                      fixture.round,
                                      game.number,
                                      team.dictionary,
                                      team.data,
                                      ground.data,
                                      ground.location,
                                      travel.distance,
                                      elo.params,
                                      rating.time.series,
                                      ladder.data) {
  
  game.info <- 
    SetupRegularSeasonGame(
      game.info, 
      fixture.round, 
      game.number, 
      team.dictionary
    )
  
  sim.game.result <-
    SimulateGame(
      game.info,
      team.data,
      ground.data,
      ground.location,
      travel.distance,
      elo.params,
      rating.time.series
    )
  
  ladder.data <- 
    ProcessOutputsRegularSeasonGame(sim.game.result$game.info, ladder.data)
  
  result <- list(
    game.info = sim.game.result$game.info,
    pred = sim.game.result$pred,
    score.sim = sim.game.result$score.sim,
    elo.game = sim.game.result$elo.game,
    elo.data = sim.game.result$elo.data,
    team.data = sim.game.result$team.data,
    ground.data = sim.game.result$ground.data,
    rating.time.series = sim.game.result$rating.time.series,
    ladder.data = ladder.data
  )
  
}



SimulateFinalsGame <- function(game.info,
                               finals,
                               this.final,
                               ladder.data,
                               team.data,
                               ground.data,
                               ground.location,
                               travel.distance,
                               elo.params,
                               rating.time.series) {
  
  # Teams playing
  switch(
    
    this.final,
    
    EF1 = {
      # Elimination final 1: 5th vs 8th
      game.info$round <- "QF/EF"
      game.info$team.home <- 
        GetTeamAtLadderPosition(ladder.data, req.position = 5)
      game.info$team.away <- 
        GetTeamAtLadderPosition(ladder.data, req.position = 8)
    },
    
    EF2 = {
      # Elimination final 2: 6th vs 7th
      game.info$round <- "QF/EF"
      game.info$team.home <- 
        GetTeamAtLadderPosition(ladder.data, req.position = 6)
      game.info$team.away <- 
        GetTeamAtLadderPosition(ladder.data, req.position = 7)
    },
    
    QF1 = {
      # Qualifying final 1: 1st vs 4th
      game.info$round <- "QF/EF"
      game.info$team.home <- 
        GetTeamAtLadderPosition(ladder.data, req.position = 1)
      game.info$team.away <- 
        GetTeamAtLadderPosition(ladder.data, req.position = 4)
    },
    
    QF2 = {
      # Qualifying final 2: 2nd vs 3rd
      game.info$round <- "QF/EF"
      game.info$team.home <- 
        GetTeamAtLadderPosition(ladder.data, req.position = 2)
      game.info$team.away <- 
        GetTeamAtLadderPosition(ladder.data, req.position = 3)
    },
    
    SF1 = {
      # Semi final 1: Loser QF1 vs Winner EF1
      game.info$round <- "SF"
      game.info$team.home <- GetTeamAtFinalsStatus(finals, "QF1", "loser")
      game.info$team.away <- GetTeamAtFinalsStatus(finals, "EF1", "winner")
    },
    
    SF2 = {
      # Semi final 2: Loser QF2 vs Winner EF2
      game.info$round <- "SF"
      game.info$team.home <- GetTeamAtFinalsStatus(finals, "QF2", "loser")
      game.info$team.away <- GetTeamAtFinalsStatus(finals, "EF2", "winner")
    },
    
    PF1 = {
      # Preliminary final 1: Winner QF1 vs Winner SF2
      game.info$round <- "PF"
      game.info$team.home <- GetTeamAtFinalsStatus(finals, "QF1", "winner")
      game.info$team.away <- GetTeamAtFinalsStatus(finals, "SF2", "winner")
    },
    
    PF2 = {
      # Preliminary final 2: Winner QF2 vs Winner SF1
      game.info$round <- "PF"
      game.info$team.home <- GetTeamAtFinalsStatus(finals, "QF2", "winner")
      game.info$team.away <- GetTeamAtFinalsStatus(finals, "SF1", "winner")
    },
    
    GF = {
      # Grand final: Winner PF1 vs Winner PF2 (MCG)
      game.info$round <- "GF"
      game.info$team.home <- GetTeamAtFinalsStatus(finals, "PF1", "winner")
      game.info$team.away <- GetTeamAtFinalsStatus(finals, "PF2", "winner")
    },
    
    {
      print("ERROR: Finals string does not match accepted list")
    }
    
  )
  
  # Ground
  game.info$ground <- GetFinalsGround(this.final, game.info, team.data) 
  
  sim.game.result <-
    SimulateGame(
      game.info,
      team.data,
      ground.data,
      ground.location,
      travel.distance,
      elo.params,
      rating.time.series
    )
  
  finals <- 
    ProcessOutputsFinalsGame(sim.game.result$game.info, finals, this.final)
  
  result <- list(
    game.info = sim.game.result$game.info,
    pred = sim.game.result$pred,
    score.sim = sim.game.result$score.sim,
    elo.game = sim.game.result$elo.game,
    elo.data = sim.game.result$elo.data,
    team.data = sim.game.result$team.data,
    ground.data = sim.game.result$ground.data,
    rating.time.series = sim.game.result$rating.time.series,
    finals = finals
  )
  
}


GetTeamAtLadderPosition <- function(ladder.data, req.position) {
  team <-
    ladder.data %>%
      filter(ladder.posn == req.position) %>%
      .[["team"]]
}

GetTeamAtFinalsStatus <- function(finals, req.final, winner.or.loser) {
  team <-
    finals %>%
      filter(rnd == req.final) %>%
      .[[winner.or.loser]]
}



SimulateFinalsElo <- function(season,
                              ladder.data,
                              team.data,
                              ground.data,
                              ground.location,
                              travel.distance,
                              elo.params,
                              rating.time.series) {
  
  finals.rounds <-
    c("EF1", "EF2", "QF1", "QF2", "SF1", "SF2", "PF1", "PF2", "GF")
  n.games <- length(finals.rounds)
  
  finals <-
    data.frame(
      rnd = finals.rounds,
      team.home = character(n.games),
      team.away = character(n.games),
      ground = character(n.games),
      team.home.score = rep(0, n.games),
      team.away.score = rep(0, n.games),
      winner = character(n.games),
      loser = character(n.games),
      stringsAsFactors = FALSE
    )
  
  for (this.final in finals.rounds) {
    
    game.info <- list(
      season = season
    )
    
    sim.game.result <-
      SimulateFinalsGame(
        game.info,
        finals,
        this.final,
        ladder.data,
        team.data,
        ground.data,
        ground.location,
        travel.distance,
        elo.params,
        rating.time.series
      )
    
    game.info <- sim.game.result$game.info
    # pred <- sim.game.result$pred
    # score.sim <- sim.game.result$score.sim
    # elo.game <- sim.game.result$elo.game
    # elo.data <- sim.game.result$elo.data
    team.data <- sim.game.result$team.data
    ground.data <- sim.game.result$ground.data
    rating.time.series <- sim.game.result$rating.time.series
    finals <- sim.game.result$finals
    
  }
  
  result <-
    list(
      finals = finals,
      team.data = team.data,
      ground.data = ground.data,
      rating.time.series = rating.time.series
    )
  
}


SimulateFullSeasonElo <- function(season,
                                  fixture.season,
                                  team.data,
                                  ground.data,
                                  team.dictionary,
                                  ground.location,
                                  travel.distance,
                                  elo.params,
                                  rating.time.series,
                                  margin.error.sigma = 38.5,
                                  lose.score.mu = 75.2,
                                  lose.score.sigma = 19.2) {
  
  sim.reg.result <- 
    SimulateRegularSeasonElo(
      season,
      fixture.season,
      team.data,
      ground.data,
      team.dictionary,
      ground.location,
      travel.distance,
      elo.params,
      rating.time.series,
      margin.error.sigma,
      lose.score.mu,
      lose.score.sigma
    )
  
  team.data <- sim.reg.result$team.data
  ground.data <- sim.reg.result$ground.data
  rating.time.series <- sim.reg.result$rating.time.series
  ladder.data <- sim.reg.result$ladder.data
  
  sim.finals.result <- 
    SimulateFinalsElo(
      season,
      ladder.data,
      team.data,
      ground.data,
      ground.location,
      travel.distance,
      elo.params,
      rating.time.series
    )
  
  finals <- sim.finals.result$finals
  team.data <- sim.finals.result$team.data
  ground.data <- sim.finals.result$ground.data
  rating.time.series <- sim.finals.result$rating.time.series
  
  result <-
    list(
      finals = finals,
      ladder.data = ladder.data,
      team.data = team.data,
      ground.data = ground.data,
      rating.time.series = rating.time.series
    )
}


SimulateFullSeasonEloMany <- function(season, 
                                      fixture.season, 
                                      n.itns,
                                      team.data, 
                                      ground.data, 
                                      ground.location,
                                      team.dictionary,
                                      travel.distance,
                                      elo.params,
                                      rating.time.series,
                                      margin.error.sigma = 38.5,
                                      lose.score.mu = 75.2,
                                      lose.score.sigma = 19.2) {
  
  # n.rounds <- fixture.season %>% unique() %>% length()
  n.teams <- 
    append(fixture.season$team.home, fixture.season$team.away) %>% 
      unique() %>% 
      length()
  
  n.finals.games <- 9
  
  ladder.many <- 
    data.frame(
      sim.itn = integer(),
      team = character(),
      played = integer(),
      won = integer(),
      lost = integer(),
      drawn = integer(), 
      prem.pts = integer(), 
      score.for = numeric(), 
      score.against = numeric(), 
      percentage = numeric(),
      ladder.posn = integer(),
      stringsAsFactors = FALSE
    )
  
  finals.many <-
    data.frame(
      rnd = character(),
      team.home = character(),
      team.away = character(),
      ground = character(),
      team.home.score = numeric(),
      team.away.score = numeric(),
      winner = character(),
      loser = character(),
      stringsAsFactors = FALSE
    )
  
  for (itn in 1 : n.itns) {
    
    sim.data <- 
      SimulateFullSeasonElo(
        season,
        fixture.season,
        team.data,
        ground.data,
        team.dictionary,
        ground.location,
        travel.distance,
        elo.params,
        rating.time.series,
        margin.error.sigma,
        lose.score.mu,
        lose.score.sigma
      )
    
    # sim.data <- SimulateSeasonElo(season, fixture.season, team.data, ground.data, ground.location, travel.distance)
    
    # Sim results:
    # finals = finals,
    # ladder.data = ladder.data,
    # team.data = team.data,
    # ground.data = ground.data,
    # rating.time.series = rating.time.series
    
    ladder.many <- 
      rbind(
        ladder.many, 
        cbind(
          sim.itn = rep(itn, n.teams), 
          sim.data$ladder.data
        )
      )
    
    finals.many <-
      rbind(
        finals.many,
        cbind(
          sim.itn = rep(itn, n.finals.games), 
          sim.data$finals
        )
      )
    
  }
  
  result <-
    list(
      ladder.many = ladder.many,
      finals.many = finals.many
    )
  
}

