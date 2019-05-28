
CalculateGroundAdj <- function(team, ground, ground.data) {
  ground.adj <- ground.data[ground, team]
  ground.adj
}

CalculateTravelAdj <- function(team, 
                               ground, 
                               team.data,
                               ground.location,
                               travel.distance,
                               param.coeff.travel,
                               param.power.travel) {
  
  team.location <- team.data[team, 'location']
  distance <- travel.distance[ground.location[ground, ], team.location]
  
  travel.adj <- -param.coeff.travel * (distance ^ param.power.travel)
  
  travel.adj
}


CalculateResultExp <- function(delta.ratings, param.spread) {
  #result.exp <- 1 / (10 ^ (-delta.ratings / param.spread) + 1)
  result.exp <- pnorm(delta.ratings, mean = 0, sd = param.spread / 2 * sqrt(2))
}


CalculateMarginExp <- function(delta.ratings, param.spread, param.margin) {
  #margin.exp <- delta.ratings / (param.margin * param.spread)
  margin.exp <- delta.ratings / (param.margin * param.spread / 2 * sqrt(2)) # TODO CHECK THIS!!! Works, but need to do maths...
}


CalculateResultAct <- function(margin.act, param.margin) {
  #result.act <- 1 / (10 ^ (-param.margin * margin.act) + 1)
  result.act <- pnorm(param.margin * margin.act, mean = 0, sd = 1) # TODO CHECK THIS!!! Works, but need to do maths...
}


CalculateRatingNew <- function(rating, result.exp, result.act, param.coeff.update) {
  rating.new <- rating + param.coeff.update * (result.act - result.exp)
}


CalculateRegressRating <- function(rating, param.rating.mean, param.regress) {
  rating.new <- 
    ((1 - param.regress) * rating) + (param.regress * param.rating.mean)
}


RegressRatings <- function(season, team.data, elo.params) {

  # Ratings are only regressed to mean for teams that played previous season and
  # are also active this season
  yes.regress <- 
    (team.data$season.start < season) & (team.data$season.end >= season)
  
  if (any(yes.regress)) {
    
    team.data$rating[yes.regress] <- 
      CalculateRegressRating(
        team.data$rating[yes.regress], 
        elo.params$rating.mean, 
        elo.params$regress.rating
      )
    
    # TODO: Check if need to regress ground ratings?
    
  }
 
  team.data
   
}


SetupSeason <- function(team.data,
                        rating.time.series,
                        season,
                        elo.params) {
  
  team.data <- RegressRatings(season, team.data, elo.params)
  
  team.data$yes.active <- 
    (team.data$season.start <= season) & 
    (team.data$season.end >= season)
  
  # Record ratings of active teams at season start (after regression)
  # TODO: Refactor - using rownames in this way is bad and hard to read and
  # creates weird looking code like that below
  rating.time.series[
    paste(season, 'start', sep = ' '), 
    team.data$yes.active
  ] <-
    team.data$rating[team.data$yes.active]
  
  # Move the Swans to Sydney in 1982
  if (
    (team.data['sydney', 'location'] != 'Sydney') &
    (season >= 1982)
  ) {
    team.data['sydney', 'location'] <- 'Sydney'
  }
  
  # Change the home ground of West Coast Eagles and Fremantle to 
  # Perth Stadium in 2018
  if (
    (team.data['west.coast.eagles', 'home.ground'] != 'Perth Stadium') &
    (season >= 2018)
  ) {
    team.data['west.coast.eagles', 'home.ground'] <- 'Perth Stadium'
  }
  if (
    (team.data['fremantle', 'home.ground'] != 'Perth Stadium') &
    (season >= 2018)
  ) {
    team.data['fremantle', 'home.ground'] <- 'Perth Stadium'
  }
  
  result <-
    list(
      team.data = team.data,
      rating.time.series = rating.time.series
    )
  
}



DoGameElo <- function(game.info, 
                      team.data, 
                      ground.location, 
                      ground.data, 
                      travel,distance,
                      param.spread,
                      param.margin,
                      param.coeff.rating.update,
                      param.coeff.ground.update,
                      param.coeff.travel, 
                      param.power.travel) {
  
  # Extract home and away teams, their scores and the ground
  team.home <- game.info$team.home
  team.away <- game.info$team.away
  score.points.home <- game.info$score.points.home
  score.points.away <- game.info$score.points.away
  ground <- game.info$ground
  
  # Extract team ratings
  rating.home <- team.data[team.home, 'rating']
  rating.away <- team.data[team.away, 'rating']
  
  # Determine ground adjustments
  rating.ground.adj.home <- CalculateGroundAdj(team.home, ground, ground.data)
  rating.ground.adj.away <- CalculateGroundAdj(team.away, ground, ground.data)
  
  # Determine travel adjustments
  rating.travel.adj.home <- CalculateTravelAdj(team.home, ground, team.data, ground.location, travel.distance, param.coeff.travel, param.power.travel)
  rating.travel.adj.away <- CalculateTravelAdj(team.away, ground, team.data, ground.location, travel.distance, param.coeff.travel, param.power.travel)
  
  # Determine adjusted ratings = rating + ground adj + travel adj
  rating.adj.home <- rating.home + rating.ground.adj.home + rating.travel.adj.home
  rating.adj.away <- rating.away + rating.ground.adj.away + rating.travel.adj.away
  
  # Calculate the difference in adj ratings and the expected result and margin
  delta.rating.home <- rating.adj.home - rating.adj.away
  delta.rating.away <- rating.adj.away - rating.adj.home
  result.exp.home <- CalculateResultExp(delta.rating.home, param.spread)
  result.exp.away <- 1 - result.exp.home
  margin.exp.home <- CalculateMarginExp(delta.rating.home, param.spread, param.margin)
  margin.exp.away <- -margin.exp.home
  
  # Determine the actual margin and result
  margin.act.home <- score.points.home - score.points.away
  margin.act.away <- score.points.away - score.points.home
  result.act.home <- CalculateResultAct(margin.act.home, param.margin)
  result.act.away <- 1 - result.act.home
  
  # Calculate the new ratings based on the difference between expected and actual results
  new.rating.home <- CalculateRatingNew(rating.home, result.exp.home, result.act.home, param.coeff.rating.update)
  new.rating.away <- CalculateRatingNew(rating.away, result.exp.away, result.act.away, param.coeff.rating.update)
  
  # Calculate the new ground ratings based on the difference between expected and actual results
  new.rating.ground.adj.home <- CalculateRatingNew(rating.ground.adj.home, result.exp.home, result.act.home, param.coeff.ground.update)
  new.rating.ground.adj.away <- CalculateRatingNew(rating.ground.adj.away, result.exp.away, result.act.away, param.coeff.ground.update)
  
  elo.game <- 
    list(
      old.rating.home = rating.home,
      old.rating.away = rating.away,
      old.rating.ground.adj.home = rating.ground.adj.home,
      old.rating.ground.adj.away = rating.ground.adj.away,
      new.rating.home = new.rating.home, 
      new.rating.away = new.rating.away,
      new.rating.ground.adj.home = new.rating.ground.adj.home, 
      new.rating.ground.adj.away = new.rating.ground.adj.away
    )
  
}

UpdateEloRatings <- function(team.data, 
                             ground.data, 
                             rating.time.series,
                             game.info, 
                             elo.game) {
  
  team.home <- game.info$team.home
  team.away <- game.info$team.away
  ground <- game.info$ground
  season.round.current <- paste(game.info$season, game.info$round, sep = ' ')
  team.data[team.home, "rating"] <- elo.game$new.rating.home
  team.data[team.away, "rating"] <- elo.game$new.rating.away
  ground.data[ground, team.home] <- elo.game$new.rating.ground.adj.home
  ground.data[ground, team.away] <- elo.game$new.rating.ground.adj.away
  rating.time.series[season.round.current, team.home] <- elo.game$new.rating.home
  rating.time.series[season.round.current, team.away] <- elo.game$new.rating.away
  
  elo.data <- list(team.data = team.data, ground.data = ground.data, rating.time.series = rating.time.series)
  elo.data
}


RunElo <- function(all.games, 
                   team.dictionary,
                   team.data,
                   ground.location,
                   ground.data, 
                   travel.distance,
                   rating.time.series, 
                   all.games.elo,
                   elo.params,
                   param.rating.mean, 
                   param.spread,
                   param.margin,
                   param.coeff.rating.update,
                   param.regress.rating,
                   param.coeff.ground.update,
                   param.coeff.travel,
                   param.power.travel,
                   param.rating.expansion.init,
                   do.store.detail = FALSE) {
  
  # Initialise ratings
  yes.founding.team <- (team.data$season.start <= 1897)
  team.data$rating[yes.founding.team] <- param.rating.mean
  team.data$rating[!yes.founding.team] <- param.rating.expansion.init
  
  # Initialise error tracking variables
  margin.cumulative.abs.error <- 0
  result.cumulative.abs.error <- 0
  brier.cumulative.error <- 0
  log.score.cumulative.error <- 0
  margin.cumulative.sq.error <- 0
  
  # Initialise season tracking variable
  season.current <- 1897

  # Main iterative loop: For each game ...
  for (game.idx in 1:nrow(all.games)) {

    # Update season and round tracking variables
    season.of.prev.game <- season.current
    season.current <- all.games[game.idx, "season"]
    round.current <- all.games[game.idx, "round"]
    season.round.current <- paste(season.current, round.current, sep = ' ')

    # If this game marks the start of a new season ...
    if (season.of.prev.game != season.current) {
      
      setup <- 
        SetupSeason(
          team.data, 
          rating.time.series, 
          season.current, 
          elo.params
        )
      
      team.data <- setup$team.data
      rating.time.series <- setup$rating.time.series
      
      # TODO: Make this season a variable rather than being hardcoded
      # Start calibration/optimisation for 1994 season
      if (season.current == 1994) {
        margin.cumulative.abs.error <- 0
        result.cumulative.abs.error <- 0
        brier.cumulative.error <- 0
        log.score.cumulative.error <- 0
        margin.cumulative.sq.error <- 0
      }
    
    }
    
    # Extract home and away teams, their scores and the ground
    team.home <- team.dictionary[[all.games[game.idx, 'team.home']]]
    team.away <- team.dictionary[[all.games[game.idx, 'team.away']]]
    score.points.home <- all.games[game.idx, 'score.points.home']
    score.points.away <- all.games[game.idx, 'score.points.away']
    ground <- all.games[game.idx, 'ground']

    # Extract team ratings
    rating.home <- team.data[team.home, 'rating']
    rating.away <- team.data[team.away, 'rating']

    # Determine ground adjustments
    rating.ground.adj.home <- CalculateGroundAdj(team.home, ground, ground.data)
    rating.ground.adj.away <- CalculateGroundAdj(team.away, ground, ground.data)

    # Determine travel adjustments
    rating.travel.adj.home <- CalculateTravelAdj(team.home, ground, team.data, ground.location, travel.distance, param.coeff.travel, param.power.travel)
    rating.travel.adj.away <- CalculateTravelAdj(team.away, ground, team.data, ground.location, travel.distance, param.coeff.travel, param.power.travel)

    # Determine adjusted ratings = rating + ground adj + travel adj
    rating.adj.home <- rating.home + rating.ground.adj.home + rating.travel.adj.home
    rating.adj.away <- rating.away + rating.ground.adj.away + rating.travel.adj.away

    # Calculate the difference in adj ratings and the expected result and margin
    delta.rating.home <- rating.adj.home - rating.adj.away
    delta.rating.away <- rating.adj.away - rating.adj.home
    result.exp.home <- CalculateResultExp(delta.rating.home, param.spread)
    result.exp.away <- 1 - result.exp.home
    margin.exp.home <- CalculateMarginExp(delta.rating.home, param.spread, param.margin)
    margin.exp.away <- -margin.exp.home
    
    # Determine the actual margin and result
    margin.act.home <- score.points.home - score.points.away
    margin.act.away <- score.points.away - score.points.home
    result.act.home <- CalculateResultAct(margin.act.home, param.margin)
    result.act.away <- 1 - result.act.home

    # Calculate the new ratings based on the difference between expected and actual results
    new.rating.home <- CalculateRatingNew(rating.home, result.exp.home, result.act.home, param.coeff.rating.update)
    new.rating.away <- CalculateRatingNew(rating.away, result.exp.away, result.act.away, param.coeff.rating.update)

    # Calculate the new ground ratings based on the difference between expected and actual results
    new.rating.ground.adj.home <- CalculateRatingNew(rating.ground.adj.home, result.exp.home, result.act.home, param.coeff.ground.update)
    new.rating.ground.adj.away <- CalculateRatingNew(rating.ground.adj.away, result.exp.away, result.act.away, param.coeff.ground.update)

    # Store the new ratings
    team.data[team.home, 'rating'] <- new.rating.home
    team.data[team.away, 'rating'] <- new.rating.away
    ground.data[ground, team.home] <- new.rating.ground.adj.home
    ground.data[ground, team.away] <- new.rating.ground.adj.away
    rating.time.series[season.round.current, team.home] <- new.rating.home
    rating.time.series[season.round.current, team.away] <- new.rating.away

    if (score.points.home > score.points.away) {
      outcome.home <- 1
    } else if (score.points.home < score.points.away) {
      outcome.home <- 0
    } else {
      outcome.home <- 0.5
    }
    outcome.away <- 1 - outcome.home

    brier.game <- (result.exp.home - outcome.home) ^ 2
    log.score.game <- (outcome.home * log(result.exp.home)) + ((1 - outcome.home) * log(1 - result.exp.home))

    # Option to store the details of Elo variables for this game for post-analysis
    if (do.store.detail) {
      all.games.elo[game.idx, 1:3] <- c(team.home, team.away, ground)  # Cols 1:3 separate because class = character not numeric
      all.games.elo[game.idx, 4:ncol(all.games.elo)] <- c(rating.home, rating.ground.adj.home, rating.travel.adj.home, rating.adj.home,
                                                          rating.away, rating.ground.adj.away, rating.travel.adj.away, rating.adj.away,
                                                          delta.rating.home, delta.rating.away,
                                                          result.exp.home, result.exp.away,
                                                          margin.exp.home, margin.exp.away,
                                                          outcome.home, outcome.away,
                                                          brier.game, log.score.game,
                                                          result.act.home, result.act.away, 
                                                          margin.act.home, margin.act.away,
                                                          result.act.home - result.exp.home, result.act.away - result.exp.away, 
                                                          margin.act.home - margin.exp.home, margin.act.away - margin.exp.away,
                                                          new.rating.home - rating.home, new.rating.away - rating.away,
                                                          new.rating.home, new.rating.away)
    }

    # Update the cumulative error tracking variables
    margin.cumulative.abs.error <- margin.cumulative.abs.error + abs(margin.act.home - margin.exp.home)
    result.cumulative.abs.error <- result.cumulative.abs.error + abs(result.exp.home - result.act.home)
    brier.cumulative.error <- brier.cumulative.error + brier.game
    log.score.cumulative.error <- log.score.cumulative.error + log.score.game
    margin.cumulative.sq.error <- margin.cumulative.sq.error + (margin.act.home - margin.exp.home) ^ 2
  
  }

  elo.result <- 
    list(
      team.data, 
      rating.time.series,
      ground.data, 
      all.games.elo,
      margin.cumulative.abs.error,
      result.cumulative.abs.error,
      brier.cumulative.error,
      log.score.cumulative.error,
      margin.cumulative.sq.error
    )
  
}
