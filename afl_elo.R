
CalculateGroundAdj <- function(team, ground, ground.data) {
  ground.adj <- ground.data[ground, team]
  ground.adj
}

CalculateTravelAdj <- function(team, ground, team.data, ground.location, travel.distance, param.coeff.travel, param.power.travel) {
  team.location <- team.data[team, 'location']
  distance <- travel.distance[ground.location[ground, ], team.location]
  
  travel.adj <- -param.coeff.travel * (distance ^ param.power.travel)
  
  travel.adj
}


CalculateResultExp <- function(delta.ratings, param.spread) {
  result.exp <- 1 / (10 ^ (-delta.ratings / param.spread) + 1)
}


CalculateMarginExp <- function(delta.ratings, param.spread, param.margin) {
  margin.exp <- delta.ratings / (param.margin * param.spread)
}


CalculateResultAct <- function(margin.act, param.margin) {
  result.act <- 1 / (10 ^ (-param.margin * margin.act) + 1)
}


CalculateRatingNew <- function(rating, result.exp, result.act, param.coeff.update) {
  rating.new <- rating + param.coeff.update * (result.act - result.exp)
}


RegressRating <- function(rating, param.rating.mean, param.regress) {
  rating.new <- ((1 - param.regress) * rating) + (param.regress * param.rating.mean)
}


RunElo <- function(all.games, team.dictionary, team.data,
                   ground.location, ground.data, travel.distance,
                   rating.time.series, all.games.elo,
                   param.rating.mean, param.spread,
                   param.margin,
                   param.coeff.rating.update, param.regress.rating,
                   param.coeff.ground.update,
                   param.coeff.travel, param.power.travel,
                   param.rating.expansion.init,
                   do.store.detail = FALSE) {
  
  # Initialise ratings
  yes.founding.team <- (team.data$season.start <= 1897)
  team.data$rating[yes.founding.team] <- param.rating.mean
  team.data$rating[!yes.founding.team] <- param.rating.expansion.init
  
  # Initialise error tracking variables
  margin.cumulative.abs.error <- 0
  result.cumulative.sq.error <- 0
  brier.cumulative.error <- 0
  log.score.cumulative.error <- 0
  
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
      
      # Ratings regress to mean for teams that played the previous season
      yes.regress <- (team.data$season.start < season.current) & (team.data$season.end >= season.current)
      if (any(yes.regress)) {
        team.data$rating[yes.regress] <- RegressRating(team.data$rating[yes.regress], param.rating.mean, param.regress.rating)
        #ground.data[, c(F, yes.regress)] <- RegressRating(ground.data[, c(F, yes.regress)], 0, param.regress.ground)
      }
      
      # Determine active teams and record their rating at season start (after regression)
      yes.active <- (team.data$season.start <= season.current) & (team.data$season.end >= season.current)
      rating.time.series[paste(season.current, 'start', sep = ' '), yes.active] <- team.data$rating[yes.active]
    
      # Move the Swans to Sydney in 1982
      if ((team.data['sydney', 'location'] != 'Sydney') & (season.current >= 1982)) {
        team.data['sydney', 'location'] <- 'Sydney'
      }

      # Start calibration/optimisation for 1994 season
      if (season.current == 2000) {
        margin.cumulative.abs.error <- 0
        result.cumulative.sq.error <- 0
        brier.cumulative.error <- 0
        log.score.cumulative.error <- 0
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
    rating.home.adj <- rating.home + rating.ground.adj.home + rating.travel.adj.home
    rating.away.adj <- rating.away + rating.ground.adj.away + rating.travel.adj.away

    # Calculate the difference in adj ratings and the expected result and margin
    delta.ratings <- rating.home.adj - rating.away.adj
    result.exp.home <- CalculateResultExp(delta.ratings, param.spread)
    result.exp.away <- 1 - result.exp.home
    margin.exp.home <- CalculateMarginExp(delta.ratings, param.spread, param.margin)
    
    # Determine the actual margin and result
    margin.act.home <- score.points.home - score.points.away
    result.act.home <- CalculateResultAct(margin.act.home, param.margin)
    result.act.away <- 1 - result.act.home

    # Calculate the new ratings based on the difference between expected and actual results
    rating.home.new <- CalculateRatingNew(rating.home, result.exp.home, result.act.home, param.coeff.rating.update)
    rating.away.new <- CalculateRatingNew(rating.away, result.exp.away, result.act.away, param.coeff.rating.update)

    # Calculate the new ground ratings based on the difference between expected and actual results
    rating.ground.adj.home.new <- CalculateRatingNew(rating.ground.adj.home, result.exp.home, result.act.home, param.coeff.ground.update)
    rating.ground.adj.away.new <- CalculateRatingNew(rating.ground.adj.away, result.exp.away, result.act.away, param.coeff.ground.update)

    # Store the new ratings
    team.data[team.home, 'rating'] <- rating.home.new
    team.data[team.away, 'rating'] <- rating.away.new
    ground.data[ground, team.home] <- rating.ground.adj.home.new
    ground.data[ground, team.away] <- rating.ground.adj.away.new
    rating.time.series[season.round.current, team.home] <- rating.home.new
    rating.time.series[season.round.current, team.away] <- rating.away.new

    # Determine outcome (win/loss/draw) for the home team, and also update the
    # ground panel records with the win/loss/draw outcome
    #idx.gpr.team.home <- (ground.panel.record$team == team.home) &
                         #(ground.panel.record$ground == ground) &
                         #(ground.panel.record$season == season.current)
    #idx.gpr.team.away <- (ground.panel.record$team == team.away) &
                         #(ground.panel.record$ground == ground) &
                         #(ground.panel.record$season == season.current)
    #ground.panel.record[idx.gpr.team.home, "played"] <- ground.panel.record[idx.gpr.team.home, "played"] + 1
    #ground.panel.record[idx.gpr.team.away, "played"] <- ground.panel.record[idx.gpr.team.away, "played"] + 1
    if (score.points.home > score.points.away) {
      outcome.home <- 1
      #ground.panel.record[idx.gpr.team.home, "wins"] <- ground.panel.record[idx.gpr.team.home, "wins"] + 1
      #ground.panel.record[idx.gpr.team.away, "losses"] <- ground.panel.record[idx.gpr.team.away, "losses"] + 1
    } else if (score.points.home < score.points.away) {
      outcome.home <- 0
      #ground.panel.record[idx.gpr.team.home, "losses"] <- ground.panel.record[idx.gpr.team.home, "losses"] + 1
      #ground.panel.record[idx.gpr.team.away, "wins"] <- ground.panel.record[idx.gpr.team.away, "wins"] + 1
    } else {
      outcome.home <- 0.5
      #ground.panel.record[idx.gpr.team.home, "draws"] <- ground.panel.record[idx.gpr.team.home, "draws"] + 1
      #ground.panel.record[idx.gpr.team.away, "draws"] <- ground.panel.record[idx.gpr.team.away, "draws"] + 1
    }

    brier.game <- (result.exp.home - outcome.home) ^ 2
    log.score.game <- (outcome.home * log(result.exp.home)) + ((1 - outcome.home) * log(1 - result.exp.home))

    # Option to store the details of Elo variables for this game for post-analysis
    if (do.store.detail) {
      all.games.elo[game.idx, 1:2] <- c(team.home, team.away)
      all.games.elo[game.idx, 3:ncol(all.games.elo)] <- c(rating.home, rating.ground.adj.home, rating.travel.adj.home, rating.home.adj,
                                                          rating.away, rating.ground.adj.away, rating.travel.adj.away, rating.away.adj,
                                                          delta.ratings,
                                                          result.exp.home, result.exp.away, margin.exp.home,
                                                          outcome.home, brier.game, log.score.game,
                                                          result.act.home, result.act.away, margin.act.home,
                                                          result.act.home - result.exp.home, result.act.away - result.exp.away, margin.act.home - margin.exp.home,
                                                          rating.home.new - rating.home, rating.home.new, rating.away.new)
    }

    # Update the cumulative error tracking variables
    margin.cumulative.abs.error <- margin.cumulative.abs.error + abs(margin.act.home - margin.exp.home)
    result.cumulative.sq.error <- result.cumulative.sq.error + (result.exp.home - result.act.home) ^ 2
    brier.cumulative.error <- brier.cumulative.error + brier.game
    log.score.cumulative.error <- log.score.cumulative.error + log.score.game
  
  }

  # Record the currently active teams at the conlcusion of all games iterated
  team.data$yes.active <- yes.active

  # Collate and return the results
  elo.result <- list(team.data, rating.time.series,
                     ground.data, all.games.elo,
                     margin.cumulative.abs.error, result.cumulative.sq.error,
                     brier.cumulative.error, log.score.cumulative.error)
  elo.result
}
