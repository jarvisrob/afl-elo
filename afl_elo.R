
CalculateGroundAdj <- function(team, ground, season.current, ground.panel.record,
                               param.time.window, param.coeff.wins, param.coeff.other) {
  #print(head(ground.panel.record))
  #print(season.current)
  #print(param.time.window)
  idx <- (ground.panel.record$team == team) &
         (ground.panel.record$ground == ground) &
         (ground.panel.record$season >= season.current - param.time.window) &
         (ground.panel.record$season <= season.current)
  #print(ground.panel.record[idx, ])
  n.wins <- sum(ground.panel.record[idx, "wins"])
  #print(n.wins)
  n.other <- sum(ground.panel.record[idx, "losses"],
                 ground.panel.record[idx, "draws"])
  #print(n.other)
  
  ground.adj <- (param.coeff.wins * n.wins) + (param.coeff.other * n.other)
  #print(ground.adj)

  ground.adj
}

CalculateTravelAdj <- function(team, ground, team.data, ground.location, travel.distance, param.coeff.travel) {
  team.location <- team.data[team, 'location']
  #ground.location <- ground.data[ground, 'location']
  distance <- travel.distance[ground.location[ground, ], team.location]
  
  travel.adj <- -param.coeff.travel * log10(distance + 1)
  
  travel.adj
}



CalculateResultExp <- function(delta.ratings, param.spread) {
  result.exp <- 1 / (10^(-delta.ratings/400) + 1)
}


CalculateMarginExp <- function(delta.ratings, param.spread, param.margin) {
  margin.exp <- delta.ratings / (param.margin * param.spread)
}


CalculateResultAct <- function(margin.act, param.margin) {
  result.act <- 1 / (10^(-param.margin * margin.act) + 1)
}


CalculateRatingNew <- function(rating, result.exp, result.act, param.coeff.update) {
  rating.new <- rating + param.coeff.update * (result.act - result.exp)
}


RegressRating <- function(rating, rating.mean, param.regress) {
  rating.new <- ((1 - param.regress) * rating) + (param.regress * rating.mean)
}


RunElo <- function(all.games, team.dictionary, team.data, ground.location, ground.panel.record, travel.distance, rating.time.series, all.games.elo, rating.mean,
                   param.spread, param.margin, 
                   param.coeff.rating.update, param.regress.rating, 
                   param.time.window, param.coeff.wins, param.coeff.other,
                   param.coeff.travel,
                   param.rating.expansion.init) {
  
  #print(head(ground.panel.record))
  
  # Initialise ratings
  is.founding.team <- (team.data$season.start <= 1897)
  team.data$rating[is.founding.team] <- rating.mean
  team.data$rating[!is.founding.team] <- param.rating.expansion.init
  #print(team.data$rating)
  
  # Initialise cumulative sum abs error on margin
  margin.cumulative.abs.error <- 0
  
  # Initialise cumulative sum squared error on result, for Brier score calc
  result.cumulative.sq.error <- 0
  
  # Initialise season and round tracking variables
  season.current <- 1897
  round.current <- 'xx'  # Dummy value to begin, want to trigger logging to values into rating.time.series
  season.round.current <- paste(season.current, round.current, sep = ' ')
  
  for (game.idx in 1:nrow(all.games)) {
    
    if (all.games[game.idx, 'round'] != round.current) {
      round.current <- all.games[game.idx, 'round']
      #print(round.current)
      #print(team.data$rating)
      
      if (round.current == 'R1') {
        season.current <- as.integer(format(all.games[game.idx, 'date'], '%Y'))
        #print(season.current)
        #print(game.idx)
        is.regress <- (team.data$season.start < season.current) & (team.data$season.end >= season.current)
        #if (season.current == 1926) {
          #print(is.regress)
          #print(team.data$rating)
        #}
        #print(is.regress)
        if (sum(is.regress) > 0) {
          team.data$rating[is.regress] <- RegressRating(team.data$rating[is.regress], rating.mean, param.regress.rating)
          # regress ground rating for each active team
          #ground.data.col.idx <- c(F, !logical(ncol(ground.data)-1)) & c(F, is.regress)  # Making first col index FALSE to avoid grouond locations col (1st col)
          #ground.data[, ground.data.col.idx] <- RegressRating(ground.data[, ground.data.col.idx], 0, param.regress.ground)
        }
        
        
        is.active <- (team.data$season.start <= season.current) & (team.data$season.end >= season.current)
        # ***** Need to work out how to log all active teams at YYYY start ******
        rating.time.series[paste(season.current, 'start', sep = ' '), is.active] <- team.data$rating[is.active]

        # Move the Swans to Sydney from 1982 onwards
        if ((season.current >= 1982) & (team.data['sydney', 'location'] != 'Sydney')) {
          team.data['sydney', 'location'] <- 'Sydney'
        }
        
        # Start calibration/optimisation on the AFL era
        if (season.current == 1990) {
          margin.cumulative.abs.error <- 0
          result.cumulative.sq.error <- 0
        }
      }
      
      season.round.current <- paste(season.current, round.current, sep = ' ')
    }
    
    
    team.home <- team.dictionary[[all.games[game.idx, 'team.home']]]
    team.away <- team.dictionary[[all.games[game.idx, 'team.away']]]
    score.points.home <- all.games[game.idx, 'score.points.home']
    score.points.away <- all.games[game.idx, 'score.points.away']
    ground <- all.games[game.idx, 'ground']
    
    rating.home <- team.data[team.home, 'rating']
    rating.away <- team.data[team.away, 'rating']
      
    rating.ground.adj.home <- CalculateGroundAdj(team.home, ground, season.current, ground.panel.record,
                                                   param.time.window, param.coeff.wins, param.coeff.other)
    rating.ground.adj.away <- CalculateGroundAdj(team.away, ground, season.current, ground.panel.record,
                                                   param.time.window, param.coeff.wins, param.coeff.other)
    
    rating.travel.adj.home <- CalculateTravelAdj(team.home, ground, team.data, ground.location, travel.distance, param.coeff.travel)
    rating.travel.adj.away <- CalculateTravelAdj(team.away, ground, team.data, ground.location, travel.distance, param.coeff.travel)
    
    rating.home.adj <- rating.home + rating.ground.adj.home + rating.travel.adj.home
    rating.away.adj <- rating.away + rating.ground.adj.away + rating.travel.adj.away
    
    delta.ratings <- rating.home.adj - rating.away.adj
    result.exp.home <- CalculateResultExp(delta.ratings, param.spread)
    result.exp.away <- 1 - result.exp.home
    margin.exp.home <- CalculateMarginExp(delta.ratings, param.spread, param.margin)



    idx.gpr.team.home <- (ground.panel.record$team == team.home) &
                         (ground.panel.record$ground == ground) &
                         (ground.panel.record$season == season.current)
    idx.gpr.team.away <- (ground.panel.record$team == team.away) &
                         (ground.panel.record$ground == ground) &
                         (ground.panel.record$season == season.current)
    ground.panel.record[idx.gpr.team.home, "played"] <- ground.panel.record[idx.gpr.team.home, "played"] + 1
    ground.panel.record[idx.gpr.team.away, "played"] <- ground.panel.record[idx.gpr.team.away, "played"] + 1
    if (score.points.home > score.points.away) {
      ground.panel.record[idx.gpr.team.home, "wins"] <- ground.panel.record[idx.gpr.team.home, "wins"] + 1
      ground.panel.record[idx.gpr.team.away, "losses"] <- ground.panel.record[idx.gpr.team.away, "losses"] + 1
    } else if (score.points.home < score.points.away) {
      ground.panel.record[idx.gpr.team.home, "losses"] <- ground.panel.record[idx.gpr.team.home, "losses"] + 1
      ground.panel.record[idx.gpr.team.away, "wins"] <- ground.panel.record[idx.gpr.team.away, "wins"] + 1
    } else {
      ground.panel.record[idx.gpr.team.home, "draws"] <- ground.panel.record[idx.gpr.team.home, "draws"] + 1
      ground.panel.record[idx.gpr.team.away, "draws"] <- ground.panel.record[idx.gpr.team.away, "draws"] + 1
    }
    
    
    
    margin.act.home <- score.points.home - score.points.away
    result.act.home <- CalculateResultAct(margin.act.home, param.margin)
    result.act.away <- 1 - result.act.home
    
    rating.home.new <- CalculateRatingNew(rating.home, result.exp.home, result.act.home, param.coeff.rating.update)
    rating.away.new <- CalculateRatingNew(rating.away, result.exp.away, result.act.away, param.coeff.rating.update)
    
    team.data[team.home, 'rating'] <- rating.home.new
    team.data[team.away, 'rating'] <- rating.away.new
    
    rating.time.series[season.round.current, team.home] <- rating.home.new
    rating.time.series[season.round.current, team.away] <- rating.away.new
    
    #rating.ground.adj.home.new <- CalculateRatingNew(rating.ground.adj.home, result.exp.home, result.act.home, param.coeff.ground.update)
    #rating.ground.adj.away.new <- CalculateRatingNew(rating.ground.adj.away, result.exp.away, result.act.away, param.coeff.ground.update)
    
    #ground.data[ground, team.home] <- rating.ground.adj.home.new
    #ground.data[ground, team.away] <- rating.ground.adj.away.new
    
    # Add all Elo data for this game to the all.games.elo data frame for later game-by-game analysis
    # all.games.elo[game.idx, "team.home"]
    # all.games.elo[game.idx, "team.away"]
    # 
    # all.games.elo[game.idx, "rating.home.before"]
    # all.games.elo[game.idx, "rating.ground.adj.home.before"]
    # all.games.elo[game.idx, "rating.travel.adj.home.before"]
    # all.games.elo[game.idx, "rating.home.adj.before"]
    # 
    # all.games.elo[game.idx, "rating.away.before"]
    # all.games.elo[game.idx, "rating.ground.adj.away.before"]
    # all.games.elo[game.idx, "rating.travel.adj.away.before"]
    # all.games.elo[game.idx, "rating.away.adj.before"]


     all.games.elo[game.idx, 1:2] <- c(team.home, team.away)
     all.games.elo[game.idx, 3:ncol(all.games.elo)] <- c(rating.home, rating.ground.adj.home, rating.travel.adj.home, rating.home.adj,
                                                         rating.away, rating.ground.adj.away, rating.travel.adj.away, rating.away.adj,
                                                         delta.ratings,
                                                         result.exp.home, result.exp.away, margin.exp.home,
                                                         result.act.home, result.act.away, margin.act.home,
                                                         result.act.home - result.exp.home, result.act.away - result.exp.away, margin.act.home - margin.exp.home,
                                                         rating.home.new - rating.home, rating.home.new, rating.away.new)
    
    margin.cumulative.abs.error <- margin.cumulative.abs.error + abs(margin.act.home - margin.exp.home)
    result.cumulative.sq.error <- result.cumulative.sq.error + (result.exp.home - result.act.home)^2
  }
  
  team.data$is.active <- is.active
  
  elo.result <- list(team.data, rating.time.series, ground.panel.record, all.games.elo, 
                     margin.cumulative.abs.error, result.cumulative.sq.error)
  elo.result
}



