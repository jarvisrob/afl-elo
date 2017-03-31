PredictGame <- function(team.home, team.away, ground, team.data, ground.data, ground.location, travel.distance, commission = 0, param.spread = 400, param.margin = 0.0218, param.coeff.travel = 34.7, param.power.travel = 0.136) {

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

  # Betting odds
  back.bookie.home <- 1 / result.exp.home
  back.bookie.away <- 1 / result.exp.away
  back.exchange.home <- (1 / result.exp.home - 1) / (1 - commission) + 1
  back.exchange.away <- (1 / result.exp.away - 1) / (1 - commission) + 1
  lay.exchange.home <- (1 / result.exp.home - 1) * (1 - commission) + 1
  lay.exchange.away <- (1 / result.exp.away - 1) * (1 - commission) + 1

  # Print details
  print(paste0("HOME | Rating = ", as.character(round(rating.home, 1)),
               " | Ground adj = ", as.character(round(rating.ground.adj.home, 1)),
               " | Travel adj = ", as.character(round(rating.travel.adj.home, 1)),
               " | Adj rating = ", as.character(round(rating.home.adj, 1))))
  print(paste0("     | p = ", as.character(round(result.exp.home * 100, 1)), " per cent"))
  print(paste0("     | Bookie:   Back > ", round(back.bookie.home, 2)))
  print(paste0("     | Exchange: Back > ", round(back.exchange.home, 2),
               " and/or Lay < ", round(lay.exchange.home, 2)))
  print(paste0("AWAY | Rating = ", as.character(round(rating.away, 1)),
               " | Ground adj = ", as.character(round(rating.ground.adj.away, 1)),
               " | Travel adj = ", as.character(round(rating.travel.adj.away, 1)),
               " | Adj rating = ", as.character(round(rating.away.adj, 1))))
  print(paste0("     | p = ", as.character(round(result.exp.away * 100, 1)), " per cent"))
  print(paste0("     | Bookie:   Back > ", round(back.bookie.away, 2)))
  print(paste0("     | Exchange: Back > ", round(back.exchange.away, 2),
               " and/or Lay < ", round(lay.exchange.away, 2)))
    
  # Return prediction
  prediction <- list(result.exp.home = result.exp.home, margin.exp.home = margin.exp.home)
  prediction

}