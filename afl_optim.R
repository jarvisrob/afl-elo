# Optimisation

OptimWrapperRunElo <- function(tunable.params, fixed.params, data.inputs) {
  
  # Tunable parameters
  param.margin <- tunable.params[1]
  
  param.coeff.rating.update <- tunable.params[2]
  param.regress.rating <- tunable.params[3]
  
  param.coeff.ground.update <- tunable.params[4]
  param.regress.ground <- tunable.params[5]
  
  param.coeff.travel <- tunable.params[6]
  param.rating.expansion.init <- tunable.params[7]
  
  # Fixed parameters
  rating.mean <- fixed.params[1]
  param.spread <- fixed.params[2]
  
  # Data inputs
  all.games <- data.inputs[[1]]
  team.dictionary <- data.inputs[[2]]
  team.data <- data.inputs[[3]]
  ground.data <- data.inputs[[4]]
  travel.distance <- data.inputs[[5]]
  rating.time.series <- data.inputs[[6]]
  all.games.elo <- data.inputs[[7]]
  
  elo.result <- RunElo(all.games, team.dictionary, team.data, ground.data, travel.distance, rating.time.series, all.games.elo,
                       rating.mean, 
                       param.spread, param.margin, 
                       param.coeff.rating.update, param.regress.rating, 
                       param.coeff.ground.update, param.regress.ground, 
                       param.coeff.travel, param.rating.expansion.init)
  #team.data.run <- elo.result[[1]]
  #rating.time.series.run <- elo.result[[2]]
  #ground.data.run <- elo.result[[3]]
  # all.games.elo.run <- elo.result[[4]]
  margin.sum.abs.error <- elo.result[[5]]
  result.sum.sq.error <- elo.result[[6]]
  
  # margin.mae <- margin.sum.abs.error / nrow(all.games)
  # print(margin.mae)
  # margin.mae
  
  print(result.sum.sq.error)
  result.sum.sq.error
  
  # brier.score <- result.sum.sq.error / nrow(all.games)
  # print(brier.score)
  # brier.score
}


# Initial values
tunable.params.init <- c(0.01129223, 66.70423, 0.1865318, 25.58827, 0.1051385, 13.60048, 1300)
tunable.params.lower <- c(0.0001, 0.1, 0, 0.1, 0, 0, 800)
tunable.params.upper <- c(1, 250, 1, 250, 1, 250, 1500)
fixed <- c(1500, 400)

# Data inputs
df.inputs <- list(all.games, team.dictionary, team.data, ground.data, travel.distance, rating.time.series, all.games.elo)

# Optimisation call
tic()
optim.result <- optim(par = tunable.params.init, OptimWrapperRunElo,
                      fixed.params = fixed, data.inputs = df.inputs,
                      method = "L-BFGS-B", lower = tunable.params.lower, upper = tunable.params.upper)
toc()
