# Optimisation

OptimWrapperRunElo <- function(tunable.params, fixed.params, data.inputs) {
  
  # Tunable parameters
  param.margin <- tunable.params[1]
  
  param.coeff.rating.update <- tunable.params[2]
  param.regress.rating <- tunable.params[3]
  
  param.time.window <- tunable.params[4]
  param.coeff.wins <- tunable.params[5]
  param.coeff.other <- tunable.params[6]

  param.coeff.travel <- tunable.params[7]

  param.rating.expansion.init <- tunable.params[8]

  # Fixed parameters
  param.rating.mean <- fixed.params[1]
  param.spread <- fixed.params[2]
  
  # Data inputs
  all.games <- data.inputs[[1]]
  team.dictionary <- data.inputs[[2]]
  team.data <- data.inputs[[3]]

  ground.location <- data.inputs[[4]]
  ground.panel.record <- data.inputs[[5]]
  travel.distance <- data.inputs[[6]]

  rating.time.series <- data.inputs[[7]]
  all.games.elo <- data.inputs[[8]]
  
  elo.result <- RunElo(all.games, team.dictionary, team.data,
                       ground.location, ground.panel.record, travel.distance,
                       rating.time.series, all.games.elo,
                       param.rating.mean, param.spread,
                       param.margin,
                       param.coeff.rating.update, param.regress.rating,
                       param.time.window, param.coeff.wins, param.coeff.other,
                       param.coeff.travel,
                       param.rating.expansion.init,
                       do.store.detail = FALSE)
  
  #team.data.run <- elo.result[[1]]
  #rating.time.series.run <- elo.result[[2]]

  #ground.panel.record.run <- elo.result[[3]]
  #all.games.elo.run <- elo.result[[4]]

  #margin.sum.abs.error <- elo.result[[5]]
  result.sum.sq.error <- elo.result[[6]]
  
  #margin.mae <- margin.sum.abs.error / nrow(all.games)
  #print(margin.mae)
  #margin.mae
  
  print(result.sum.sq.error)
  result.sum.sq.error
  
  #brier.score <- result.sum.sq.error / nrow(all.games)
  #print(brier.score)
  #brier.score
}


# Initial values
tunable.params.init <- c(0.01,
                         50.0, 0.2,
                         3, 5.0, 2.0,
                         15.0,
                         1300.0)
tunable.params.lower <- c(0.000001,
                          0.1, 0,
                          0, 0.001, 0.001,
                          0.001,
                          800.0)
tunable.params.upper <- c(1,
                          250.0, 1,
                          30, 250.0, 250.0,
                          250.0,
                          1500.0)
fixed <- c(1500.0, 400.0)

# Data inputs
df.inputs <- list(all.games, team.dictionary, team.data,
                  ground.location, ground.panel.record, travel.distance,
                  rating.time.series, all.games.elo)

# Optimisation call
tic()
optim.result <- optim(par = tunable.params.init, OptimWrapperRunElo,
                      fixed.params = fixed, data.inputs = df.inputs,
                      method = "L-BFGS-B",
                      lower = tunable.params.lower, upper = tunable.params.upper)
toc()
