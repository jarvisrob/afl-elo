# Optimisation

OptimWrapperRunElo <- function(tunable.params, fixed.params, data.inputs) {

  print("Tunable params:")
  print(tunable.params)

  # Tunable parameters
  param.margin <- tunable.params[1]
  
  param.coeff.rating.update <- tunable.params[2]
  param.regress.rating <- tunable.params[3]
  
  param.coeff.ground.update <- tunable.params[4]

  param.coeff.travel <- tunable.params[5]
  param.power.travel <- tunable.params[6]

  param.rating.expansion.init <- tunable.params[7]

  
  # Fixed parameters
  param.rating.mean <- fixed.params[1]
  param.spread <- fixed.params[2]
  
  # Data inputs
  all.games <- data.inputs[[1]]
  team.dictionary <- data.inputs[[2]]
  team.data <- data.inputs[[3]]

  ground.location <- data.inputs[[4]]
  ground.data <- data.inputs[[5]]
  travel.distance <- data.inputs[[6]]

  rating.time.series <- data.inputs[[7]]
  all.games.elo <- data.inputs[[8]]
  
  elo.result <- RunElo(all.games, team.dictionary, team.data,
                       ground.location, ground.data, travel.distance,
                       rating.time.series, all.games.elo,
                       param.rating.mean, param.spread,
                       param.margin,
                       param.coeff.rating.update, param.regress.rating,
                       param.coeff.ground.update,
                       param.coeff.travel, param.power.travel,
                       param.rating.expansion.init,
                       do.store.detail = FALSE)
  
  #team.data.run <- elo.result[[1]]
  #rating.time.series.run <- elo.result[[2]]

  #ground.panel.record.run <- elo.result[[3]]
  #all.games.elo.run <- elo.result[[4]]

  margin.sum.abs.error <- elo.result[[5]]
  #result.sum.sq.error <- elo.result[[6]]

  brier.cumulative.error <- elo.result[[7]]
  #log.score.cumulative.error <- elo.result[[8]]
  
  margin.mae <- margin.sum.abs.error / sum(all.games$season >= 2000)
  print(paste0("Margin MAE = ", as.character(margin.mae)))
  #margin.mae
  
  #print(result.sum.sq.error)
  #result.sum.sq.error

  brier.score <- brier.cumulative.error / sum(all.games$season >= 2000)
  print(paste0("Brier score = ", as.character(brier.score)))
  #brier.cumulative.error

  #calib <- CheckCalibration(all.games.elo.run[all.games$season >= 1994, ], 0.02)
  ##print(calib)
  #ss.calib.error <- sum(calib$calib.error ^ 2, na.rm = TRUE)
  #print(paste0("SS calib error = ", as.character(ss.calib.error)))

  #print(log.score.cumulative.error)
  #log.score.cumulative.error

  print("---")
  output <- list(brier.score = brier.score, margin.mae = margin.mae)

}


# Initial values
tunable.params.type <- c("continuous", 
                         "continuous", "continuous", 
                         "continuous", 
                         "continuous", "continuous", 
                         "continuous")

#tunable.params.init <- c(0.01,
                         #50.0, 0.2,
                         #3, 5.0, 2.0,
                         #15.0,
                         #1300.0)
tunable.params.lower <- c(0.001,
                          50.0, 0.05,
                          0, 
                          20.0, 0.05,
                          750.0)
tunable.params.upper <- c(0.05,
                          100.0, 0.25,
                          5.0, 
                          50.0, 0.25,
                          1100.0)
fixed <- c(1500.0, 400.0)

f.lower <- c(0, 0)
f.upper <- c(Inf, 30.0)

fw.d <- c(0.00005, 
          0.05, 0.0002,
          0.005, 
          0.03, 0.0002,
          0.35)

# Data inputs
df.inputs <- list(all.games, team.dictionary, team.data,
                  ground.location, ground.data, travel.distance,
                  rating.time.series, all.games.elo)

# Optimisation call
source("../stochoptim/harmony_search.R")


tic()
harm.search.res <- HarmonySearch(OptimWrapperRunElo, fixed.params = fixed, data.inputs = df.inputs,
                                 x.type = tunable.params.type, 
                                 x.lower = tunable.params.lower, 
                                 x.upper = tunable.params.upper, 
                                 f.lower = f.lower,
                                 f.upper = f.upper,
                                 fw.d = fw.d,
                                 hms = 70, hmcr = 0.9, par = 0.3,
                                 itn.max = 2400, minimize = TRUE,
                                 hm.init = hm)

#optim.result <- optim(par = tunable.params.init, OptimWrapperRunElo,
                      #fixed.params = fixed, data.inputs = df.inputs,
                      #method = "L-BFGS-B",
                      #lower = tunable.params.lower, upper = tunable.params.upper)
toc()
