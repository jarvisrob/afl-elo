OptimWrapperRunElo <- function(tunable.params, fixed.params, data.inputs) {
  
  # Season start tuning from
  season.start.tuning <- 2000

  print("Tunable params:")
  tunable.params.with.names <- tunable.params
  names(tunable.params.with.names) <-
    c(
      "margin (sd)",
      "coeff.rating.update",
      "regress.rating",
      "coeff.ground.update",
      "coeff.travel",
      "power.travel",
      "rating.expn.init"
    )
  print(tunable.params.with.names)

  # Tunable parameters
  param.margin <- tunable.params[1]
  param.coeff.rating.update <- tunable.params[2]
  param.regress.rating <- tunable.params[3]
  param.coeff.ground.update <- tunable.params[4]
  # param.coeff.ground.update <- 0
  param.coeff.travel <- tunable.params[5]
  param.power.travel <- tunable.params[6]
  param.rating.expansion.init <- tunable.params[7]
  
  # Fixed parameters
  param.rating.mean <- 0
  param.spread <- 0  # No longer used
  # param.power.travel <- 0
  # param.rating.expansion.init <- fixed.params[1]
  
  # Data inputs
  all.games <- data.inputs[[1]]
  team.dictionary <- data.inputs[[2]]
  team.data <- data.inputs[[3]]
  ground.location <- data.inputs[[4]]
  ground.data <- data.inputs[[5]]
  travel.distance <- data.inputs[[6]]
  rating.time.series <- data.inputs[[7]]
  all.games.elo <- data.inputs[[8]]
  
  elo.params <- list(
    rating.mean = param.rating.mean,
    spread = param.spread,
    margin = param.margin,
    coeff.rating.update = param.coeff.rating.update,
    regress.rating = param.regress.rating,
    coeff.ground.update = param.coeff.ground.update,
    coeff.travel = param.coeff.travel,
    power.travel = param.power.travel,
    rating.expansion.init = param.rating.expansion.init
  )

  elo.result <-
    RunElo(
      all.games, 
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
      do.store.detail = TRUE
    )
  
  #team.data.run <- elo.result[[1]]
  #rating.time.series.run <- elo.result[[2]]
  #ground.panel.record.run <- elo.result[[3]]
  all.games.elo.run <- elo.result[[4]]
  margin.sum.abs.error <- elo.result[[5]]
  result.sum.abs.error <- elo.result[[6]]
  brier.cumulative.error <- elo.result[[7]]
  log.score.cumulative.error <- elo.result[[8]]
  
  margin.mae <- margin.sum.abs.error / sum(all.games$season >= season.start.tuning)
  print(paste0("Margin MAE = ", as.character(margin.mae)))
  #margin.mae
  
  #print(result.sum.sq.error)
  result.mae <- result.sum.abs.error / sum(all.games$season >= season.start.tuning)
  # print(paste0("Result MAE = ", result.mae))

  brier.score <- brier.cumulative.error / sum(all.games$season >= season.start.tuning)
  print(paste0("Brier score = ", as.character(brier.score)))
  #brier.cumulative.error

  #print(log.score.cumulative.error)
  log.score <- log.score.cumulative.error / sum(all.games$season >= season.start.tuning)
  # print(paste0("Log score = ", as.character(log.score)))
  
  # Tuning the std dev in margin error to match the param.margin
  elo.1994.2016 <- all.games.elo.run %>% filter(all.games$season >= season.start.tuning)
  elo.1994.2016.rs <- SelectHomeOrAwayValueRandom(elo.1994.2016, c('margin.exp', 'margin.act', 'margin.error'))
  
  margin.error.distrib <- fitdistr(elo.1994.2016.rs$margin.error, 'normal')
  margin.error.mean <- margin.error.distrib$estimate[["mean"]]
  margin.error.sd <- margin.error.distrib$estimate[["sd"]]
  print(paste0("mean = ", margin.error.mean))
  print(paste0("std dev = ", margin.error.sd, " cf. param.margin = ", param.margin))

  print("---")
  
  result <- 
    list(
      brier.score = brier.score, 
      err_mean = abs(margin.error.mean - 0),
      err_sd = abs(margin.error.sd - param.margin),
      margin_mae = margin.mae
    )

}

# Initial values
tunable.params.type <- 
  c(
    "continuous",
    "continuous",
    "continuous",
    "continuous",
    "continuous",
    "continuous",
    "continuous"
  )

tunable.params.lower <- 
  c(
    36.8200,
    10.200,
    0.1400,
    0.2250,
    0.700,
    0.330,
    -80.0
  )

tunable.params.upper <- 
  c(
    36.8700,
    10.40,
    0.1700,
    0.2800,
    0.999,
    0.500,
    -78.0
  )

f.lower <- c(0, 0, 0, 0)
f.upper <- 
  c(
    1.0,  # Brier score
    0.5,  # Mean margin error
    0.5,  # Standard deviation error
    30.0  # Margin MAE
  )

fw.d <- (tunable.params.upper - tunable.params.lower) / 2000

# Data inputs
data.inputs <- 
  list(
    all.games, 
    team.dictionary, 
    team.data,
    ground.location, 
    ground.data,
    travel.distance,
    rating.time.series, 
    all.games.elo
  )

# Optimisation call
source("../stochoptim/harmony_search.R")
tic()
harm.search.res <- 
  HarmonySearch(
    OptimWrapperRunElo, 
    # fixed.params = fixed, 
    data.inputs = data.inputs,
    x.type = tunable.params.type,
    x.lower = tunable.params.lower,
    x.upper = tunable.params.upper,
    f.lower = f.lower,
    f.upper = f.upper,
    fw.d = fw.d,
    hms = 30, 
    hmcr = 0.9, 
    par = 0.3,
    itn.max = 600,
    minimize = TRUE,
    hm.init = hm
  )
toc()
