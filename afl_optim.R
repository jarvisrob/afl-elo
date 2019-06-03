

OptimWrapperRunElo <- function(tunable.params, fixed.params, data.inputs) {

  print("Tunable params:")
  tunable.params.with.names <- tunable.params
  names(tunable.params.with.names) <-
    c(
      "margin (sd)",
      "coeff.rating.update",
      "regress.rating",
      "coeff.travel",
      "rating.exp.init"
    )
  print(tunable.params.with.names)

  # Tunable parameters
  param.margin <- tunable.params[1]
  param.coeff.rating.update <- tunable.params[2]
  param.regress.rating <- tunable.params[3]
  # param.coeff.ground.update <- tunable.params[4]
  param.coeff.ground.update <- 0  # Not being used
  param.coeff.travel <- tunable.params[4]
  # param.power.travel <- tunable.params[6]
  param.rating.expansion.init <- tunable.params[5]
  
  # Fixed parameters
  param.rating.mean <- 0
  param.spread <- 0  # No longer used
  param.power.travel <- 0  # Not being used
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
  
  # Mapping params to elo.params, in the future won't need to do this, but for now ...
  # elo.params <- 
  #   list(
  #     rating.mean = 1500,
  #     spread = 400,
  #     margin = 0.03213133,
  #     coeff.rating.update = 76.72256,
  #     regress.rating = 0.2038160,
  #     coeff.ground.update = 1.675048,
  #     coeff.travel = 14.01393,
  #     power.travel = 0.2689826,
  #     rating.expansion.init = 1330
  #   )
  
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
  
  margin.mae <- margin.sum.abs.error / sum(all.games$season >= 1994)
  print(paste0("Margin MAE = ", as.character(margin.mae)))
  #margin.mae
  
  #print(result.sum.sq.error)
  result.mae <- result.sum.abs.error / sum(all.games$season >= 1994)
  # print(paste0("Result MAE = ", result.mae))

  brier.score <- brier.cumulative.error / sum(all.games$season >= 1994)
  print(paste0("Brier score = ", as.character(brier.score)))
  #brier.cumulative.error

  #calib <- CheckCalibration(all.games.elo.run[all.games$season >= 1994, ], 0.02)
  ##print(calib)
  #ss.calib.error <- sum(calib$calib.error ^ 2, na.rm = TRUE)
  #print(paste0("SS calib error = ", as.character(ss.calib.error)))

  #print(log.score.cumulative.error)
  log.score <- log.score.cumulative.error / sum(all.games$season >= 1994)
  # print(paste0("Log score = ", as.character(log.score)))
  
  # Tuning the std dev in margin error to match the param.margin
  elo.1994.2016 <- all.games.elo.run %>% filter(all.games$season >= 1994)
  elo.1994.2016.rs <- SelectHomeOrAwayValueRandom(elo.1994.2016, c('margin.exp', 'margin.act', 'margin.error'))
  
  margin.error.distrib <- fitdistr(elo.1994.2016.rs$margin.error, 'normal')
  margin.error.mean <- margin.error.distrib$estimate[["mean"]]
  margin.error.sd <- margin.error.distrib$estimate[["sd"]]
  print(paste0("mean = ", margin.error.mean))
  print(paste0("std dev = ", margin.error.sd, " cf. param.margin = ", param.margin))
  
  # # err_sd <- abs(margin.error.sd - 1 / param.margin)
  # # err_sd <- max(0, abs(1 / param.margin - margin.error.sd) - 0.2)
  # err_sd <- max(0, abs(param.margin - margin.error.sd) - 0.5)
  # err_mean <- max(0, abs(margin.error.mean - 0) - 0.5)
  # 
  # # Cost terms
  # print(paste0("Cost term brier = ", brier.score))
  # print(paste0("Cost term margin MAE = ", margin.mae, " x 0.001 = ", 0.001 * margin.mae))
  # print(paste0("Cost term error mean = ", err_mean, " x 10 = ", 10 * err_mean))
  # print(paste0("Cost term error sd = ", err_sd, " x 10 = ", 10 * err_sd))
  # 
  # cost <- brier.score + (0.001 * margin.mae) + (10 * err_mean) + (10 * err_sd)
  # print(paste0("COST = ", cost))
  # print("---")
  # cost
  
  print("---")
  
  result <- 
    list(
      brier.score = brier.score, 
      err_mean = abs(margin.error.mean - 0),
      err_sd = abs(margin.error.sd - param.margin),
      margin_mae = margin.mae
    )

}



# RunElo <- function(all.games,                   DATA[[1]]
#                    team.dictionary,             DATA[[2]]
#                    team.data,                   DATA[[3]]
#                    ground.location,             DATA[[4]]
#                    ground.data,                 DATA[[5]]
#                    travel.distance,             DATA[[6]]
#                    rating.time.series,          DATA[[7]]
#                    all.games.elo,               DATA[[8]]
#                    elo.params,
#                    param.rating.mean,           FIXED[[1]] 
#                    param.spread,                FIXED[[2]]
#                    param.margin,                TUNABLE[[1]]
#                    param.coeff.rating.update,   TUNABLE[[2]]
#                    param.regress.rating,        TUNABLE[[3]]
#                    param.coeff.ground.update,   TUNABLE[[4]]--SET TO ZERO
#                    param.coeff.travel,          TUNABLE[[4]]
#                    param.power.travel,          TUNABLE[[6]]--NOT USED, SET TO ZERO
#                    param.rating.expansion.init, TUNABLE[[5]]
#                    do.store.detail = FALSE)


# Initial values
tunable.params.type <- 
  c(
    "continuous",
    "continuous",
    "continuous",
    "continuous",
    "continuous"
  )

tunable.params.init <-
  c(
    35.0,
    5.0,
    0.15,
    0.3,
    -15.0
  )

tunable.params.lower <- 
  c(
    30.0,
    0.01,
    0.0,
    0.0,
    -30.0
  )

tunable.params.upper <- 
  c(
    40.0,
    25.0,
    0.5,
    4.0,
    0
  )

# tunable.params.scale <-
#   c(
#     1,
#     1,
#     4,
#     # 3,
#     2
#   )

tunable.params.steps <-
  c(
    0.5,
    1.0,
    0.05,
    0.5,
    6.0
  )

# fixed.params <- 
#   c(
#     1500.0, 
#     400.0,
#     0.5,
#     1300
#   )


f.lower <- c(0, 0, 0, 0)
f.upper <- 
  c(
    1.0,  # Brier score
    0.5,  # Mean margin error
    0.5,  # Standard deviation error
    31.0  # Margin MAE
  )

fw.d <- (tunable.params.upper - tunable.params.lower) / 1000

# Data inputs
# all.games <- all.games %>% filter(season <= 2017)
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



# tic()
# optim.res <-
#   optim(
#     par = tunable.params.init,
#     fn = OptimWrapperRunElo,
#     # fixed.params = fixed.params,
#     data.inputs = data.inputs,
#     method = "L-BFGS-B",
#     lower = tunable.params.lower,
#     upper = tunable.params.upper,
#     control = list(
#       maxit = 1000,
#       # parscale = tunable.params.scale
#       ndeps = tunable.params.steps
#     )
#   )
# toc()


# Results from first go at optimisation are below, with Brier and SD error being pretty good!
# However, in some cases they haven't moved far from init values
# Suggests that more params than needed to tune the model, some could be consts
# chosen in advance

# > optim.res$par[[1]]
# [1] 0.02618325
# > optim.res$par[[2]]
# [1] 74.9716
# > optim.res$par[[3]]
# [1] 0.2579766
# > optim.res$par[[4]]
# [1] 1.268286
# > optim.res$par[[5]]
# [1] 14.99937
# > optim.res$par[[6]]
# [1] 0.2090868
# > optim.res$par[[7]]
# [1] 1301.193

# Perhaps:
# - margin param could be fixed, fixing the SD of margin (tunable param 1 -> const) MOST CONTROVERSIAL
# - Travel curve could be fixed sqrt() or ln() curve with just coeff (tunable params 5, 6 -> single param)
# - Starting Elo for expansion teams could be fixed (tunable param 7 -> const)
# 
# Also, potentially significant colinearity between travel coeff and ground update coeff, should we divorce this?



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
    itn.max = 500, 
    minimize = TRUE,
    hm.init = NULL
  )
toc()
