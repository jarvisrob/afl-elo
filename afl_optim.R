
# library(MASS)

# elo.params <- 
#   list(
#     rating.mean = 1500,               FIXED
#     spread = 400,                     FIXED
#     margin = 0.03213133,              TUNABLE
#     coeff.rating.update = 76.72256,   TUNABLE
#     regress.rating = 0.2038160,       TUNABLE
#     coeff.ground.update = 1.675048,   TUNABLE
#     coeff.travel = 14.01393,          TUNABLE
#     power.travel = 0.2689826,         TUNABLE
#     rating.expansion.init = 1330      TUNABLE
#   )


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
#                    param.coeff.ground.update,   TUNABLE[[4]]
#                    param.coeff.travel,          TUNABLE[[5]]
#                    param.power.travel,          TUNABLE[[6]]
#                    param.rating.expansion.init, TUNABLE[[7]]
#                    do.store.detail = FALSE)
  




OptimWrapperRunElo <- function(tunable.params, fixed.params, data.inputs) {

  print("Tunable params:")
  print(tunable.params)

  # Tunable parameters
  param.margin <- 1 / tunable.params[1]
  param.coeff.rating.update <- tunable.params[2]
  param.regress.rating <- tunable.params[3]
  # param.coeff.ground.update <- tunable.params[4]
  param.coeff.ground.update <- 0
  param.coeff.travel <- tunable.params[4]
  # param.power.travel <- tunable.params[6]
  # param.rating.expansion.init <- tunable.params[7]
  
  # Fixed parameters
  param.rating.mean <- fixed.params[1]
  param.spread <- fixed.params[2]
  param.power.travel <- fixed.params[3]
  param.rating.expansion.init <- fixed.params[4]
  
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
  elo.params$margin <- param.margin
  elo.params$coeff.rating.update <- param.coeff.rating.update
  elo.params$regress.rating <- param.regress.rating
  elo.params$coeff.ground.update <- param.coeff.ground.update
  elo.params$coeff.travel <- param.coeff.travel
  elo.params$power.travel <- param.power.travel
  elo.params$rating.expansion.init <- param.rating.expansion.init
  
  
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
  print(paste0("Result MAE = ", result.mae))

  brier.score <- brier.cumulative.error / sum(all.games$season >= 1994)
  print(paste0("Brier score = ", as.character(brier.score)))
  #brier.cumulative.error

  #calib <- CheckCalibration(all.games.elo.run[all.games$season >= 1994, ], 0.02)
  ##print(calib)
  #ss.calib.error <- sum(calib$calib.error ^ 2, na.rm = TRUE)
  #print(paste0("SS calib error = ", as.character(ss.calib.error)))

  #print(log.score.cumulative.error)
  log.score <- log.score.cumulative.error / sum(all.games$season >= 1994)
  print(paste0("Log score = ", as.character(log.score)))
  
  # Tuning the std dev in margin error to match the param.margin
  elo.1994.2016 <- all.games.elo.run %>% filter(all.games$season >= 1994)
  elo.1994.2016.rs <- SelectHomeOrAwayValueRandom(elo.1994.2016, c('margin.exp', 'margin.act', 'margin.error'))
  
  margin.error.distrib <- fitdistr(elo.1994.2016.rs$margin.error, 'normal')
  margin.error.mean <- margin.error.distrib$estimate[["mean"]]
  margin.error.sd <- margin.error.distrib$estimate[["sd"]]
  print(paste0("mean = ", margin.error.mean))
  print(paste0("std dev = ", margin.error.sd, " | 1/param.margin = ", 1 / param.margin))
  
  # err_sd <- abs(margin.error.sd - 1 / param.margin)
  err_sd <- max(0, abs(1 / param.margin - margin.error.sd) - 0.2)
  err_mean <- max(0, abs(margin.error.mean - 0) - 0.4)
  
  # Cost terms
  print(paste0("Cost term brier = ", brier.score))
  print(paste0("Cost term mean = ", err_mean))
  print(paste0("Cost term sd = ", err_sd))
  
  # output <- list(brier.score = brier.score, err = err)
  cost <- brier.score + (100 * err_mean) + (100 * err_sd)
  print(paste0("cost = ", cost))
  print("---")
  
  cost

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
#                    param.power.travel,          TUNABLE[[6]]--FIXED
#                    param.rating.expansion.init, TUNABLE[[7]]--FIXED
#                    do.store.detail = FALSE)


# Initial values
# tunable.params.type <- c("continuous", 
#                          "continuous", "continuous", 
#                          "continuous",
#                          "continuous", "continuous", 
#                          "continuous")


tunable.params.lower <- 
  c(
    20.0,
    10.0, 
    0.001,
    # 0.001,
    0.001
    # 0.10,
    # 1000.0
  )

tunable.params.init <-
  c(
    35.0,
    100.0,
    0.25,
    # 1.0,
    20.0
    # 0.25,
    # 1300.0
  )

tunable.params.upper <- 
  c(
    50.0,
    200.0,
    0.5,
    # 100.0,
    50.0 
    # 0.50,
    # 1480.0
  )

tunable.params.scale <-
  c(
    1,
    1,
    4,
    # 3,
    2
  )

tunable.params.steps <-
  c(
    0.5,
    5,
    0.05,
    # 0.5,
    0.5
  )

fixed.params <- 
  c(
    1500.0, 
    400.0,
    0.5,
    1300
  )

# f.lower <- c(0, 0)
# f.upper <- c(Inf, 0.1)  # brier score, error in 1/param.margin rel to margin error std dev
# 
# fw.d <- (tunable.params.upper - tunable.params.lower) / 1000

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



tic()
optim.res <-
  optim(
    par = tunable.params.init,
    fn = OptimWrapperRunElo,
    fixed.params = fixed.params,
    data.inputs = data.inputs,
    method = "L-BFGS-B",
    lower = tunable.params.lower,
    upper = tunable.params.upper,
    control = list(
      maxit = 1000,
      # parscale = tunable.params.scale
      ndeps = tunable.params.steps
    )
  )
toc()


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



# # Optimisation call
# source("../stochoptim/harmony_search.R")
# 
# 
# tic()
# harm.search.res <- HarmonySearch(OptimWrapperRunElo, fixed.params = fixed, data.inputs = df.inputs,
#                                  x.type = tunable.params.type, 
#                                  x.lower = tunable.params.lower, 
#                                  x.upper = tunable.params.upper, 
#                                  f.lower = f.lower,
#                                  f.upper = f.upper,
#                                  fw.d = fw.d,
#                                  hms = 3, hmcr = 0.9, par = 0.3,
#                                  itn.max = 1, minimize = TRUE,
#                                  hm.init = NULL)
# 
# #optim.result <- optim(par = tunable.params.init, OptimWrapperRunElo,
#                       #fixed.params = fixed, data.inputs = df.inputs,
#                       #method = "L-BFGS-B",
#                       #lower = tunable.params.lower, upper = tunable.params.upper)
# toc()
