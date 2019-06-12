#                    param.margin,                TUNABLE[[1]]
#                    param.coeff.rating.update,   TUNABLE[[2]]
#                    param.regress.rating,        TUNABLE[[3]]
#                    param.coeff.ground.update,   TUNABLE[[4]]
#                    param.coeff.travel,          TUNABLE[[5]]
#                    param.power.travel,          TUNABLE[[6]]
#                    param.rating.expansion.init, TUNABLE[[7]]


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
  
  margin.mae <- margin.sum.abs.error / sum(all.games$season >= season.start.tuning)
  print(paste0("Margin MAE = ", as.character(margin.mae)))
  #margin.mae
  
  #print(result.sum.sq.error)
  result.mae <- result.sum.abs.error / sum(all.games$season >= season.start.tuning)
  # print(paste0("Result MAE = ", result.mae))

  brier.score <- brier.cumulative.error / sum(all.games$season >= season.start.tuning)
  print(paste0("Brier score = ", as.character(brier.score)))
  #brier.cumulative.error

  #calib <- CheckCalibration(all.games.elo.run[all.games$season >= 1994, ], 0.02)
  ##print(calib)
  #ss.calib.error <- sum(calib$calib.error ^ 2, na.rm = TRUE)
  #print(paste0("SS calib error = ", as.character(ss.calib.error)))

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
#                    param.coeff.ground.update,   TUNABLE[[4]]
#                    param.coeff.travel,          TUNABLE[[5]]
#                    param.power.travel,          TUNABLE[[6]]
#                    param.rating.expansion.init, TUNABLE[[7]]
#                    do.store.detail = FALSE)


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


# tunable.params.init <-
#   c(
#     35.0,
#     5.0,
#     0.15,
#     0.3,
#     -15.0
#   )


#                    param.margin,                TUNABLE[[1]]
#                    param.coeff.rating.update,   TUNABLE[[2]]
#                    param.regress.rating,        TUNABLE[[3]]
#                    param.coeff.ground.update,   TUNABLE[[4]]
#                    param.coeff.travel,          TUNABLE[[5]]
#                    param.power.travel,          TUNABLE[[6]]
#                    param.rating.expansion.init, TUNABLE[[7]]


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

# tunable.params.scale <-
#   c(
#     1,
#     1,
#     4,
#     # 3,
#     2
#   )

# tunable.params.steps <-
#   c(
#     0.5,
#     1.0,
#     0.05,
#     0.5,
#     6.0
#   )

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
    30.0  # Margin MAE
  )

fw.d <- (tunable.params.upper - tunable.params.lower) / 2000

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
    itn.max = 600, # prev was 600 + 600
    minimize = TRUE,
    hm.init = hm
  )
toc()

# $f1.best
# X8 
# 0.1983515 
# 
# $x.best
# X1          X2          X3          X4          X5          X6          X7 
# 36.9126194  10.3551736   0.1515931   0.2543393   1.0173340   0.3389640 -49.9844428 
# 
# $f.best
# X8         X9        X10        X11 
# 0.1983515  0.2068169  0.4942553 29.8336732 
# 
# $idx.best
# [1] 23
# 
# $hm
# X1       X2        X3        X4       X5        X6        X7        X8         X9       X10      X11
# [1,] 36.91252 10.35526 0.1516093 0.2461267 1.063219 0.3340408 -49.97795 0.1983537 0.15236211 0.4954084 29.83303
# [2,] 36.91244 10.35526 0.1516170 0.2450042 1.063283 0.3345510 -49.97616 0.1983537 0.02872622 0.4958913 29.83288
# [3,] 36.91265 10.35490 0.1518429 0.2460753 1.063219 0.3340369 -49.97814 0.1983537 0.31713536 0.4942957 29.83307
# [4,] 36.91233 10.35509 0.1516170 0.2452377 1.063279 0.3340348 -49.98641 0.1983536 0.43952850 0.4933785 29.83300
# [5,] 36.91257 10.35515 0.1518411 0.2461409 1.063283 0.3345510 -49.97616 0.1983537 0.38742340 0.4937106 29.83293
# [6,] 36.91260 10.35517 0.1518409 0.2460753 1.063288 0.3340395 -49.97804 0.1983537 0.04797221 0.4956552 29.83307
# [7,] 36.91258 10.35490 0.1516053 0.2474112 1.063283 0.3345518 -49.97608 0.1983537 0.09130621 0.4954470 29.83291
# [8,] 36.91258 10.35511 0.1515875 0.2452377 1.063229 0.3340333 -49.97814 0.1983537 0.29176414 0.4945929 29.83302
# [9,] 36.91257 10.35508 0.1516093 0.2452383 1.063283 0.3340455 -49.97814 0.1983537 0.41348111 0.4934676 29.83302
# [10,] 36.91271 10.35536 0.1518479 0.2461409 1.063143 0.3340650 -49.98075 0.1983537 0.10946832 0.4953911 29.83306
# [11,] 36.91265 10.35490 0.1518411 0.2461280 1.063246 0.3345510 -49.98090 0.1983537 0.49145659 0.4923930 29.83292
# [12,] 36.91271 10.35508 0.1518444 0.2452383 1.063219 0.3345510 -49.97616 0.1983537 0.25412183 0.4947835 29.83292
# [13,] 36.91271 10.35536 0.1518487 0.2452377 1.063135 0.3340568 -49.97809 0.1983537 0.05346795 0.4955944 29.83305
# [14,] 36.91245 10.36554 0.1518416 0.2487955 1.057508 0.3340352 -49.97814 0.1983536 0.03580407 0.4953525 29.83321
# [15,] 36.91281 10.35517 0.1518416 0.2461230 1.063219 0.3340333 -49.98651 0.1983537 0.12438147 0.4952266 29.83305
# [16,] 36.91259 10.35516 0.1516093 0.2450042 1.063219 0.3340371 -49.98451 0.1983537 0.32815588 0.4942797 29.83300
# [17,] 36.91258 10.35509 0.1515931 0.2452383 1.063283 0.3340395 -49.97808 0.1983537 0.14929477 0.4954341 29.83302
# [18,] 36.91263 10.35527 0.1516053 0.2452377 1.057508 0.3340487 -49.98093 0.1983536 0.38676863 0.4936948 29.83326
# [19,] 36.91271 10.35490 0.1516170 0.2450042 1.063135 0.3340480 -49.97814 0.1983537 0.16526897 0.4952621 29.83302
# [20,] 36.91241 10.35508 0.1518429 0.2452377 1.063446 0.3340487 -49.98444 0.1983536 0.31169350 0.4946244 29.83303
# [21,] 36.91257 10.35474 0.1518411 0.2452383 1.063253 0.3340480 -49.97804 0.1983537 0.07910240 0.4957154 29.83306
# [22,] 36.91273 10.35509 0.1518411 0.2452377 1.063143 0.3340371 -49.97814 0.1983537 0.36678282 0.4938250 29.83306
# [23,] 36.91262 10.35517 0.1515931 0.2543393 1.017334 0.3389640 -49.98444 0.1983515 0.20681691 0.4942553 29.83367
# [24,] 36.91271 10.36558 0.1515931 0.2461277 1.063279 0.3340424 -49.97814 0.1983537 0.36564806 0.4934723 29.83289
# [25,] 36.91273 10.35524 0.1516135 0.2452383 1.063143 0.3345510 -49.98651 0.1983536 0.31834383 0.4941917 29.83285
# [26,] 36.91258 10.35538 0.1518429 0.2450042 1.063660 0.3340487 -49.98651 0.1983537 0.28058913 0.4946995 29.83301
# [27,] 36.91258 10.40304 0.1516053 0.2487986 1.005584 0.3387744 -49.97788 0.1983529 0.20235318 0.4939050 29.83371
# [28,] 36.91263 10.35517 0.1515931 0.2461368 1.063219 0.3340568 -49.98093 0.1983537 0.06131369 0.4955464 29.83302
# [29,] 36.91252 10.35522 0.1516170 0.2474087 1.063384 0.3345518 -49.97811 0.1983537 0.18966111 0.4951280 29.83290
# [30,] 36.91270 10.35515 0.1515898 0.2450042 1.063143 0.3340560 -49.97814 0.1983537 0.22714576 0.4949376 29.83301
# 
# $cv
# f.cv.total
# [1,] 0 0 0 0          0
# [2,] 0 0 0 0          0
# [3,] 0 0 0 0          0
# [4,] 0 0 0 0          0
# [5,] 0 0 0 0          0
# [6,] 0 0 0 0          0
# [7,] 0 0 0 0          0
# [8,] 0 0 0 0          0
# [9,] 0 0 0 0          0
# [10,] 0 0 0 0          0
# [11,] 0 0 0 0          0
# [12,] 0 0 0 0          0
# [13,] 0 0 0 0          0
# [14,] 0 0 0 0          0
# [15,] 0 0 0 0          0
# [16,] 0 0 0 0          0
# [17,] 0 0 0 0          0
# [18,] 0 0 0 0          0
# [19,] 0 0 0 0          0
# [20,] 0 0 0 0          0
# [21,] 0 0 0 0          0
# [22,] 0 0 0 0          0
# [23,] 0 0 0 0          0
# [24,] 0 0 0 0          0
# [25,] 0 0 0 0          0
# [26,] 0 0 0 0          0
# [27,] 0 0 0 0          0
# [28,] 0 0 0 0          0
# [29,] 0 0 0 0          0
# [30,] 0 0 0 0          0
