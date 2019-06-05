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
    30.0,
    0.01,
    0.0,
    0.0,
    0.0,
    0.0,
    -36.0
  )

tunable.params.upper <- 
  c(
    40.0,
    25.0,
    0.5,
    25.0,
    5.0,
    1.0,
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
    30.5  # Margin MAE
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
    itn.max = 600, 
    minimize = TRUE,
    hm.init = hm
  )
toc()

# $f1.best
# [1] 0.1992353
# 
# $x.best
# [1]  38.0707255   8.9181117   0.1181192   0.8887767   1.1485622   0.3170362 -17.2034813
# 
# $f.best
# [1]  0.1992353  0.0520821  0.4357437 30.0206816
# 
# $idx.best
# [1] 22
# 
# $hm
# [,1]      [,2]       [,3]      [,4]      [,5]      [,6]       [,7]      [,8]       [,9]
# [1,] 38.08234 11.000966 0.15484662 2.1441346 1.1485622 0.3165728 -16.281128 0.1995249 0.49081473
# [2,] 37.31193 11.017103 0.07919458 1.8292209 1.1457779 0.3162249 -16.269384 0.1994105 0.12418706
# [3,] 38.07073 11.017103 0.07909958 0.8887767 0.4769697 0.4172853  -6.240519 0.1992706 0.32037469
# [4,] 37.31193 10.997421 0.07914677 2.1406156 1.1527953 0.3922446 -31.146180 0.1994944 0.01348124
# [5,] 38.06518 11.000966 0.09221455 2.1262315 1.1457779 0.3162249 -16.303718 0.1995226 0.10402931
# [6,] 38.07073 11.000966 0.15484662 2.1262315 1.1476063 0.3170362 -16.303718 0.1995157 0.36389361
# [7,] 38.07414 11.023093 0.15473296 2.1271048 1.1476063 0.3161726 -17.203481 0.1995056 0.04358839
# [8,] 37.31193 11.000966 0.16591504 2.1441346 1.1522361 0.3163149 -16.283327 0.1995145 0.47974584
# [9,] 38.06518 11.027509 0.11811924 2.1303114 1.0002667 0.3162249 -22.744530 0.1994852 0.17984540
# [10,] 38.08109  9.305348 0.07909958 1.8292209 1.1476063 0.3320965 -16.303718 0.1994819 0.27517567
# [11,] 38.08293 11.000966 0.07918843 1.8414601 1.0002667 0.3180177 -16.281128 0.1994815 0.16243026
# [12,] 38.06783 11.023093 0.15457420 2.1438589 1.1403743 0.3320965 -16.281128 0.1994886 0.21919266
# [13,] 37.31417 11.000966 0.11760571 1.8224434 1.0002667 0.3161726 -16.308226 0.1994392 0.27998436
# [14,] 38.06783 11.016366 0.11703674 1.8292209 1.1471986 0.3170362 -16.303718 0.1993882 0.18659768
# [15,] 38.08647 11.000966 0.11811924 1.1500639 1.1442738 0.3931815 -16.269384 0.1994122 0.02283384
# [16,] 38.08293 10.997421 0.15484662 1.8457514 1.1484529 0.3170362 -16.303718 0.1994002 0.11042915
# [17,] 38.08376 11.017103 0.15484662 2.1262315 1.1471986 0.3163149 -17.203481 0.1995051 0.27035980
# [18,] 37.30626 11.027509 0.16547965 1.8224434 0.4707512 0.4176094 -16.332720 0.1994161 0.16731887
# [19,] 38.07524 11.027509 0.07914708 2.1543151 1.1485622 0.3320965 -16.303718 0.1995053 0.12000770
# [20,] 37.31193 11.017103 0.15473296 2.1584211 0.9957474 0.3320965 -17.202753 0.1995082 0.39108109
# [21,] 38.08062 10.989226 0.16547965 2.1388801 1.1458626 0.3170362 -17.203481 0.1995144 0.20756944
# [22,] 38.07073  8.918112 0.11811924 0.8887767 1.1485622 0.3170362 -17.203481 0.1992353 0.05208210
# [23,] 38.07649 11.017103 0.11730317 2.1441346 1.1489347 0.3165728 -17.203481 0.1995042 0.03490075
# [24,] 38.07985 11.000966 0.07973805 2.1262315 1.1485622 0.3162249 -17.202753 0.1995189 0.13030803
# [25,] 37.29813 11.000966 0.08012869 1.8224434 1.0002667 0.3922766 -17.203481 0.1993911 0.35363930
# [26,] 37.30626 11.017103 0.15457420 2.1498674 1.1446609 0.3320965 -17.203481 0.1994669 0.05721250
# [27,] 37.31193 11.027509 0.07973805 1.8224434 1.1468466 0.3170362 -17.215677 0.1993909 0.08513407
# [28,] 37.31417 11.004781 0.15752855 1.8105133 1.1435301 0.3161726 -17.203481 0.1993541 0.01043412
# [29,] 38.07425 11.000966 0.06720849 2.1584211 0.4754272 0.4176563 -22.744530 0.1995084 0.30764178
# [30,] 38.07524  8.918112 0.07909211 2.1262315 1.1442738 0.3170362 -31.369822 0.1994712 0.18820522
# [,10]    [,11]
# [1,] 0.4819298 30.02371
# [2,] 0.2939092 30.03062
# [3,] 0.4498802 30.02363
# [4,] 0.2811551 29.98851
# [5,] 0.4607615 30.03278
# [6,] 0.4697992 30.02278
# [7,] 0.4771349 30.01948
# [8,] 0.3071596 30.03341
# [9,] 0.4958318 30.01440
# [10,] 0.4391156 30.04815
# [11,] 0.4848930 30.03342
# [12,] 0.4675968 30.01608
# [13,] 0.2937738 30.03539
# [14,] 0.4821577 30.01297
# [15,] 0.4417992 30.00833
# [16,] 0.4946852 30.00972
# [17,] 0.4878617 30.01934
# [18,] 0.3044368 30.03230
# [19,] 0.4690494 30.02873
# [20,] 0.3018543 30.03269
# [21,] 0.4818639 30.01980
# [22,] 0.4357437 30.02068
# [23,] 0.4797833 30.02440
# [24,] 0.4783735 30.03213
# [25,] 0.3183036 30.01060
# [26,] 0.3058263 30.02275
# [27,] 0.2877345 30.02610
# [28,] 0.2841578 30.01533
# [29,] 0.4957579 30.03058
# [30,] 0.4912703 30.02181
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
