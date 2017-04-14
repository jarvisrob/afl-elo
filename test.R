# Set the working dir
setwd("C:/Lab/afl-elo")

# Load packages
library(tictoc)

# Source
source('afltables_all_games_prep.R')
source('afl_elo_init.R')
source('afl_elo.R')
source("afl_elo_predict_game.R")

# Init
all.games <- GetAllGames(do.download = FALSE)
all.games.elo <- InitAllGamesElo(all.games)
team.dictionary <- InitTeamLDictionary()
team.dictionary.reverse <- InitTeamDictionaryReverse()
team.data <- InitTeamData()
rating.time.series <- InitRatingTimeSeries(all.games, team.data)
ground.data <- InitGroundData(all.games, team.data)
ground.location <- InitGroundLocation()
ground.panel.record <- InitGroundPanelRecord(all.games, team.dictionary)
travel.distance <- InitTravelDistance()

# Run Elo
print("Starting Elo run ...")
tic()
elo.result <- RunElo(all.games, team.dictionary, team.data,
                     ground.location, ground.data, travel.distance,
                     rating.time.series, all.games.elo,
                     param.rating.mean = 1500, param.spread = 400,
                     param.margin = 0.02395478,
                     param.coeff.rating.update = 76.75845, param.regress.rating = 0.2062030,
                     param.coeff.ground.update = 1.653744,
                     param.coeff.travel = 17.70182, param.power.travel = 0.2377348,
                     param.rating.expansion.init = 1335,
                     do.store.detail = FALSE)

#param.coeff.ground.update = 25.58827, param.regress.ground = 0.1051385,

toc()
print("... Finished Elo run")

team.data.run <- elo.result[[1]]
rating.time.series.run <- elo.result[[2]]
ground.data.run <- elo.result[[3]]
all.games.elo.run <- elo.result[[4]]
margin.sum.abs.error <- elo.result[[5]]
result.sum.abs.error <- elo.result[[6]]
brier.cumulative.error <- elo.result[[7]]
log.score.cumulative.error <- elo.result[[8]]

fixture <- LoadRoundFixture()
PredictRound(fixture, 2017, "R4", all.games, team.data.run, ground.data.run, ground.location, travel.distance, team.dictionary.reverse, commission = 0.05, con = "afl_pred_2017-R4.txt")
