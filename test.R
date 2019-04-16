
# Load packages
# install.packages('tictoc')
library(tictoc)
library(tidyverse)
library(MASS)

# Source
source('afltables_all_games_prep.R')
source('afl_elo_init.R')
source("afl_params.R")
source('afl_elo.R')
source("afl_elo_predict_game.R")
source("afl_elo_postproc.R")
source("afl_elo_sim.R")
source("afl_parse_excel_fixture.R")
source("afltables_scrape_season_fixture.R")

# Prediction run (all games to today) or testing run (games until end 2018)
yes.pred.run <- FALSE

# Download the list of all games from AFL tables?
do.download = TRUE

# Init
all.games <- GetAllGames(do.download = do.download)
if (!yes.pred.run) {
  all.games <- all.games %>% filter(season <= 2018)
}
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
                     param.margin = 0.03213133,
                     param.coeff.rating.update = 76.72256, param.regress.rating = 0.2038160,
                     param.coeff.ground.update = 1.675048,
                     param.coeff.travel = 14.01393, param.power.travel = 0.2689826,
                     param.rating.expansion.init = 1330,
                     do.store.detail = TRUE)

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
margin.cumulative.sq.error <- elo.result[[9]]

margin.rmse <- sqrt(margin.cumulative.sq.error / sum(all.games$season >= 1994))
margin.mae <- margin.sum.abs.error / sum(all.games$season >= 1994)
result.mae <- result.sum.abs.error / sum(all.games$season >= 1994)
brier.score.ave <- brier.cumulative.error / sum(all.games$season >= 1994)
log.score.ave <- log.score.cumulative.error / sum(all.games$season >= 1994)

writeLines(c(paste0("Margin RMSE = ", margin.rmse), paste0("Margin MAE = ", margin.mae), paste0("Result MAE = ", result.mae),
             paste0("Brier score ave = ", brier.score.ave), paste0("Log score ave = ", log.score.ave)))

# Games and results of interest: 1994-2016
if (!yes.pred.run) {
  games.1994.2016 <- all.games %>% filter(season >= 1994)
  elo.1994.2016 <- all.games.elo.run %>% filter(all.games$season >= 1994)
  elo.1994.2016.rs <- SelectHomeOrAwayValueRandom(elo.1994.2016, c('margin.exp', 'margin.act', 'margin.error'))
  calib.1994.2016 <- CheckCalibration(elo.1994.2016, 0.05)
} else {
  fixture <- LoadRoundFixture()
  PredictRound(fixture, 2019, "R4", all.games, team.data.run, ground.data.run, ground.location, travel.distance, team.dictionary.reverse, commission = 0.05,
               param.spread = 400,
               param.margin = 0.03213133,
               param.coeff.travel = 14.01393, param.power.travel = 0.2689826,
               con = 'out/afl_elo_pred_2019-R4.txt')
               # con = stdout())
}

