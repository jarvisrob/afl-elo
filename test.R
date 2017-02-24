# Set the working dir
setwd("C:/Lab/afl-elo")

# Source
source('afltables_all_games_prep.R')
source('afl_elo_init.R')
source('afl_elo.R')

# Init
all.games <- GetAllGames(do.download = F)
all.games.elo <- InitAllGamesElo(all.games)
team.dictionary <- InitTeamLDictionary()
team.data <- InitTeamData()
rating.time.series <- InitRatingTimeSeries(all.games, team.data)
ground.data <- InitGroundData(all.games, team.data)
ground.location <- InitGroundLocation()
ground.panel.record <- InitGroundPanelRecord(all.games, team.dictionary)
travel.distance <- InitTravelDistance()


# Run Elo
tic()
elo.result <- RunElo(all.games, team.dictionary, team.data, ground.location, ground.panel.record, travel.distance, rating.time.series, all.games.elo, rating.mean = 1500, 
                     param.spread = 400,
                     param.margin = 0.01129223, 
                     param.coeff.rating.update = 66.70423, param.regress.rating = 0.1865318, 
                     param.coeff.ground.update = 25.58827, param.regress.ground = 0.1051385, 
                     param.coeff.travel = 13.60048, param.rating.expansion.init = 1301.614)
toc()
team.data.run <- elo.result[[1]]
rating.time.series.run <- elo.result[[2]]
ground.panel.record.run <- elo.result[[3]]
all.games.elo.run <- elo.result[[4]]
margin.sum.abs.error <- elo.result[[5]]
result.sum.sq.error <- elo.result[[6]]