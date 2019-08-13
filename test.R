# Load packages
library(tictoc)
library(MASS)
library(tidyverse)

# Source
source('afltables_all_games_prep.R')
source('afl_elo_init.R')
source("afl_params.R")
source('afl_elo.R')
source("afl_elo_predict_game.R")
source("afl_elo_postproc.R")
source("afl_elo_sim.R")
source("afl_parse_excel_fixture.R")
source("afltables_scrape.R")
source("afl_fixture_manipulation.R")

# Prediction run (all games to today) or testing run (games until end 2017)
yes.pred.run <- FALSE

# Download the list of all games from AFL tables?
do.download = TRUE

# Season to start tuning from
season.start.tuning <- 2000

# Season to end tuning
season.end.tuning <- 2017

# Init
all.games <- GetAllGames(do.download = do.download)
if (!yes.pred.run) {
  all.games <- all.games %>% filter(season <= season.end.tuning)
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
    param.rating.mean = 0,
    param.spread = 0,
    param.margin = 36.84553274,
    param.coeff.rating.update = 10.29627330,
    param.regress.rating = 0.15732180,
    param.coeff.ground.update = 0.24685585,
    param.coeff.travel = 0.92211573,
    param.power.travel = 0.35214608,
    param.rating.expansion.init = -79.06871615,
    do.store.detail = TRUE
  )
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

margin.rmse <- sqrt(margin.cumulative.sq.error / sum(all.games$season >= season.start.tuning))
margin.mae <- margin.sum.abs.error / sum(all.games$season >= season.start.tuning)
result.mae <- result.sum.abs.error / sum(all.games$season >= season.start.tuning)
brier.score.ave <- brier.cumulative.error / sum(all.games$season >= season.start.tuning)
log.score.ave <- log.score.cumulative.error / sum(all.games$season >= season.start.tuning)

writeLines(
  c(
    paste0("Margin RMSE = ", margin.rmse), 
    paste0("Margin MAE = ", margin.mae), 
    paste0("Result MAE = ", result.mae),
    paste0("Brier score ave = ", brier.score.ave), 
    paste0("Log score ave = ", log.score.ave)
  )
)

# Games and results of interest: 1994-2018
if (!yes.pred.run) {
  tuning_games <-
    all.games.elo.run %>%
      mutate(season = all.games$season) %>%
      filter(season >= season.start.tuning) %>%
      dplyr::select(
        season,
        team.home, 
        team.away, 
        delta.rating.home, 
        margin.act.home,
        margin.error.home,
        result.exp.home, 
        outcome.home,
        result.exp.away, 
        outcome.away, 
        brier.game
      ) %>%
      rename(
        home = team.home,
        away = team.away,
        margin_expected = delta.rating.home,
        margin_actual = margin.act.home,
        margin_error = margin.error.home,
        p_home_win = result.exp.home,
        outcome_home = outcome.home,
        p_away_win = result.exp.away,
        outcome_away = outcome.away,
        brier = brier.game
      )
  
  tuning_games_home <-
    tuning_games %>%
      dplyr::select(p_home_win, outcome_home) %>% 
      rename(p = p_home_win, outcome = outcome_home)
  
  tuning_games_away <-
    tuning_games %>%
      dplyr::select(p_away_win, outcome_away) %>% 
      rename(p = p_away_win, outcome = outcome_away)
  
  tuning_games_pred_act <- union(tuning_games_home, tuning_games_away)
  
  n_preds <- tuning_games_pred_act %>% nrow()
  
  plt <-
    tuning_games_pred_act %>%
      group_by(cut(p, breaks = seq(0, 1, 0.05))) %>%
      summarize(n(), sum(outcome)) %>%
      rename(bin = `cut(p, breaks = seq(0, 1, 0.05))`, n_games = `n()`, n_wins = `sum(outcome)`) %>%
      mutate(bin_centre = seq(0.025, 0.975, 0.05), win_fraction = n_wins / n_games) %>%
      ggplot(aes(bin_centre, win_fraction)) +
        geom_point() +
        geom_abline(slope = 1, intercept = 0) +
        scale_x_continuous(limits = c(0, 1)) +
        labs(
          x = "Predicted probability",
          y = "Actual win fraction",
          title = "Calibration on tuning data"
        )
  print(plt)
  

} else {
  
  season <- 2019
  rnd <- 21
  
  # For R13, if AFL Tables hasn't yet added the venue for GC v STK, will need to
  # manually add it:
  # fixture.season[108, "team.away"] <- "St Kilda"
  # fixture.season[108, "ground"] <- "Riverway Stadium"
  
  fixture.season <-
    ScrapeAFLTablesSeasonFixture(season)
  
  fixture.season[108, "team.away"] <- "St Kilda"
  fixture.season[108, "ground"] <- "Riverway Stadium"
  
  fixture.season %>%
    ExtractRoundFixture(rnd) %>% 
      mutate(
        team.home = map_chr(team.home, ~team.dictionary[[.]]), 
        team.away = map_chr(team.away, ~team.dictionary[[.]])
      ) %>%
    PredictRound(
      season, 
      str_c("R", rnd), 
      all.games, 
      team.data.run, 
      ground.data.run, 
      ground.location, 
      travel.distance, 
      team.dictionary.reverse, 
      commission = 0.05,
      param.spread = 0,
      param.margin = 36.84553274,
      param.coeff.travel = 0.92211573,
      param.power.travel = 0.35214608,
      con = str_c("out/afl_elo_pred_", season, "-R", rnd, ".txt")
      # con = stdout()
    )
  
}

