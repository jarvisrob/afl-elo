


GetAllGames <- function(file.url = 'http://afltables.com/afl/stats/biglists/bg3.txt',
                        file.local = 'all_games.txt', do.download = TRUE) {
  
  if (do.download) {
    download.file(file.url, file.local)
  }
  
  all.games <- read.fwf(file.local, widths = c(7, 17, 5, 18, 17, 17, 18, 20), header = F, skip = 2, 
                        col.names = c('game.idx', 'date', 'round', 'team.home', 'score.home', 
                                      'team.away', 'score.away', 'ground'))
  
  all.games$game.idx <- NULL
  all.games$date <- trimws(all.games$date)
  all.games$round <- trimws(all.games$round)
  all.games$team.home <- trimws(all.games$team.home)
  all.games$score.home <- trimws(all.games$score.home)
  all.games$team.away <- trimws(all.games$team.away)
  all.games$score.away <- trimws(all.games$score.away)
  all.games$ground <- trimws(all.games$ground)
  
  all.games$date <- as.Date(all.games$date, format = '%d-%b-%Y')
  all.games$season <- as.integer(format(all.games$date, "%Y"))
  
  all.games$score.goals.home <- ParseGoalsBehindsPts(all.games$score.home, 1)
  all.games$score.behinds.home <- ParseGoalsBehindsPts(all.games$score.home, 2)
  all.games$score.points.home <- ParseGoalsBehindsPts(all.games$score.home, 3)
  
  all.games$score.goals.away <- ParseGoalsBehindsPts(all.games$score.away, 1)
  all.games$score.behinds.away <- ParseGoalsBehindsPts(all.games$score.away, 2)
  all.games$score.points.away <- ParseGoalsBehindsPts(all.games$score.away, 3)
  
  # Semi-finals in 1897 were played in three rounds, but are just listed as 'SF' in txt file
  all.games <- CorrectSfRoundRobin(all.games, 1897, c('SF1', 'SF1', 'SF2', 'SF2', 'SF3', 'SF3'))

  # Semi-final round-robin system was used in 1924, with result that no GF was required
  all.games <- CorrectSfRoundRobin(all.games, 1924, c('SF1', 'SF1', 'SF2', 'SF2', 'SF3', 'SF3'))
  
  # During 1991-1993, QF and EF were in the same round
  all.games <- CorrectQfEf(all.games, year.start = 1991, year.end = 1993)
  
  # Since 2000, QF and EF have been in the same round
  all.games <- CorrectQfEf(all.games, year.start = 2000)
  
  # 1977 and 2010 GF were originally drawn, so were replayed 1 week later
  all.games <- CorrectDrawnGf(all.games, 1977)
  all.games <- CorrectDrawnGf(all.games, 2010)
  
  all.games
}


ParseGoalsBehindsPts <- function(score, idx) {
  score.split <- strsplit(score, '.', fixed = T)
  v <- unlist(lapply(score.split, function(x) x[idx]))
  v <- as.integer(v)
  v
  
}


CorrectSfRoundRobin <- function(all.games, season, correction.vector) {
  game.idx <- (as.integer(format(all.games$date, '%Y')) == season) & (all.games$round == 'SF')
  all.games$round[game.idx] <- correction.vector
  all.games
}


CorrectQfEf <- function(all.games, year.start, year.end = '', correction.string = 'QF/EF') {
  year.start.string <- paste(as.character(year.start), '-01-01', sep = '')
  year.end.string <- paste(as.character(year.end), '-12-31', sep = '')
  
  if (year.end == '') {
    date.fits <- (all.games$date >= year.start.string)
  }
  else {
    date.fits <- (all.games$date >= year.start.string) & (all.games$date <= year.end.string)
  }
  game.fits <- date.fits & ((all.games$round == 'QF') | (all.games$round == 'EF'))
  
  all.games$round[game.fits] <- rep(correction.string, sum(game.fits))
  all.games
}


CorrectDrawnGf <- function(all.games, season) {
  gf.correction.vector <- c('GF1', 'GF2')
  game.idx <- (all.games$round == 'GF') & (as.integer(format(all.games$date, '%Y')) == season)
  all.games$round[game.idx] <- gf.correction.vector
  all.games
}
