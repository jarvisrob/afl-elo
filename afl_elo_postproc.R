
CheckCalibration <- function(all.games.elo, bin.width) {

}

GetFracHomeWinsInBin <- function(all.games.elo, bin.lower, bin.upper) {
  games.in.bin <- all.games.elo[all.games.elo$result.exp.home > bin.lower & 
                                all.games.elo$result.exp.home <= bin.upper, ]
  n.games <- nrow(games.in.bin)
  n.wins <- nrow(games.in.bin[games.in.bin$outcome.home == 1,])
  frac.wins <- n.wins / n.games

  frac.wins
}


