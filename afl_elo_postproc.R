
CheckCalibration <- function(games.elo, bin.width) {
  bin.edges <- lapply(seq(0, 1 - bin.width, by = bin.width),
                      function(x) c(x, x + bin.width))
  win.frac.home <- sapply(bin.edges, GetFracHomeWinsInBin, games.elo)
  bin.mid <- seq(bin.width / 2, 1 - bin.width / 2, by = bin.width)
  calib.error <- win.frac.home - bin.mid

  calib <- data.frame(bin.mid = bin.mid, win.frac.home = win.frac.home, calib.error = calib.error)

  #plot(calib$bin.mid, calib$win.frac.home)
  #lines(c(0, 1), c(0, 1))

  calib
}

GetFracHomeWinsInBin <- function(bin.edges, games.elo) {
  bin.lower <- bin.edges[1]
  bin.upper <- bin.edges[2]

  games.in.bin <- games.elo[games.elo$result.exp.home > bin.lower & 
                            games.elo$result.exp.home <= bin.upper, ]
  n.games <- nrow(games.in.bin)
  n.wins <- sum(games.in.bin$outcome.home)

  if (n.games == 0) {
    frac.wins <- NA
  } else {
    frac.wins <- n.wins / n.games
  }

  frac.wins
}


