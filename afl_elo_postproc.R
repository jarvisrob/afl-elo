
CheckCalibration <- function(games.elo, bin.width) {
  bin.edges <- lapply(seq(0, 1 - bin.width, by = bin.width),
                      function(x) c(x, x + bin.width))

  games.rand <- SelectHomeOrAwayValueRandom(games.elo, c("result.exp", "outcome"))

  win.frac <- sapply(bin.edges, GetFracWinsInBin, games.rand)
  bin.mid <- seq(bin.width / 2, 1 - bin.width / 2, by = bin.width)
  calib.error <- win.frac - bin.mid

  calib <- data.frame(bin.mid = bin.mid, win.frac = win.frac, calib.error = calib.error)

  p <- ggplot(calib, aes(bin.mid, win.frac)) + geom_point() + geom_abline(intercept = 0, slope = 1)
  print(p)

  calib
}

GetFracWinsInBin <- function(bin.edges, games) {
  bin.lower <- bin.edges[1]
  bin.upper <- bin.edges[2]

  games.in.bin <- games[games$result.exp > bin.lower & games$result.exp <= bin.upper, ]
  n.games <- nrow(games.in.bin)
  n.wins <- sum(games.in.bin$outcome)

  if (n.games == 0) {
    frac.wins <- NA
  } else {
    frac.wins <- n.wins / n.games
  }

  frac.wins
}


SelectHomeOrAwayValueRandom <- function(df, columns) {
  n.games <- nrow(df)
  n.columns <- length(columns)

  rand.logical <- sample(c(TRUE, FALSE), n.games, replace = TRUE)

  df.rand <- data.frame(matrix(rep(0, n.games * n.columns), ncol = n.columns))
  colnames(df.rand) <- columns

  df.rand$selected <- "home"
  df.rand[!rand.logical, "selected"] <- "away"

  df.rand[rand.logical, columns] <- df[rand.logical, paste0(columns, '.home')]
  df.rand[!rand.logical, columns] <- df[!rand.logical, paste0(columns, '.away')]

  df.rand
}