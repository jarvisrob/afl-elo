
PredictRound <- function(fixture, season, rnd,
                         all.games,
                         team.data, ground.data, ground.location, travel.distance,
                         team.dictionary.reverse,
                         con = stdout(),
                         commission = 0,
                         param.spread = 400,
                         param.margin = 0.02395478,
                         param.coeff.travel = 17.70182, param.power.travel = 0.2377348) {

  txt <- c("", strrep("+", 26), sprintf("PREDICTIONS for %s-%s", season, rnd), strrep("+", 26))

  date.time.stamp <- paste0("Generated: ", Sys.time())
  last.game.idx <- nrow(all.games)
  last.game <- all.games[last.game.idx, ]
  last.game.summary <- paste0("Last known game: ", last.game.idx, ". ",
                              last.game$season, "-", last.game$round, " (",
                              last.game$date, ") ",
                              last.game$team.home, " ", last.game$score.home,
                              " vs ", last.game$team.away, " ", last.game$score.away,
                              " at ", last.game$ground)
  txt <- append(txt, c("", date.time.stamp, last.game.summary))

  git.commit.sha <- paste0("Git commit hash: ", system("git rev-parse HEAD", intern = TRUE))
  git.status.output <- system("git status -s", intern = TRUE)
  if (length(git.status.output) == 0) {
    git.status.txt <- "Git status: Working tree clean"
  } else {
    git.status.txt <- append("Git status:", git.status.output)
  }
  txt <- append(txt, c("", git.commit.sha))
  txt <- append(txt, git.status.txt)

  for (i in 1:nrow(fixture)) {
    team.home <- fixture[i, "team.home"]
    team.away <- fixture[i, "team.away"]
    ground <- fixture[i, "ground"]
    prediction <- PredictGame(team.home, team.away, ground,
                              team.data, ground.data, ground.location, travel.distance,
                              commission, param.spread, param.margin, 
                              param.coeff.travel, param.power.travel)
    prediction.txt <- WriteSinglePrediction("quiet", prediction, team.dictionary.reverse)
    txt <- append(txt, c("", "", prediction.txt))
  }
  txt <- append(txt, "")

  writeLines(txt, con)
}


LoadRoundFixture <- function(file.name = "round_fixture.txt") {
  fixture <- read.table(file.name, header = T, sep = '', skip = 2, strip.white = T, as.is = T)
}


PredictGame <- function(team.home, team.away, ground,
                        team.data, ground.data, ground.location, travel.distance, 
                        commission = 0, 
                        param.spread = 400, 
                        param.margin = 0.02395478,
                        param.coeff.travel = 17.70182, param.power.travel = 0.2377348) {

  # Extract team ratings
  rating.home <- team.data[team.home, 'rating']
  rating.away <- team.data[team.away, 'rating']

  # Determine ground adjustments
  rating.ground.adj.home <- CalculateGroundAdj(team.home, ground, ground.data)
  rating.ground.adj.away <- CalculateGroundAdj(team.away, ground, ground.data)

  # Determine travel adjustments
  rating.travel.adj.home <- CalculateTravelAdj(team.home, ground, team.data, ground.location, travel.distance, param.coeff.travel, param.power.travel)
  rating.travel.adj.away <- CalculateTravelAdj(team.away, ground, team.data, ground.location, travel.distance, param.coeff.travel, param.power.travel)

  # Determine adjusted ratings = rating + ground adj + travel adj
  rating.home.adj <- rating.home + rating.ground.adj.home + rating.travel.adj.home
  rating.away.adj <- rating.away + rating.ground.adj.away + rating.travel.adj.away

  # Calculate the difference in adj ratings and the expected result and margin
  delta.ratings <- rating.home.adj - rating.away.adj
  result.exp.home <- CalculateResultExp(delta.ratings, param.spread)
  result.exp.away <- 1 - result.exp.home
  margin.exp.home <- CalculateMarginExp(delta.ratings, param.spread, param.margin)
  margin.exp.away <- -margin.exp.home

  # Betting odds
  back.bookie.home <- 1 / result.exp.home
  back.bookie.away <- 1 / result.exp.away
  back.exchange.home <- (1 / result.exp.home - 1) / (1 - commission) + 1
  back.exchange.away <- (1 / result.exp.away - 1) / (1 - commission) + 1
  lay.exchange.home <- (1 / result.exp.home - 1) * (1 - commission) + 1
  lay.exchange.away <- (1 / result.exp.away - 1) * (1 - commission) + 1

  ## Print details
  #print(paste0("HOME | Rating = ", as.character(round(rating.home, 1)),
               #" | Ground adj = ", as.character(round(rating.ground.adj.home, 1)),
               #" | Travel adj = ", as.character(round(rating.travel.adj.home, 1)),
               #" | Adj rating = ", as.character(round(rating.home.adj, 1))))
  #print(paste0("     | p = ", as.character(round(result.exp.home * 100, 1)), " per cent"))
  #print(paste0("     | Bookie:   Back > ", round(back.bookie.home, 2)))
  #print(paste0("     | Exchange: Back > ", round(back.exchange.home, 2),
               #" and/or Lay < ", round(lay.exchange.home, 2)))
  #print(paste0("AWAY | Rating = ", as.character(round(rating.away, 1)),
               #" | Ground adj = ", as.character(round(rating.ground.adj.away, 1)),
               #" | Travel adj = ", as.character(round(rating.travel.adj.away, 1)),
               #" | Adj rating = ", as.character(round(rating.away.adj, 1))))
  #print(paste0("     | p = ", as.character(round(result.exp.away * 100, 1)), " per cent"))
  #print(paste0("     | Bookie:   Back > ", round(back.bookie.away, 2)))
  #print(paste0("     | Exchange: Back > ", round(back.exchange.away, 2),
               #" and/or Lay < ", round(lay.exchange.away, 2)))
    
  # Return prediction
  prediction <- list(team.home = team.home, team.away = team.away, ground = ground,
                     rating.home = rating.home,
                     rating.ground.adj.home = rating.ground.adj.home,
                     rating.travel.adj.home = rating.travel.adj.home,
                     rating.home.adj = rating.home.adj,
                     rating.away = rating.away,
                     rating.ground.adj.away = rating.ground.adj.away,
                     rating.travel.adj.away = rating.travel.adj.away,
                     rating.away.adj = rating.away.adj,
                     delta.home = delta.ratings,
                     delta.away = -delta.ratings,
                     result.exp.home = result.exp.home, result.exp.away = result.exp.away,
                     margin.exp.home = margin.exp.home, margin.exp.away = margin.exp.away,
                     back.bookie.home = back.bookie.home, back.bookie.away = back.bookie.away,
                     back.exchange.home = back.exchange.home, lay.exchange.home = lay.exchange.home,
                     back.exchange.away = back.exchange.away, lay.exchange.away = lay.exchange.away,
                     commission = commission)

  prediction

}


WriteSinglePrediction <- function(con, prediction, team.dictionary.reverse) {
  col.widths <- c(13, 20, 20)
  cell.lead.space <- 2
  row.n.char <- sum(col.widths, 2, 2 * cell.lead.space)

  border.plus <- strrep("+", row.n.char)
  border.dash <- strrep("-", row.n.char)

  team.home.full <- team.dictionary.reverse[[prediction$team.home]]
  team.away.full <- team.dictionary.reverse[[prediction$team.away]]

  tip.home <- GetDescriptiveTip(prediction$result.exp.home)
  tip.away <- GetDescriptiveTip(prediction$result.exp.away)  

  txt <- c(border.plus,
           paste0(toupper(team.home.full), " vs ", toupper(team.away.full), " at ",
                  toupper(prediction$ground)),
           border.dash,
           AssemblePredictionTableRow("Team", team.home.full, team.away.full,
                                      col.widths, cell.lead.space),
           border.dash,
           AssemblePredictionTableRow("Win prob.",
                                      sprintf("%9.1f per cent", prediction$result.exp.home * 100),
                                      sprintf("%9.1f per cent", prediction$result.exp.away * 100),
                                      col.widths, cell.lead.space),
           AssemblePredictionTableRow("Margin",
                                      sprintf("%+9.1f points", prediction$margin.exp.home),
                                      sprintf("%+9.1f points", prediction$margin.exp.away),
                                      col.widths, cell.lead.space),
           border.dash,
           AssemblePredictionTableRow("Tip", tip.home, tip.away,
                                      col.widths, cell.lead.space),
           border.dash,
           AssemblePredictionTableRow("Team rating",
                                      sprintf("%9.1f", prediction$rating.home),
                                      sprintf("%9.1f", prediction$rating.away),
                                      col.widths, cell.lead.space),
           AssemblePredictionTableRow("Travel adj.",
                                      sprintf("%+9.1f", prediction$rating.travel.adj.home),
                                      sprintf("%+9.1f", prediction$rating.travel.adj.away),
                                      col.widths, cell.lead.space),
           AssemblePredictionTableRow("Ground adj.",
                                      sprintf("%+9.1f", prediction$rating.ground.adj.home),
                                      sprintf("%+9.1f", prediction$rating.ground.adj.away),
                                      col.widths, cell.lead.space),
           border.dash,
           AssemblePredictionTableRow("Rating tot.",
                                      sprintf("%9.1f", prediction$rating.home.adj),
                                      sprintf("%9.1f", prediction$rating.away.adj),
                                      col.widths, cell.lead.space),
           border.dash,
           AssemblePredictionTableRow("Delta",
                                      sprintf("%+9.1f", prediction$delta.home),
                                      sprintf("%+9.1f", prediction$delta.away),
                                      col.widths, cell.lead.space),
           border.dash,
           "Bookie",
           AssemblePredictionTableRow(paste0(strrep(" ", cell.lead.space), "Back"),
                                      sprintf(">%9.2f", prediction$back.bookie.home),
                                      sprintf(">%9.2f", prediction$back.bookie.away),
                                      col.widths, cell.lead.space),
           sprintf("Exchange (commission of %.2f per cent)", prediction$commission * 100),
           AssemblePredictionTableRow(paste0(strrep(" ", cell.lead.space), "Back"),
                                      sprintf(">%9.2f", prediction$back.exchange.home),
                                      sprintf(">%9.2f", prediction$back.exchange.away),
                                      col.widths, cell.lead.space),
           AssemblePredictionTableRow(paste0(strrep(" ", cell.lead.space), "Lay"),
                                      sprintf("<%9.2f", prediction$lay.exchange.home),
                                      sprintf("<%9.2f", prediction$lay.exchange.away),
                                      col.widths, cell.lead.space),
           border.plus)

  if (con != "quiet") {
    writeLines(txt, con)
  }

  txt
}


AssemblePredictionTableRow <- function(row.id, cont.home, cont.away,
                                       col.widths, cell.lead.space) {
  row.id.filled <- paste0(row.id, strrep(" ", col.widths[1] - nchar(row.id)))
  cont.home.filled <- paste0(strrep(" ", cell.lead.space), cont.home, strrep(" ", col.widths[2] - nchar(cont.home)))
  cont.away.filled <- paste0(strrep(" ", cell.lead.space), cont.away, strrep(" ", col.widths[3] - nchar(cont.away)))
  row.str <- paste0(row.id.filled, "|", cont.home.filled, "|", cont.away.filled)
}


GetDescriptiveTip <- function(result.exp) {
  wep.prob <- result.exp

  if (result.exp < 0.5) {
    win.lose <- "Lose"
    wep.prob <- 1 - result.exp
  } else if (result.exp > 0.5) {
    win.lose <- "Win"
  } else {
    win.lose <- "Draw"
  }

  if (wep.prob > 0.95) {
    wep <- "Almost cert."
  } else if (wep.prob > 0.75) {
    wep <- "Likely"
  } else if (wep.prob > 0.60) {
    wep <- "Probable"
  } else {
    wep <- "Toss-up"
  }

  tip <- paste0(win.lose, ": ", wep)
  tip
}