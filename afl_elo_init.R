
InitTeamLDictionary <- function() {
  team.dictionary <- list('Adelaide' = 'adelaide',
                          'Brisbane Bears' = 'brisbane.bears',
                          'Brisbane Lions' = 'brisbane.lions',
                          'Carlton' = 'carlton',
                          'Collingwood' = 'collingwood',
                          'Essendon' = 'essendon',
                          'Fitzroy' = 'fitzroy',
                          'Fremantle' = 'fremantle',
                          'Geelong' = 'geelong',
                          'Gold Coast' = 'gold.coast',
                          'GW Sydney' = 'greater.western.sydney',
                          'Hawthorn' = 'hawthorn',
                          'Melbourne' = 'melbourne',
                          'North Melbourne' = 'north.melbourne',
                          'Kangaroos' = 'north.melbourne',
                          'Port Adelaide' = 'port.adelaide',
                          'Richmond' = 'richmond',
                          'St Kilda' = 'st.kilda',
                          'South Melbourne' = 'sydney',
                          'Sydney' = 'sydney',
                          'University' = 'university',
                          'West Coast' = 'west.coast.eagles',
                          'Footscray' = 'western.bulldogs',
                          'Western Bulldogs' = 'western.bulldogs')
  team.dictionary
}


InitTeamData <- function() {
  team.data <- read.table('afl_init_team_data.txt', header = T, sep = '', skip = 2, strip.white = T, as.is = T)
  team.data
}


InitRatingTimeSeries <- function(all.games, team.data) {
  
  season.start <- paste(unique(format(all.games$date, '%Y')), 'start', sep = ' ')
  season.round <- unique(paste(format(all.games$date, '%Y'), all.games$round, sep = ' '))
  
  # Find all the 'YYYY R1', nchar ==7 is to avoid 'YYYY R1x' (e.g. '2000 R11')h
  is.round.one <- (substr(season.round, start = 6, stop = 7) == 'R1') & (nchar(season.round) == 7)
  idx.season.round <- seq_along(is.round.one)
  idx.round.one <- idx.season.round[is.round.one]
  idx.new <- c(idx.season.round, idx.round.one - 0.5)
  temp.comb <- c(season.round, season.start)
  
  season.round <- temp.comb[order(idx.new)]
  
  
  
  rating.time.series <- data.frame(matrix(nrow = length(season.round), ncol = nrow(team.data)))
  rownames(rating.time.series) <- season.round
  colnames(rating.time.series) <- rownames(team.data)
  rating.time.series
}


InitGroundData <- function(all.games, team.data) {
  ground.location <- InitGroundLocation()
  grounds <- rownames(ground.location)
  teams <- rownames(team.data)
  
  ground.data <- data.frame(matrix(0, nrow = nrow(ground.location), ncol = length(teams)))
  rownames(ground.data) <- grounds
  colnames(ground.data) <- teams
  
  ground.data <- merge(ground.location, ground.data, by = 'row.names')
  rownames(ground.data) <- ground.data$Row.names    # Why capital 'R'?
  ground.data$Row.names <- NULL                     # Why capital 'R'?
  
  ground.data
}


InitGroundPanelRecord <- function(all.games, team.dictionary) {
  team.home.half <- unique(all.games[c("season", "team.home", "ground")])
  colnames(team.home.half) <- c("season", "team.name", "ground")
  team.away.half <- unique(all.games[c("season", "team.away", "ground")])
  colnames(team.away.half) <- c("season", "team.name", "ground")
  unique.combos <- unique(rbind.data.frame(team.home.half, team.away.half))
  unique.combos$team <- unlist(team.dictionary[unique.combos$team.name])
  unique.combos$team.name <- NULL
  unique.combos <- unique.combos[c("team", "ground", "season")]
  n.unique.combos <- nrow(unique.combos)

  ground.panel <- unique.combos[order(unique.combos$team, unique.combos$ground,
                                      unique.combos$season),]

  ground.panel.record <- ground.panel
  ground.panel.record$played <- rep(0, n.unique.combos)
  ground.panel.record$wins <- rep(0, n.unique.combos)
  ground.panel.record$losses <- rep(0, n.unique.combos)
  ground.panel.record$draws <- rep(0, n.unique.combos)

  rownames(ground.panel.record) <- 1:n.unique.combos

  ground.panel.record
}


InitTravelDistance <- function() {
  travel.distance <- read.table('afl_travel_distances.txt', header = T, sep = '', skip = 7, strip.white = T, as.is = T)
  travel.distance
}


InitGroundLocation <- function() {
  ground.location <- read.table('afl_ground_locations.txt', header = T, sep = '', skip = 2, strip.white = T, as.is = T)
  ground.location
}


InitAllGamesElo <- function(all.games) {
  cn <- c("team.home", "team.away",
          "rating.home.before", "rating.ground.adj.home.before", "rating.travel.adj.home", "rating.home.adj.before",
          "rating.away.before", "rating.ground.adj.away.before", "rating.travel.adj.away", "rating.away.adj.before",
          "delta.rating.before",
          "result.exp.home", "result.exp.away", "margin.exp.home",
          "outcome.home",
          "result.act.home", "result.act.away", "margin.act.home",
          "result.error.home", "result.error.away", "margin.error.home",
          "rating.delta.update.home", "rating.home.new", "rating.away.new")
  all.games.elo <- data.frame(matrix(NA, nrow = nrow(all.games), ncol = length(cn)))
  colnames(all.games.elo) <- cn
  
  all.games.elo
}
