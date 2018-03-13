
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
                          "Gold Coast Suns" = "gold.coast",
                          'GW Sydney' = 'greater.western.sydney',
                          "GWS Giants" = "greater.western.sydney",
                          "Greater Western Sydney" = "greater.western.sydney",
                          'Hawthorn' = 'hawthorn',
                          'Melbourne' = 'melbourne',
                          'North Melbourne' = 'north.melbourne',
                          'Kangaroos' = 'north.melbourne',
                          'Port Adelaide' = 'port.adelaide',
                          'Richmond' = 'richmond',
                          'St Kilda' = 'st.kilda',
                          'South Melbourne' = 'sydney',
                          'Sydney' = 'sydney',
                          "Sydney Swans" = "sydney",
                          'University' = 'university',
                          'West Coast' = 'west.coast.eagles',
                          "West Coast Eagles" = "west.coast.eagles",
                          'Footscray' = 'western.bulldogs',
                          'Western Bulldogs' = 'western.bulldogs')
  team.dictionary
}

InitTeamDictionaryReverse <- function() {
  team.dictionary.reverse <- list("adelaide" = "Adelaide Crows",
                                  "brisbane.bears" = "Brisbane Bears",
                                  "brisbane.lions" = "Brisbane Lions",
                                  "carlton" = "Carlton",
                                  "collingwood" = "Collingwood",
                                  "essendon" = "Essendon",
                                  "fitzroy" = "Fitzroy",
                                  "fremantle" = "Fremantle",
                                  "geelong" = "Geelong Cats",
                                  "gold.coast" = "Gold Coast Suns",
                                  "greater.western.sydney" = "GWS Giants",
                                  "hawthorn" = "Hawthorn",
                                  "melbourne" = "Melbourne",
                                  "north.melbourne" = "North Melbourne",
                                  "port.adelaide" = "Port Adelaide",
                                  "richmond" = "Richmond",
                                  "st.kilda" = "St Kilda",
                                  "sydney" = "Sydney Swans",
                                  "university" = "University",
                                  "west.coast.eagles" = "West Coast Eagles",
                                  "western.bulldogs" = "Western Bulldogs")
  team.dictionary.reverse
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
  cn <- c("team.home", "team.away", "ground",
          "rating.home", "rating.ground.adj.home", "rating.travel.adj.home", "rating.adj.home",
          "rating.away", "rating.ground.adj.away", "rating.travel.adj.away", "rating.adj.away",
          "delta.rating.home", "delta.rating.away",
          "result.exp.home", "result.exp.away",
          "margin.exp.home", "margin.exp.away",
          "outcome.home", "outcome.away",
          "brier.game", "log.score.game",
          "result.act.home", "result.act.away",
          "margin.act.home", "margin.act.away",
          "result.error.home", "result.error.away",
          "margin.error.home", "margin.error.away",
          "rating.delta.update.home", "rating.delta.update.away",
          "new.rating.home", "new.rating.away")
  all.games.elo <- data.frame(matrix(NA, nrow = nrow(all.games), ncol = length(cn)))
  colnames(all.games.elo) <- cn
  
  all.games.elo
}
