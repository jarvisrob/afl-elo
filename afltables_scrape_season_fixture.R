
ScrapeAFLTablesSeasonFixture <- function(season) {
  
  # Requires tidyverse and rvest packages to be loaded
  require(tidyverse)
  require(rvest)
  
  # Scrapes from AFL Tables website
  base.url <- "https://afltables.com/afl/seas/"
  url.extn <- ".html"
  url <- paste0(base.url, season, url.extn)
  
  # Collect raw HTML "soup" that is the webpage and extract the tables that contain fixture content only
  soup <- read_html(url)
  relevant.html.tables <- soup %>% html_nodes("body center > table")
  
  # Blank data frame to fill with fixture
  fixture <- data.frame(rnd = integer(), game = integer(), team.home = character(), team.away = character(), ground = character(), stringsAsFactors = FALSE)
  
  # Outer loop through rounds in season (regular season only)
  rnd <- 1
  repeat {
    
    # Parse out "Round xx" string using CSS selctors, stop when encounter something other than "Round" heading
    rnd.str <- relevant.html.tables %>% .[[2*rnd-1]] %>% html_nodes("b:nth-of-type(1)") %>% html_text() %>% .[1]
    if (rnd.str %>% pmatch("Round", .) %>% is.na()) {
      break
    }
    
    # Inner loop through games in round
    game <- 1
    repeat {
      
      # Parse out game details from table below the "Round xx" header table, stop when empty string returned
      game.str.raw <- relevant.html.tables %>% .[[2*rnd]] %>% html_nodes("table") %>% .[[game]] %>% html_nodes("a") %>% html_text()
      if (game.str.raw %>% is_empty()) {
        break
      }
      
      # Check for byes (string length == 1) and ignore, parse out home/away team and ground, add observation to fixture data frame
      if (length(game.str.raw) > 1) {
        team.home <- game.str.raw[1]
        ground <- game.str.raw[2]
        team.away <- game.str.raw[3]
        game.row <- data.frame(rnd = rnd, game = game, team.home = team.home, team.away = team.away, ground = ground, stringsAsFactors = FALSE)
        fixture <- rbind(fixture, game.row)
      }
      
      # Increment game counter for the round
      game <- game + 1
    }
    
    # Increment round counter for the season
    rnd <- rnd + 1
  }
  
  # Return the scraped fixture for the season
  fixture
}



