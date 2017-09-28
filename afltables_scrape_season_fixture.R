
ScrapeAFLTablesSeasonFixture <- function(season) {
  
  # Requires tidyverse and rvest packages to be loaded
  
  base.url <- "https://afltables.com/afl/seas/"
  url.extn <- ".html"
  
  url <- paste0(base.url, season, url.extn)
  
  soup <- read_html(url)
  
  relevant.html.tables <- soup %>% html_nodes("body center > table")
  
  fixture <- data.frame(round = integer(), game = integer(), team.home = character(), team.away = character(), ground = character())
  
  rnd <- 1
  repeat {
    
    rnd.str <- relevant.html.tables %>% .[[2*rnd-1]] %>% html_nodes("b:nth-of-type(1)") %>% html_text() %>% .[1]
    if (rnd.str %>% pmatch("Round", .) %>% is.na()) {
      break
    }
    # print(rnd.str)
    
    game <- 1
    repeat {
      game.str.raw <- relevant.html.tables %>% .[[2*rnd]] %>% html_nodes("table") %>% .[[game]] %>% html_nodes("a") %>% html_text()
      if (game.str.raw %>% is_empty()) {
        break
      }
      if (length(game.str.raw) > 1) {
        team.home <- game.str.raw[1]
        ground <- game.str.raw[2]
        team.away <- game.str.raw[3]
        # print(paste0(team.home, " v ", team.away, " at ", ground))
        
        game.row <- data.frame(round = rnd, game = game, team.home = team.home, team.away = team.away, ground = ground)
        # str(game.row)
        fixture <- rbind(fixture, game.row)
      }
      game <- game + 1
    }
    
    rnd <- rnd + 1
  }
  
  fixture
}



