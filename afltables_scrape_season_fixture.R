
ScrapeAFLTablesSeasonFixture <- function(season) {
  
  # Requires tidyverse and rvest packages to be loaded
  
  base.url <- "https://afltables.com/afl/seas/"
  url.extn <- ".html"
  
  url <- paste0(base.url, season, url.extn)
  
  soup <- read_html(url)
  
  relevant.html.tables <- soup %>% html_nodes("body center > table")
  
  relevant.html.tables %>% .[[2*24-1]] %>% html_nodes("b:nth-of-type(1)") %>% html_text() %>% .[1]
  relevant.html.tables %>% .[[2*23]] %>% html_nodes("table") %>% .[[1]] %>% html_nodes("a") %>% html_text()
  
  loop.done <- FALSE
  rnd <- 1
  repeat {
    rnd.str <- relevant.html.tables %>% .[[2*rnd-1]] %>% html_nodes("b:nth-of-type(1)") %>% html_text() %>% .[1]
    if (rnd.str %>% pmatch("Round", .) %>% is.na()) {
      break
    }
    print(rnd.str)
    game <- 1
    repeat {
      game.str.raw <- relevant.html.tables %>% .[[2*rnd]] %>% html_nodes("table") %>% .[[game]] %>% html_nodes("a") %>% html_text()
      if (game.str.raw %>% is_empty()) {
        break
      }
      home.team <- game.str.raw[1]
      ground <- game.str.raw[2]
      away.team <- game.str.raw[3]
      print(paste0(home.team, " v ", away.team, " at ", ground))
      game <- game + 1
    }
    rnd <- rnd + 1
  }
  
  
}



