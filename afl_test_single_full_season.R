# Getting 2019 fixture
fixture.2019 <- ScrapeAFLTablesSeasonFixture(2019)
fixture.2019[108, "team.away"] <- "St Kilda"
fixture.2019[108, "ground"] <- "Riverway Stadium"

print("--- Test single full season sim")

print("Team ratings (ranked) at end of 2018 season")
team.ratings <- 
  team.data.run %>%
  rownames_to_column("team") %>%
  filter(yes.active) %>%
  arrange(desc(rating)) %>%
  dplyr::select(team, rating)

print(team.ratings)

sim.data <- 
  SimulateFullSeasonElo(
    2019,
    fixture.2019,
    team.data.run,
    ground.data.run,
    team.dictionary,
    ground.location,
    travel.distance,
    elo.params,
    rating.time.series.run,
    score.params$margin.error.sigma,
    score.params$lose.score.mu,
    score.params$lose.score.sigma
  )

print(sim.data$ladder.data)
print(sim.data$finals)
print(sim.data$team.data)
sim.data$rating.time.series %>% tail(35) %>% View()
