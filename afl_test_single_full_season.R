# Run all the prep stuff first
source("test.R")

# Getting 2019 fixture
fixture.2019 <- ScrapeAFLTablesSeasonFixture(2019)
fixture.2019[108, "team.away"] <- "St Kilda"
fixture.2019[108, "ground"] <- "Riverway Stadium"

# Get current ladder
ladder.data <- ScrapeAflTablesLadder(2019)

# Limit fixture to start at next round, need to edit manually for now
fixture.2019 <- ExtractRemainingSeasonFixture(fixture.2019, 7)

print("--- Test single full season sim")

print("Team ratings before starting sim")
team.data.run %>%
  rownames_to_column("team") %>%
  filter(yes.active) %>%
  arrange(desc(rating)) %>%
  dplyr::select(team, rating) %>%
  print()

print(ladder.data)

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
    score.params$lose.score.sigma,
    ladder.data
  )

print("--- Sim results")
print(sim.data$ladder.data)
print(sim.data$finals)
sim.data$team.data %>%
  rownames_to_column("team") %>%
  filter(yes.active) %>%
  arrange(desc(rating)) %>%
  dplyr::select(team, rating) %>%
  print()
sim.data$rating.time.series %>% tail(35) %>% View()
