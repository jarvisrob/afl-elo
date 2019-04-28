# Getting 2019 fixture
fixture.2019 <- ScrapeAFLTablesSeasonFixture(2019)
fixture.2019[108, "team.away"] <- "St Kilda"
fixture.2019[108, "ground"] <- "Riverway Stadium"

# Testing MC sims
n.itns <- 100
check.team <- "melbourne"

team.ratings <- 
  team.data.run %>%
    rownames_to_column("team") %>%
    filter(yes.active) %>%
    arrange(desc(rating)) %>%
    dplyr::select(team, rating)

print(team.ratings)

print(Sys.time())
print(paste0("Starting MC sims for ", n.itns, " iterations"))
print(paste0("Estimated time = ", n.itns/4/60, " mins"))
tic()
sim.many <-
  SimulateFullSeasonEloMany(
    2019, 
    fixture.2019, 
    n.itns,
    team.data.run, 
    ground.data.run, 
    ground.location,
    team.dictionary,
    travel.distance,
    elo.params,
    rating.time.series.run,
    score.params$margin.error.sigma,
    score.params$lose.score.mu,
    score.params$lose.score.sigma
  )
toc()
print("Iterations complete")


plt <- 
  sim.many$ladder.many %>%
    filter(team == check.team) %>%
    group_by(ladder.posn) %>%
    count() %>%
    mutate(p = n/n.itns) %>%
    ggplot(aes(ladder.posn, p)) + 
      geom_col() +
      ggtitle(
        paste0(
          "Probability of end ladder position for ", 
          team.dictionary.reverse[check.team]
        )
      ) +
      xlab("Ladder position") +
      ylab("Probability")

print(plt)

n.flags <-
  sim.many$finals.many %>%
    filter(rnd == "GF", winner == check.team) %>%
    count() %>%
    pull(n)

p.flag <- n.flags / n.itns
print(
  paste0(
    "Probability of ", 
    team.dictionary.reverse[check.team], 
    " winning the flag = ", 
    p.flag
  )
)
