# Run all the prep stuff first
source("test.R")

# Getting 2019 fixture
fixture.2019 <- ScrapeAFLTablesSeasonFixture(2019)
fixture.2019[108, "team.away"] <- "St Kilda"
fixture.2019[108, "ground"] <- "Riverway Stadium"

# Get current ladder
ladder.data <- ScrapeAflTablesLadder(2019)

# Limit fixture to start at next round, need to edit manually for now
fixture.2019 <- ExtractRemainingSeasonFixture(fixture.2019, 17)


print("--- Test MC many full season sims")

print("Team ratings before starting sim")
team.data.run %>%
  rownames_to_column("team") %>%
  filter(yes.active) %>%
  arrange(desc(rating)) %>%
  dplyr::select(team, rating) %>%
  print()

print(ladder.data)


# Testing MC sims
n.itns <- 5000

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
    score.params$lose.score.sigma,
    ladder.data
  )
toc()
print("Iterations complete")


print("--- Postproc of results")

check.team <- "essendon"

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


# ladder_posn_prob <- 
#   sim.many$ladder.many %>%
#   filter(team == team_of_interest) %>%
#   group_by(ladder.posn) %>%
#   count() %>%
#   mutate(p = n/n_itns)
# 
# ladder_posn_prob %>%
#   ggplot(aes(ladder.posn, p)) + 
#   geom_col() +
#   ggtitle(
#     paste0(
#       "Probability of end ladder position for ", 
#       team_of_interest
#     )
#   ) +
#   xlab("Ladder position") +
#   ylab("Probability")
# 
# tibble(
#   event = c(
#     "Top of ladder (Minor premiers)",
#     "Top 4 (2nd chance in finals)",
#     "Top 8 (Play in finals)"
#     #"Bottom of ladder (Wooden spoon)"
#   ),
#   p = c(
#     ladder_posn_prob %>% filter(ladder.posn == 1) %>% pull(p),
#     ladder_posn_prob %>% filter(ladder.posn <= 4) %>% pull(p) %>% sum(),
#     ladder_posn_prob %>% filter(ladder.posn <= 8) %>% pull(p) %>% sum()
#     ladder_posn_prob %>% filter(ladder.posn == 16) %>% pull(p)
#   )
# )
# 

