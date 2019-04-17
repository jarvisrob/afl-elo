# Testing MC sims
tic()
sim.many <-
  SimulateFullSeasonEloMany(
    2019, 
    fixture.2019, 
    10,
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

