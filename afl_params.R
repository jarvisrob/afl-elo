# Elo parameters
# elo.params <- 
#   list(
#     rating.mean = 1500,
#     spread = 400,
#     margin = 0.03213133,
#     coeff.rating.update = 76.72256,
#     regress.rating = 0.2038160,
#     coeff.ground.update = 1.675048,
#     coeff.travel = 14.01393,
#     power.travel = 0.2689826,
#     rating.expansion.init = 1330
# )

elo.params <- 
  list(
    rating.mean = 0,
    spread = 0,
    margin = 36.8455546,
    coeff.rating.update = 10.2963909,
    regress.rating = 0.1567977,
    coeff.ground.update = 0.2452136,
    coeff.travel = 0.9507260,
    power.travel = 0.3492311,
    rating.expansion.init = -78.7120968
  )
  

# Scoring parameters
# score.params <- 
#   list(
#     margin.error.sigma = 38.5,  # This should be = 1/elo.params$margin
#     lose.score.mu = 75.2, 
#     lose.score.sigma = 19.2
# )

score.params <- 
  list(
    margin.error.sigma = 36.8455546,
    lose.score.mu = 75.2, 
    lose.score.sigma = 19.2
  )


