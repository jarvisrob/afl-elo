
ExtractRoundFixture <- function(fixture.season, target.rnd) {
  fixture.round <-
    fixture.season %>%
      filter(rnd == target.rnd) %>%
      arrange(game)
}


ExtractRemainingSeasonFixture <- function(fixture.season, from.rnd.inclusive) {
  fixture.rem.season <-
    fixture.season %>%
    filter(rnd >= from.rnd.inclusive) %>%
    arrange(rnd, game)
}

