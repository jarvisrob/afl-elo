#!/bin/env Rscript

library(
  c("optparse",
    "tictoc",
    "MASS",
    "tidyverse"),
  quietly = TRUE
)

# CLI argument parsing
# 
# - elo param config file
# 
# - verbose
# 
# - download results (big.txt) from afltables (or use same file that's already downloaded)
# - scrape fixure from afltables (and use the next round)
# - use xls fixture at location (best for upcoming season)
# - user-supplied round fixture (plain text file, can't do sims only round pred--useful for finals at this point)
# 
# - write to screen
# - write to files
# 
# - calibration testing results
#   - training data
#   - test data, i.e. test year
# - only evaluate current ratings
# - predict next round
# - sim then predict remainder of season
# - sim then predict finals
# - number of season sim iterations
# 
# - csv (or required format) for squiggle
# 
# - output folder?
# 
# - write ratings summary text output
# - write summary next round prediction text output
# - write summary ladder pmfs for all teams
# - write ladder sim results
# - write finals sim results
# - write time-series ratings results
# 
# - plot/png files
#   - round predictions
#   - pmf for end regular season ladder position
#     - team/teams
#   - summary ladder predictions (top 1, top 4, top 8, wooden spoon)
#   - finals predictions (flag, runner-up, other)
# 
# - start tuning from year (default = 2000)
# - test year (default = last complete season)
# 
# 
option_list <-
  list(
    make_option(
      c("-f", "--file"),
      action = "store",
      type = "character",
      default = NULL,
      help = "input file",
      metavar = "input_file_goes_here"
    ),
    make_option(
      c("-o", "--output"),
      action = "store",
      type = "character",
      default = "out.txt",
      help = "output file",
      metavar = "character"
    ),
    make_option(
      c("-n", "--number"),
      action = "store",
      type = "integer",
      default = 1,
      help = "number of iterations",
      metavar = "integer"
    ),
    make_option(
      c("-d", "--do-true"),
      action = "store_true",
      type = "logical",
      default = FALSE,
      help = "flag to do if true",
      metavar = "logical"
    )
  )

opt <- parse_args(OptionParser(option_list = option_list),  convert_hyphens_to_underscores = TRUE)



