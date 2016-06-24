library(testthat)

source("R/gt_model.R")
source("R/evolutionary_functions.R")


# Variables ----------------------------------------------------

# Defaults
strat.coding.min <- 0
strat.coding.max <- 1

gm <- GM$prisoners_dilemma

popsize <- 50
dropout_rate <- 0.5
mutation_rate <- 0.2
mutation_expansion_rate <- 1.3
crossover_rate <- 0.3

mutationMagnitude <- 1/6
