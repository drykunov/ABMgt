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

# Tests -------------------------------------------------------------------

set.seed(111)

gm <- GM$prisoners_dilemma
pop_in <- InitializePopulation(50, gm)
pop <- NormalizePopulation(pop_in)

# print(pop_norm)
# 
# mutate(pop_norm$p2, Strategy = apply(cbind(Cooperate, Defect), 1, ChoosePureStrategy))



ss <- TournamentSelecton(pop, "p2", index = 6, npairs = 40)
dd <- TournamentSelecton(pop, "p1", index = 7, npairs = 40)


# # Sort strategies to their distance from restrictions
# pop[[1]] %>% mutate(deviation = abs(Cooperate + Defect - 1)) %>% arrange(deviation)

test_dir("tests/")


# 
# pop[[2]][1:8, ] <- matrix(c(rep(c(1,0), 4), rep(c(0,1), 4)), 8, 2, byrow = TRUE)
# 
# fits <- GetFitValues(population = pop, ngames = 50)
# sortedPop <- SortToFit(population = pop, fitValues = fits)