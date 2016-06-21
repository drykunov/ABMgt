# _Game model computing_
# 
# !! Data structure:
# ! Metadata:
# number of player types
# strategies representation
# game equilibrium set
# 
# ! Data in use:
# Sets of strategies
# Game logic (how to choose outcome from strategies)
# Outcome's payoffs
#     
# !! Functions:    
# Strategies to decisions mechanism (mixed to pure strategies)
# Tournament selection
# population management
#     population update (insert)
#     population erase
# Fitness evaluation
#     for particular population - whole one or strategies from outside
#     * because of iterativeness of the process, a cache needs to be maintained
#     
#     
# !! Input:
# Population to evaluate against
# Strategies to evaluate
# 
# Output:
# Fitness level for particular strategies
# logs
# 


# Functions:
# Subset Population ([ ])
#


library(dplyr)
library(magrittr)

# Games in normal form ----------------------------------------------------

GM <- list()
GM$prisoners_dilemma <-
    list(
        players = c("p1", "p2"),
        outcomes = list(p1 = matrix(c(-1, 0,-3,-2), 2, 2),
                        p2 = matrix(c(-1,-3, 0,-2), 2, 2)),
        strategies = list(
            p1 = c("Cooperate", "Defect"),
            p2 = c("Cooperate", "Defect")
        )
    )

# Variables ----------------------------------------------------

strat.coding.min <- 0
strat.coding.max <- 1
index <- list()

# Functions ----------------------------------------------------


# Utility functions -------------------------------------------------------

`[.population` <- function(x, ...) {
    if (length(x) != length(list(...)))
        stop("Substituting vector's length doesn't match population structure!")
    
    indices <- list(...)
    
    for (i in seq_along(names(x))) {
        x[[i]] <- x[[i]][indices[[i]],]
    }
    
    return(x)
}

InitializePopulation <- function(npop = 0, gm = GM$prisoners_dilemma) {
    pop <- list()
    class(pop) <- c(class(pop), "population")
    
    for (i in seq_along(gm$players)) {
        nstrat <- length(gm$strategies[[i]])
        
        pop[[i]] <-
            matrix(
                data = runif(npop * nstrat, min = strat.coding.min, max = strat.coding.max),
                nrow = npop,
                ncol = nstrat,
                dimnames = list(NULL, gm$strategies[[i]])
            ) %>% as.data.frame() %>% tbl_df()
        
        names(pop)[i] <- gm$players[[i]]
    }
    
    return(pop)
}

NormalizeVector <- function(v) {
    if (!is.vector(v) |
        !is.numeric(v))
        stop("Only numeric vector could be normalized")
    
    
    sum <- sum(v)
    output <- vapply(v, function(x)
        x / sum, FUN.VALUE = numeric(1))
    
    return(output)
}

NormalizePopulation <- function(pop) {
    wrk <- function (x) {
        output <-
            apply(x, 1, NormalizeVector) %>% t() %>% as.data.frame() %>% tbl_df()
        return(output)
    }
    
    out <- lapply(pop, wrk)
    class(out) <- class(pop)
    
    return(out)
}



# Playing Games -----------------------------------------------------------



# Input: a vector with normalized probability to choose each strategy
# Output: a number representing a strategy by its order in game-matrix, counting from upper-right corner
ChoosePureStrategy <- function(x) {
    strat <- sample(seq_along(x), size = 1, prob = x)
    return(strat)
}

ChooseDecisions <- function(strategies.single.set) {
    out <- lapply(strategies.single.set, function(x) ChoosePureStrategy(x[1, ]))
    class(out) %<>% c("decisions")
    return(out)
}

GetGameScores <- function(decisions) {
    scores <- unclass(decisions)
    
    for (i in names(scores)) {
        scores[[i]] <- do.call(`[`, c(list(gm$outcomes[[i]]), decisions))
    }
    
    class(scores) %<>% c("scores")
    
    return(scores)
}

PlayGame <- function(strategies.single.set, times) {

    if (missing(strategies.single.set)) {
        warning("Game not played - strategies set is empty!")
        return()
    } else if (any(rapply(strategies.single.set, length) != 1)) {
        stop("Game not played - strategies set is corrupted!")
    }
    
    decisions <- ChooseDecisions(strategies.single.set)
    
    scores <- GetGameScores(decisions)
    
    return(scores)
}





















