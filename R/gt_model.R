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
library(progress)

# Games in normal form ----------------------------------------------------

GM <- list()
GM$prisoners_dilemma <-
    list(
        players = c("p1", "p2"),
        outcomes = list(p1 = matrix(c(-1, 0,-3,-2), 2, 2, byrow = TRUE),
                        p2 = matrix(c(-1,-3, 0,-2), 2, 2, byrow = TRUE)),
        strategies = list(
            p1 = c("Cooperate", "Defect"),
            p2 = c("Cooperate", "Defect")
        )
    )

# Variables ----------------------------------------------------

# Defaults
strat.coding.min <- 0
strat.coding.max <- 1
gm <- GM$prisoners_dilemma

# Utility functions -------------------------------------------------------

`[.population` <- function(x, ...) {
    if (length(x) != length(list(...)))
        stop("Subsetting index length doesn't match population structure!")
    
    indices <- list(...)
    
    for (i in seq_along(names(x))) {
        x[[i]] <- x[[i]][indices[[i]],]
    }
    
    return(x)
}

`[<-.population` <- function(x, ..., value) {
    if (length(x) != length(list(...)))
        stop("Subsetting index vector's length doesn't match population structure!")
    
    indices <- list(...)
    
    for (i in seq_along(names(x))) {
        x[[i]][indices[[i]], ] <- value[[i]]
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

dot_every <- function(f, n) {
    i <- 1
    function(...) {
        if (i %% n == 0) cat(".")
        i <<- i + 1
        f(...)
    }
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

PlayGame <- function(strategies.single.set) {

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

TournamentSelecton <- function(pop, playerType, index, npairs) {
    if ((playerType %in% names(pop)) == FALSE) stop("Wrong type for the strategy!")
        if (missing(pop))
            stop("You should specify population to select from!")
        if (missing(playerType))
            stop("You should specify type of a strategy to start selection!")
        if (missing(index))
            stop("You should provide exact strategy to start selection!")
    
    strats.to.select.from <- names(pop)[! names(pop) %in% playerType]
    
    strategies.set <- list()
    length(strategies.set) <- length(pop)
    names(strategies.set) <- names(pop)
    
    strategies.set[[playerType]] <- pop[[playerType]][0, ]
    strategies.set[[playerType]][seq_len(npairs), ] <- pop[[playerType]][index, ]
    
    
    for (i in strats.to.select.from) {
        if(length(pop[[i]]) < npairs) {
            instances <-  sample.int(nrow(pop[[i]]), size = npairs, replace = TRUE)
        } else {
            instances <-  sample.int(nrow(pop[[i]]), size = npairs, replace = FALSE)
        }
        
        strategies.set[[i]] <- pop[[i]][instances, ]
    }
    
    class(strategies.set) %<>% c("strategies.set", "population")
    attr(strategies.set, which = "playerType") <- playerType
    return(strategies.set)
}


CalculateScoreForSet <- function(strategies.set) {
    npairs <- nrow(strategies.set[[1]])
    nplayers <- length(strategies.set)
    
    score <- 0
    for (i in seq_len(npairs)) {
        args <- replicate(nplayers, list(i))
        strategies.single.set <- do.call(`[`, c(list(strategies.set), args))
        score <- score + PlayGame(strategies.single.set)[[attr(strategies.set, which = "type")]]
    }
    
    return(score)
}












