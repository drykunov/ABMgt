library(magrittr)


# Utility Functions -------------------------------------------------------

TestListsEqualSize <- function(list1, list2) {
    if ((length(list1) != length(list2)) ||
        names(list1) != names(list2))
        return(FALSE)
    
    for (i in seq_len(length(list1))) {
        if (nrow(list1[[1]]) != nrow(list2[[2]]))
            return(FALSE)
    }
    
    return(TRUE)
}

DuplicateListStructure <- function(list.to.duplicate) {
    out <- list()
    length(out) <- length(list.to.duplicate)
    names(out) <- names(list.to.duplicate)
    return(out)
}

GenerateStrategiesSet <-
    function(stratVector, nSets, gameModel = gm) {
        tempPop <- InitializePopulation(npop = 0, gm = gameModel)
        
        subsVector <- vector()
        
        for (i in seq(length(tempPop))) {
            nStratTypes <- ncol(tempPop[[i]])
            
            subsVector <- c(subsVector, rep(i, nStratTypes))
        }
        
        if (length(subsVector) != length(stratVector))
            stop("f:GenerateStratgiesSet subsVector not equal to stratVector!")
        
        for (i in seq(length(tempPop))) {
            stratsToRep <- stratVector[subsVector == i]
            nStratTypes <- ncol(tempPop[[i]])
            
            tempPop[[i]][seq(nSets),] <-
                matrix(rep(stratsToRep, nSets), nSets, nStratTypes, byrow = TRUE)
        }
        
        return(tempPop)
    }

rmutate <- function(n) {
    out <-
        rnorm(n, mean = 0,
              sd = ((strat.coding.max - strat.coding.min) / (3 / mutationMagnitude)
              ))
    return(out)
}

ApplyToEveryStrategy <- function(population, FUN, varname = as.character(substitute(FUN))) {
    out <- DuplicateListStructure(population)
    
    for (i in names(population)) {
        out[[i]] <-
            data.frame(vector(length = nrow(population[[i]]))) %>% tbl_df()
        names(out[[i]]) <- varname
        
        for (n in seq_len(nrow(population[[i]]))) {
            out[[i]][n, varname] <- FUN(unlist(population[[i]][n, ]))
        }
    }
    
    return(out)
}

CorrectForRestrictions <- function(x, min = strat.coding.min, max = strat.coding.max) {
    x[x > max] <- max
    x[x < min] <- min
    return(x)
}

# Evolutionary Functions --------------------------------------------------

GetFitValues <- function(population, ngames) {
    if (missing(population))
        stop("f:GetFitValues - Provide population to calculate fir values for!")
    if (missing(ngames))
        stop("f:GetFitValues - Provide number of games to calculate values on!")
    
    fitValues <- DuplicateListStructure(population)
    class(fitValues) %<>% c("population")
    
    cat("\n\nProgress:")
    pb = progress_bar$new(total = sum(sapply(population, nrow)),
                          format = "[:bar] :percent :eta",
                          clear = FALSE)
    
    for (i in names(population)) {
        cat("\nCalculating Fit Values for ---", i, "\n")
        fitValues[[i]] <-
            data.frame(score = numeric(0)) %>% tbl_df()
        
        for (n in seq_len(nrow(population[[i]]))) {
            # Iterating through separated strategies
            stratset <- TournamentSelecton(population, i, n, ngames)
            score <- CalculateScoreForSet(stratset)
            fitValues[[i]][n, "score"] <- score
            pb$tick()
        }
    }
    
    return(fitValues)
}

SortToFit <- function(population, fitValues, decreasing = TRUE) {
    if(missing(population) || missing(fitValues)) stop("f:SortToFit must be provided with Population and fitValues!")
    if (!TestListsEqualSize(population, fitValues))
        stop("Population and fitValues have different structure - couldn't sort!")
    nplayers <- length(population)
    
    for (i in seq_len(nplayers)) {
        population[[i]] <-
            population[[i]][order(fitValues[[i]][[1]], decreasing = decreasing), ]
    }
    
    return(population)
}


# Mutation ----------------------------------------------------------------

Mutate <- function(curPop, mutationMagnitude = mutationMagnitude) {
    out <- curPop[0]
    
    for(i in names(curPop)) {
        nstratVectors <- nrow(curPop[[i]])
        nstratTypes <- ncol(curPop[[i]])
        
        out[[i]][seq(nstratVectors), ] <- curPop[[i]] + rmutate(nstratVectors*nstratTypes)
    }
    
    out <- rapply(out, CorrectForRestrictions, how = "replace")
    
    return(out)
}

GetDeviationFromRestrictions <- function(strategy, targetSum = strat.coding.max) {
    if (! "numeric" %in% class(strategy)) stop("f:GetDeviationFromRestrictions could operate only on numeric vectors!")
    deviation <- abs(targetSum - sum(strategy))
    return(deviation)
}

GetDevRanks <- function(population) {
    out <- ApplyToEveryStrategy(population, FUN = GetDeviationFromRestrictions, "deviation")
    class(out) %<>% c("population")
    return(out)
}


# Simulation --------------------------------------------------------------

CreateSimulation <- function(generations = 20, nGames = 30, popSize = 60, gameModel = gm) {
    out <- list()
    class(out) %<>% c("simulation")
    out$population <- InitializePopulation(npop = popSize, gm = gameModel) %>% NormalizePopulation()
    out$currentGeneration <- 1
    out$targetGeneration <- generations
    out$nGames <- nGames
    out$popSize <- popSize
    out$gameModel = gameModel
    
    return(out)
}

SubsetStrategiesSet <- function(population, nSets) {
    nPlayers <- length(population)
    args <- replicate(nPlayers, list(nSets))
    population <- do.call(`[`, c(list(population), args))
    return(population)
}

RunSimulation <- function(simulation, iterations = 1) {
    if(! "simulation" %in% class(simulation)) stop("First arg must be of class 'simulation'!")
    remainingGenerations <- simulation$targetGeneration - simulation$currentGeneration
    if(remainingGenerations <= 0) stop("Provided simulation already completed!")
    if(iterations > remainingGenerations) {
        warning(paste("Number of iterations to be runned was contracted from", iterations, "to", remainingGenerations))
        iterations <- remainingGenerations
    }
    
    gm <<- simulation$gameModel
    
    pbSim <- progress_bar$new(total = iterations,
                              format = "Running Simulation: {:curSim OF :total} [[:bar]] :percent :eta",
                              clear = FALSE)
    
    for (i in seq(iterations)) {
        pbSim$tick(0, tokens = list(curSim = i))
        fits <- GetFitValues(simulation$population, simulation$nGames)
        simulation$population <- SortToFit(simulation$population, fits)
        
        # nBestToMutate <- ceiling(mutation_rate * simulation$popSize)
        nDrops <- ceiling(dropout_rate * simulation$popSize)
        
        
        
        mutants <-
            simulation$population %>%
            SubsetStrategiesSet(seq(simulation$popSize - nDrops))
        mutants <- rbind(mutants, mutants)
        mutants <- rbind(mutants, mutants)
        
        mutants <- Mutate(mutants)
        
        ## Those lines cause population in matching_pennies stuck on edges, 
        ## but guarantee convergence to edges if needed.
        # mutantsRanks <- GetDevRanks(mutants)
        # mutants <- SortToFit(mutants, mutantsRanks, decreasing = FALSE)
        
        simulation$population <-
            rbind(
                SubsetStrategiesSet(simulation$population, seq(simulation$popSize - nDrops)),
                SubsetStrategiesSet(mutants, seq(nDrops))
            )
        simulation$population %<>% NormalizePopulation()
        simulation$currentGeneration <- simulation$currentGeneration + 1
        pbSim$tick(1, tokens = list(curSim = i))
    }
    
    
    return(simulation)
}
























