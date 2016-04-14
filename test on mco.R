require(mco)


pris_dilemma <- list(matrix(c(-1, 0, -3, -2), 2, 2), matrix(c(-1, -3, 0, -2), 2, 2))


normalizeStrategies <- function(s) {
    if(!is.numeric(s) || length(s) != 4) stop("Strategies vector form is inapropriable!")
    
    sumP1 <- sum(s[1:2])
    sumP2 <- sum(s[3:4])
    
    s[1] <- s[1] / sumP1
    s[2] <- s[2] / sumP1
    
    s[3] <- s[3] / sumP2
    s[4] <- s[4] / sumP2
    
    return(s)
}

chooseS <- function(s) {
    #if(sum(s) != 2) stop("Startegies are not normilized!")
    
    P1move <- sample(1:2, 1, prob = s[1:2])
    P2move <- sample(3:4, 1, prob = s[3:4])
    
    return(c(P1move, P2move))
}

calcScore <- function(ps) {
    if(!is.numeric(ps) || length(ps) != 2) stop("Pure strategies set is inapropriate!")
    
    P1score <- game_matrix[[1]][ps[1], ps[2] - 2]
    P2score <- game_matrix[[2]][ps[1], ps[2] - 2]
    
    return(c(P1score, P2score))
}


totalScore <- function(s) {
    if(!is.numeric(s) || length(s) != 4) stop("Input from alg is inapropriate!")
    
    # Initialize score table
    scores <- matrix(numeric(200), nrow = 100, ncol = 2)
    
    # Normilize input vector
    norm_strategies <- normalizeStrategies(s)
    
    for(i in seq(nrow(scores))) {
        # Derive pure strategies
        pure_strategies <- chooseS(norm_strategies)
        
        # Caclulate score
        scores[i, ] <- calcScore(pure_strategies)
    }
    
    return(scores)
}

gtMinProb <- function(x) {
    return(colSums(totalScore(x))*(-1))
}



r1 <- nsga2(gtMinProb, 4, 2,
      generations = 100,
      popsize = 100,
      lower.bounds = rep(0, 4),
      upper.bounds = rep(1, 4))


norm_r1 <- aperm(apply(r1$par, 1, normalizeStartegies))
tr1 <- cbind(round(norm_r1, 5), r1$value)
results_first_run <- tr1[order(tr1[, 5] + tr1[, 6]), ]
print(results_first_run)
plot(r1)
