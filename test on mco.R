require(mco)


game_matrix <- list(matrix(c(-1, 0, -3, -2), 2, 2), 
                    matrix(c(-1, -3, 0, -2), 2, 2))


normalizeStrategies <- function(s) {
    if(!is.numeric(s) || length(s) != 4) stop("Strategies vector form 
                                              is inapropriable!")
    
    sumP1 <- sum(s[1:2])
    sumP2 <- sum(s[3:4])
    
    s[1] <- s[1] / sumP1
    s[2] <- s[2] / sumP1
    
    s[3] <- s[3] / sumP2
    s[4] <- s[4] / sumP2
    
    return(s)
}

choosePureStrategies <- function(s) {
    #if(sum(s) != 2) stop("Strategies are not normilized!")
    
    P1move <- sample(1:2, 1, prob = s[1:2])
    P2move <- sample(3:4, 1, prob = s[3:4])
    
    return(c(P1move, P2move))
}

calcScore <- function(ps) {
    if(!is.numeric(ps) || length(ps) != 2) stop("Pure strategies set 
                                                is inapropriate!")
    
    P1score <- game_matrix[[1]][ps[1], ps[2] - 2]
    P2score <- game_matrix[[2]][ps[1], ps[2] - 2]
    
    return(c(P1score, P2score))
}


totalScore <- function(s) {
    if(!is.numeric(s) || length(s) != 4) stop("Input from alg 
                                              is inapropriate!")
    
    # Initialize score table
    scores <- matrix(numeric(200), nrow = 100, ncol = 2)
    
    # Normilize input vector
    norm_strategies <- normalizeStrategies(s)
    
    for(i in seq(nrow(scores))) {
        # Derive pure strategies
        pure_strategies <- choosePureStrategies(norm_strategies)
        
        # Caclulate score
        scores[i, ] <- calcScore(pure_strategies)
    }
    
    return(scores)
}


# Convert maximizing GT problem to minimizing problem
gtMinProb <- function(x) {
    return(colSums(totalScore(x))*(-1))
}



raw_results <- nsga2(gtMinProb, 4, 2,
      generations = 100,
      popsize = 100,
      lower.bounds = rep(0, 4),
      upper.bounds = rep(1, 4))

# Convert values back to their orginal signs
raw_results$value <- raw_results$value * (-1)


normalized_raw_params <- aperm(apply(raw_results$par, 1, normalizeStrategies))
params_with_values <- cbind(round(normalized_raw_params, 5), 
                            raw_results$value)
results_first_run <- params_with_values[order(params_with_values[, 5] + 
                                                  params_with_values[, 6], 
                                              decreasing = TRUE), ]
results_first_run <- as.data.frame(results_first_run)
colnames(results_first_run) <- c("P1 cooperates", 
                                 "P1 defects", 
                                 "P2 cooperates", 
                                 "P2 defects", 
                                 "P1 SCORE", 
                                 "P2 SCORE")
print(results_first_run)
plot(raw_results)
