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

# Evolutionary Functions --------------------------------------------------

GetFitValues <- function(population, ngames) {
    if (missing(population)) stop("f:GetFitValues - Provide population to calculate fir values for!")
    if (missing(ngames)) stop("f:GetFitValues - Provide number of games to calculate values on!")
        
    fitValues <- DuplicateListStructure(population)
    class(fitValues) %<>% c("population")
    
    cat("Progress:")
    pb = progress_bar$new(total = sum(sapply(population, nrow)), format = "[:bar] :percent :eta", clear = FALSE)
    
    for (i in names(population)) {
        cat("\nCalculating Fit Values for ---", i, "\n")
        fitValues[[i]] <- data.frame(score = numeric(0)) %>% tbl_df()
        
        for (n in seq_len(nrow(population[[i]]))) {  # Iterating through separated strategies
            stratset <- TournamentSelecton(population, i, n, ngames)
            score <- CalculateScoreForSet(stratset)
            fitValues[[i]][n, "score"] <- score
            pb$tick()
        }
    }
    
    return(fitValues)
}

SortToFit <- function(population, fitValues) {
    if (!TestListsEqualSize(population, fitValues)) stop("Population and fitValues have different structure - couldn't sort!")
    nplayers <- length(population)
    
    for (i in seq_len(nplayers)) {
        population[[i]] <- population[[i]][order(fitValues[[i]]$score, decreasing = TRUE), ]
    }
    
    return(population)
}


Mutate <- function(curPop, nMutants) {
    
}




























