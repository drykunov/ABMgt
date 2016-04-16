strat <- matrix(runif(500000), 10000, 5)
strdf <- as.data.frame(strat)

eqstrdf <- t(apply(strdf, 1, function(x) sapply(x, function(y) y/sum(x))))

