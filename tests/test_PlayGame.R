context("Playing Games")

test_that("PlayGame accept just norm results", {
    pop <- InitializePopulation(5, GM$prisoners_dilemma)
    
    expect_warning(PlayGame())
    expect_error(PlayGame(pop[1:3, 4]))
    expect_error(PlayGame(pop[2, 5, 3]))
})

test_that("ChooseDecisions operates properly", {
    pop <- InitializePopulation(1, GM$prisoners_dilemma)
    
    expect_is(ChooseDecisions(pop), "list")
    expect_is(ChooseDecisions(pop)[[1]], "integer")
    expect_is(ChooseDecisions(pop)[[2]], "integer")
})