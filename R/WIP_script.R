source("R/gt_model.R")

set.seed(111)

gm <- GM$prisoners_dilemma
pop <- InitializePopulation(100, gm)
pop_norm <- NormalizePopulation(pop)

# print(pop_norm)
# 
# mutate(pop_norm$p2, Strategy = apply(cbind(Cooperate, Defect), 1, ChoosePureStrategy))



# # Sort strategies to their distance from restrictions
# pop[[1]] %>% mutate(deviation = abs(Cooperate + Defect - 1)) %>% arrange(deviation)

test_dir("tests/")