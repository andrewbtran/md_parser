library(readr)

list <- read_csv("data/Maryland_Real_Property_Assessments__Hidden_Property_Owner_Names.csv")
colnames(list) <- make.names(colnames(list))

ham <- subset(list, grepl("3342", MDP.Street.Address..MDP.Field..ADDRESS.))

prin <- subset(list, grepl("4637", MDP.Street.Address..MDP.Field..ADDRESS.))
