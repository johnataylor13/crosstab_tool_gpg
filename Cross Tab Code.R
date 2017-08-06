#GPG Cross Tab App
library(dplyr)
dataset <- read.csv("/Users/johntaylor/Documents/GPG/Cross Tab Tool/School Survey Data.csv") #This will have to be an input for the user to read in the csv of the survey data")

#Pulling out a list of cross tab variables
crosses <- as.list(NULL) #It would probably be best if this loop was done as check boxes on the app
for (i in 1:ncol(dataset)) {
  cross_option <- ifelse(count(unique(dataset[i])) < 5, names(dataset[i]), NA) 
  crosses <- c(crosses, cross_option)
}
crosses <- crosses[!is.na(crosses)]

#Identifying Numeric Variables
numerics <- as.list(NULL)
for (i in 1:ncol(dataset)) {
  numeric_option <-ifelse(sapply(dataset[i], is.numeric), names(dataset[i]), NA)
  numerics <- c(numerics, numeric_option)
}
numerics <- numerics[!is.na(numerics)]

#Non Numeric Variables
non_numerics <- as.list(NULL)
for (i in 1:ncol(dataset)) {
  non_numeric_option <-ifelse(sapply(dataset[i], is.numeric), NA, names(dataset[i]))
  non_numerics <- c(non_numerics, non_numeric_option)
}
non_numerics <- non_numerics[!is.na(non_numerics)]

#Doing crosstabs based on the three lists of data
for (i in 1:length(crosses)) {
  paste("cross_tabs_", as.character(crosses[1], sep="")) <- dataset %>%
  group_by(as.character(crosses[1])) %>%
    summarise(
      for (j in 1: length(non_numerics)) {
        paste(as.character(non_numerics[1]), "_averages" sep="") = mean(as.character(non_numerics[1]))
      }
    )
}
