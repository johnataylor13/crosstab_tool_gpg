#GPG Cross Tab App
library(dplyr)
library(lazyeval)
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
#for (i in 1:length(crosses)) {
  assign(paste("cross_tabs", as.character(crosses[2])), dataset) %>%
    group_by_(as.character(crosses[i])) %>%
    summarise(
      for (j in 1: length(numerics)) {
      assign(as.character(numerics[1]), mean(get(as.character(numerics[1]))))
     }
    )
#}

#ideal
test <-dataset %>%
  group_by(PrivPub) %>%
  summarise(mean_ex = mean(school))
  
#successful group_by
test1 <- dataset %>%
  group_by_(as.character(crosses[2])) %>%
  summarise(mean_ex = mean(school))

#successful summarise
test2 <- dataset %>%
  group_by_(as.character(crosses[2])) %>%
  summarise_(test_avg = interp(~mean(var), var = as.name(as.character(numerics[1]))))

#Successful Inner loop
test3 <- dataset %>%
  group_by_(as.character(crosses[2])) %>%
  summarise_(for (j in 1:length(numerics)) {
    assign(as.character(numerics[j]), interp(~mean(var, var = as.name(as.charcter(numerics[j])))))
  }
  )



#Successful Inner loop
test3 <- dataset %>%
  group_by_(as.character(crosses[2])) %>%
  summarise_(for (j in 1:length(numerics)) {
    assign(as.character(numerics[j]), interp(~mean(var, var = as.name(as.charcter(numerics[j])))))
  }
  )








#Doing crosstabs based on the three lists of data
for (i in 1:length(crosses)) {
  assign(paste("cross_tabs", as.character(crosses[2])), dataset) %>%
  group_by_(as.character(crosses[2])) %>%
  summarise(
    for (j in 1: length(numerics)) {
      assign(as.character(numerics[j]), interp(~mean(var), var = as.name(as.character(numerics[j]))))
    }
  )
}


test3 <- dataset %>%
  group_by_(as.character(crosses[2])) %>%
  summarise_(
    for (j in 1: length(numerics)) {
      assign(as.character(numerics[j]), interp(~mean(var), var = as.name(as.character(numerics[j]))))
    }
  )








