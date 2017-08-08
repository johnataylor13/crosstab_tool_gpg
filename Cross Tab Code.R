#GPG Cross Tab App
library(dplyr)
library(lazyeval)
library(descr)
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


#Trying to do this a different way
#Doing a successful first crosstab
compmeans(dataset$like, dataset$PrivPub)

#Doinga  crosstab with variable references
x <- paste("dataset", crosses[1], sep='$')
cross_loop1 <- eval(parse(text = x))
compmeans(dataset$like, cross_loop1)

#Doing it with indexing on the outer loop
for (i in 1:length(crosses)) { 
  cross <- paste("dataset", crosses[i], sep="$")
  cross_loop <- eval(parse(text = cross))
  crosstab_loop <- compmeans(dataset$like, cross_loop)
  crosstab_loop <- as.data.frame(crosstab_loop)
  (assign(paste(crosses[i], "likes", sep="_"), crosstab_loop))
}

#Doing it with indexing on the outer and inner loop
for (i in 1:length(crosses)) { 
  cross <- paste("dataset", crosses[i], sep="$")
  cross_loop <- eval(parse(text = cross))
  for (j in 1:length(numerics)) {
    numeric <- paste("dataset", numerics[j], sep='$')
    numeric_loop <- eval(parse(text=numeric))
    crosstab_loop <- compmeans(numeric_loop, cross_loop)
    crosstab_loop <- as.data.frame(crosstab_loop)
    (assign(paste(crosses[i], numerics[j], sep="_"), crosstab_loop))
  }
}

