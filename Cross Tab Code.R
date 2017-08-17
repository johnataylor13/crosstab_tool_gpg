#GPG Cross Tab App
library(dplyr)
library(lazyeval)
library(descr)
library(shiny)
dataset <- read.csv("/Users/johntaylor/Documents/GPG/Cross Tab Tool/School Survey Data.csv") #This will have to be an input for the user to read in the csv of the survey data")
#dataset <- read.csv("/Users/johntaylor/Documents/GPG/Cross Tab Tool/Copy of April baseline survey(1).csv")

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
    crosstab_loop <- compmeans(numeric_loop, cross_loop, plot = F)
    crosstab_loop <- as.data.frame(crosstab_loop)
    (assign(paste(crosses[i], numerics[j], sep="_"), crosstab_loop))
  }
}



#Shiny App with Chi Square Test
library(shiny)
library(shinydashboard)
library(leaflet)
library(data.table)
library(dplyr)
library(MASS)
ui <- fluidPage(titlePanel("GPG CrossTab Tool", windowTitle =  "GPG CrossTab Tool"),
                sidebarPanel(radioButtons("cross", "Cross Variable", choices = as.list(colnames(dataset))),
                             radioButtons("summary", "Summary Variable", choices = as.list(colnames(dataset)))
                ),
                mainPanel(tableOutput("crosstab_results"), tableOutput("chisquare_results"))
)

server <- function(input, output, session) {
  output$crosstab_results <- renderTable({
    numeric_temp <- paste("dataset", input$summary, sep='$')
    numeric <- eval(parse(text=numeric_temp))
    summary_temp <- paste("dataset", input$cross, sep='$')
    summary1 <- eval(parse(text=summary_temp))
    crosstab <- compmeans(numeric, summary1)
    as.data.frame(crosstab)
  }, rownames = T)
  output$chisquare_results <- renderTable({
    numeric_temp <- paste("dataset", input$summary, sep='$')
    numeric <- eval(parse(text=numeric_temp))
    summary_temp <- paste("dataset", input$cross, sep='$')
    summary1 <- eval(parse(text=summary_temp))
    chisquare_test <- chisq.test(summary1, numeric)
    chisquare_results <- as.data.frame(chisquare_test$p.value)
    colnames(chisquare_results) <- "P Value of Pearson Chi Square"
    row.names(chisquare_results) <- as.character(input$cross)
    chisquare_results
  }, rownames = T, digits = 8)
}
shinyApp(ui = ui, server = server)


#Shiny App with Chi Square Test
library(shiny)
library(shinydashboard)
library(leaflet)
library(data.table)
library(dplyr)
library(MASS)
ui <- fluidPage(titlePanel("GPG CrossTab Tool", windowTitle =  "GPG CrossTab Tool"),
                sidebarPanel(fileInput("in_file", "Input file:",
                                       accept=c("txt/csv", "text/comma-separated-values,text/plain", ".csv")),
                             radioButtons("cross", "Cross Variable", choices = as.list(colnames(input$in_file))),
                             radioButtons("summary", "Summary Variable", choices = as.list(colnames(input$in_file)))),
                mainPanel(tableOutput("crosstab_results"), tableOutput("chisquare_results")))
server <- function(input, output, session) {
  output$crosstab_results <- renderTable({
    numeric_temp <- paste("dataset", input$summary, sep='$')
    numeric <- eval(parse(text=numeric_temp))
    summary_temp <- paste("dataset", input$cross, sep='$')
    summary1 <- eval(parse(text=summary_temp))
    crosstab <- compmeans(numeric, summary1)
    as.data.frame(crosstab)
  }, rownames = T)
  output$chisquare_results <- renderTable({
    numeric_temp <- paste("dataset", input$summary, sep='$')
    numeric <- eval(parse(text=numeric_temp))
    summary_temp <- paste("dataset", input$cross, sep='$')
    summary1 <- eval(parse(text=summary_temp))
    chisquare_test <- chisq.test(summary1, numeric)
    chisquare_results <- as.data.frame(chisquare_test$p.value)
    colnames(chisquare_results) <- "P Value of Pearson Chi Square"
    row.names(chisquare_results) <- as.character(input$cross)
    chisquare_results
  }, rownames = T, digits = 8)
}
shinyApp(ui = ui, server = server)



#Dynamic Shiny App with Chi Square Test
ui <- shinyUI(fluidPage(
  titlePanel("GPG Cross Tab Tool"),
    sidebarPanel(
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      tags$hr(),
      uiOutput("crosses"),
      uiOutput("summary"),
      tags$hr()
    ),
    mainPanel(
      tableOutput("crosstab_results"), tableOutput("chisquare_results")
  )
))
server <- shinyServer(function(input, output) {
  filedata <- reactive({
    infile <- input$file1
    if (is.null(infile)){
      return(NULL)      
    }
    read.csv(infile$datapath)
  })
  output$crosses <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("crosses","Select ONE cross variable from:",items)
  })
  output$summary <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("summary","Select ONE summary variable from:",items)
  })
  output$crosstab_results <- renderTable({
    df <- filedata()
    numeric_temp <- paste("df", input$summary, sep='$')
    numeric <- eval(parse(text=numeric_temp))
    summary_temp <- paste("df", input$crosses, sep='$')
    summary1 <- eval(parse(text=summary_temp))
    crosstab <- compmeans(numeric, summary1)
    as.data.frame(crosstab)
  }, rownames = T)
  output$chisquare_results <- renderTable({
    df <- filedata()
    numeric_temp <- paste("df", input$summary, sep='$')
    numeric <- eval(parse(text=numeric_temp))
    summary_temp <- paste("df", input$crosses, sep='$')
    summary1 <- eval(parse(text=summary_temp))
    chisquare_test <- chisq.test(summary1, numeric)
    chisquare_results <- as.data.frame(chisquare_test$p.value)
    colnames(chisquare_results) <- "P Value of Pearson Chi Square"
    row.names(chisquare_results) <- as.character(input$crosses)
    chisquare_results
  }, rownames = T, digits = 8)
})
shinyApp(ui = ui, server = server)




#Dynamic Shiny App with Chi Square Test and Error Message Removed
ui <- shinyUI(fluidPage(
  titlePanel("GPG Cross Tab Tool"),
  sidebarPanel(
    fileInput('file1', 'Choose CSV File',
              accept=c('text/csv', 
                       'text/comma-separated-values,text/plain', 
                       '.csv')),
    tags$hr(),
    uiOutput("crosses"),
    uiOutput("summary"),
    tags$hr()
  ),
  mainPanel(
    tableOutput("crosstab_results"), tableOutput("chisquare_results")
  )
))
server <- shinyServer(function(input, output) {
  filedata <- reactive({
    validate(need(input$file1 != "", "Please select a data set")
             )
    infile <- input$file1
    if (is.null(infile)){
      return(NULL)      
    }
    read.csv(infile$datapath)
  })
  output$crosses <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("crosses","Select ONE cross variable from:",items)
  })
  output$summary <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("summary","Select ONE summary variable from:",items)
  })
  output$crosstab_results <- renderTable({
    df <- filedata()
    numeric_temp <- paste("df", input$summary, sep='$')
    numeric <- eval(parse(text=numeric_temp))
    summary_temp <- paste("df", input$crosses, sep='$')
    summary1 <- eval(parse(text=summary_temp))
    crosstab <- compmeans(numeric, summary1)
    as.data.frame(crosstab)
  }, rownames = T)
  output$chisquare_results <- renderTable({
    df <- filedata()
    numeric_temp <- paste("df", input$summary, sep='$')
    numeric <- eval(parse(text=numeric_temp))
    summary_temp <- paste("df", input$crosses, sep='$')
    summary1 <- eval(parse(text=summary_temp))
    chisquare_test <- chisq.test(summary1, numeric)
    chisquare_results <- as.data.frame(chisquare_test$p.value)
    colnames(chisquare_results) <- "P Value of Pearson Chi Square"
    row.names(chisquare_results) <- as.character(input$crosses)
    chisquare_results
  }, rownames = T, digits = 8)
})
shinyApp(ui = ui, server = server)








#Dynamic Shiny App with Chi Square Test and Error Message Removed and level select for variables...
ui <- shinyUI(fluidPage(
  titlePanel("GPG Cross Tab Tool"),
  sidebarPanel(
    fileInput('file1', 'Choose CSV File',
              accept=c('text/csv', 
                       'text/comma-separated-values,text/plain', 
                       '.csv')),
    tags$hr(),
    uiOutput("crosses"),
    uiOutput("summary"),
    uiOutput("crosses_selected"),
    uiOutput("summary_selected"),
    tags$hr()
  ),
  mainPanel(
    tableOutput("crosstab_results"), tableOutput("chisquare_results")
  )
))
server <- shinyServer(function(input, output) {
  filedata <- reactive({
    validate(need(input$file1 != "", "Please select a data set")
    )
    infile <- input$file1
    if (is.null(infile)){
      return(NULL)      
    }
    read.csv(infile$datapath)
  })
  output$crosses <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("crosses","Select ONE cross variable from:",items)
  })
  output$summary <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("summary","Select ONE summary variable from:",items)
  })
  output$crosses_selected <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    crosses_selected_temp <- paste("df", input$crosses, sep='$')
    crosses_selected_temp2 <- eval(parse(text=crosses_selected_temp))
    items <- as.list(levels(crosses_selected_temp2))
    #names(items) <- items
    checkboxGroupInput("crosses_selected","Select the groups you want to test", items)
  })
  output$crosstab_results <- renderTable({
    df <- filedata()
    numeric_temp <- paste("df", input$summary, sep='$')
    numeric <- eval(parse(text=numeric_temp))
    summary_temp <- paste("df", input$crosses, sep='$')
    summary1 <- eval(parse(text=summary_temp))
    crosstab <- compmeans(numeric, summary1) #ISSUE: We don't really want summary1 to be the filter, but rather in input subcategory "crosses_selected"
    as.data.frame(crosstab)
  }, rownames = T)
  output$chisquare_results <- renderTable({
    df <- filedata()
    numeric_temp <- paste("df", input$summary, sep='$')
    numeric <- eval(parse(text=numeric_temp))
    summary_temp <- paste("df", input$crosses, sep='$')
    summary1 <- eval(parse(text=summary_temp))
    chisquare_test <- chisq.test(summary1, numeric) #ISSUE: We don't really want summary1 to be the filter, but rather in input subcategory "crosses_selected"
    chisquare_results <- as.data.frame(chisquare_test$p.value)
    colnames(chisquare_results) <- "P Value of Pearson Chi Square" 
    row.names(chisquare_results) <- as.character(input$crosses)
    chisquare_results
  }, rownames = T, digits = 8)
})
shinyApp(ui = ui, server = server)


