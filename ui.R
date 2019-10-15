library(shinydashboard)
library(shiny)



bank <-read.csv("./bank-additional/bank-full.csv", sep = ";", quote = '"')
tennis <- read.csv("./Tennis/FrenchOpen-men-2013.csv", sep = ",", quote = "")
names <- c("First Serve Percentage","First Serve Won","Second Serve Percentage","Second Serve Won","Aces won","Double Faults committed","Winners earned","Unforced Errors committed","Break Points Created","Break Points Won","Net Points Attempted","Net Points Won","Total Points Won","Set 1 result","Set 2 Result","Set 3 Result","Set 4 Result","Set 5 Result")
dashboardPage(
  dashboardHeader(title="Mini Project"),
  dashboardSidebar(
      sidebarMenu(
        menuItem("Database",tabName="database",icon=icon("database")),
        menuItem("Summary",tabName = "summary",icon = icon("summary")),
        menuItem("Visualisation",tabName = "visu",icon=icon("visu"))
      )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName="database",
              fluidRow(
                box(selectInput("path", "Choose a dataset:",
                                choices = c("Bank", "Tennis")),
                   # box(numericInput("obs", "Number of observations:", 10)),
                    actionButton("update", "Update View")
                    ),
                box(h4("La base chargée est:"),
                   textOutput("title", container = span)
                    ),
                box(h4("Les dimensions de la base sont:"),
                    verbatimTextOutput("taille")),
                box(fluidPage(fluidRow(
                conditionalPanel(
                  condition = "input.path == 'Tennis'",
                selectInput("select1", "Select columns to display", names(tennis), multiple = TRUE)),
                conditionalPanel(
                  condition = "input.path == 'Bank'",
                  selectInput("select2", "Select columns to display", names(bank), multiple = TRUE)),
            #    dataTableOutput('check')
               
                        
                  column(12,
                  div(dataTableOutput('check'), style = "overflow-x: auto;overflow-y: auto;"))
                          )
              ),width = 12)
              )),
      tabItem(tabName = "summary",
              fluidPage(h4("Summary"),
                        verbatimTextOutput("summary", placeholder = TRUE))),
      tabItem(tabName = "visu",
            
             fluidRow(
              
                  conditionalPanel(
                    condition = "input.path == 'Bank'",
                    box(plotOutput("BankPlot"),width = 12),box(title = "Parametres",width = 12,
                  selectInput("row","Data en fonction des effectifs",c("Veuillez choisir un paramètre","Age","Job","Familial situation","Education","Loan"),multiple = FALSE,selected = ""),
              selectInput("combi", "Affichage de la balance du client en fonction de: ",
                           c("Veuillez choisir un paramètre","Job", "Age", "Situation familiale","Niveau académique","Si oui ou non le client a pris un crédit"),multiple = FALSE,selected = ""),
              sliderInput("bal","Choisissez la valeur maximal de balance que vous voulez afficher : ",-8000,300000,300000)
              
                  )),
                  conditionalPanel(
                    condition="input.path == 'Tennis'",
                      fluidRow(box(plotOutput("TennisPlot2"),width = 12),
                               box(
                                 selectInput("player2", "Choisissez un joueur 1: ",  merge(tennis[,1],tennis[,2],all=TRUE), multiple = FALSE,width = 110000),
                                 selectInput("player22", "Choisissez un joueur 2: ",  merge(tennis[,1],tennis[,2],all=TRUE), multiple = FALSE,width = 110000),
                                 selectInput("aff2", "Choisissez un parametre",names,multiple = FALSE,width = 110000),width = 12)
                      )
                      )
                    
                  )
              )
             )
    ))
  


