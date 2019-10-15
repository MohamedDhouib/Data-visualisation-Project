library(shinydashboard)
library(shiny)



bank =read.csv("./bank-additional/bank-full.csv", sep = ";", quote = '"')
tennis = read.csv("./Tennis/FrenchOpen-men-2013.csv", sep = ",", quote = "")
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
                    box(numericInput("obs", "Number of observations:", 10)),
                    actionButton("update", "Update View")
                    ),
                box(h4("La base chargée est:"),
                   textOutput("title", container = span)
                    ),
                box(h4("Les dimensions de la base sont:"),
                    verbatimTextOutput("taille")),
                selectInput("select", "Select columns to display", names(tennis), multiple = TRUE),
                dataTableOutput('check')#,
              #  box(fluidPage(fluidRow(
                          #tableOutput("view")
              #    column(12,
              #    div(tableOutput("view"), style = "overflow-x: auto;overflow-y: auto;"))
              #            )
              #),width = 12)
              )),
      tabItem(tabName = "summary",
              fluidPage(h4("Summary"),
                        verbatimTextOutput("summary", placeholder = TRUE))),
      tabItem(tabName = "visu",
            
             fluidRow(
              box(plotOutput("BankPlot"),width = 12),
              box(title = "Parametres",width = 12,
                  conditionalPanel(
                    condition = "input.path == 'Bank'",
                  selectInput("row","Data en fonction des effectifs",c("Null","Age","Job","Balance","Education"),multiple = FALSE,selected = ""),
              radioButtons("combi", "Interférance entre données : ",
                           c("Clear","Balance en fonction des jobs", "Balance en fonction de l'age", "Balance en fonction de la situation familiale"),selected = "Clear")
              
              )
              )
             )
    )
  )
)
)

