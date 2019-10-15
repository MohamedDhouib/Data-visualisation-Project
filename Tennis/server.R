library(shiny)
library(shinydashboard)
library(ggplot2)
function(input, output) {
  output$BankPlot <- renderPlot({
    themetext <- theme( plot.title = element_text(color="red", size=14, face="bold.italic"), axis.title.x = element_text(color="blue", size=14, face="bold"),  axis.title.y = element_text(color="#993333", size=14, face="bold"))
    if (input$row =="Age")  {
      ggplot(datasetInput(),aes(x=age)) +geom_histogram(binwidth=1, colour="black", fill="white")+ ggtitle("Les ages des clients classés par effictifs") + ylab("Effictifs")+ themetext
    }
    else if(input$row =="Job")  {
      ggplot(datasetInput(),aes(x=job)) + geom_bar(stat = "count", colour="black", aes(fill=job),width = 0.5) + coord_flip() +  ggtitle("Les jobs des clients classés par effictifs")  + ylab("Effictifs") + themetext
    }
    else if(input$row =="Education")  {
      ggplot(datasetInput(),aes(x=education)) + geom_bar(stat = "count", colour="black", aes(fill=education),width = 0.5) + coord_flip() +  ggtitle("Les jobs des clients classés par effictifs")  + ylab("Effictifs")+ themetext
    }
    else if(input$combi == "Balance en fonction des jobs")  {
      ggplot(datasetInput(),aes(x=balance ,color=job)) + geom_point(stat = "count") + ggtitle("Les salaires des clients classés par jobs") + ylab("Effictifs")+ themetext
    }
    
    else if(input$combi == "Balance en fonction de l'age")  {
      ggplot(datasetInput(),aes(x=age ,y=balance, color=age)) +geom_histogram( colour="black", fill="white") + ggtitle("Les salaires des clients classés par jobs") + themetext
    }
    else if(input$combi == "Balance en fonction de la situation familiale")  {
      ggplot(datasetInput(),aes(x=balance ,color=marital)) + geom_point(stat = "count") + ggtitle("Les salaires des clients classés par jobs") + ylab("Effictifs")+ themetext
    }
  })
  datasetInput <- eventReactive(input$update, {
    switch(input$path,
           "Bank" =read.csv("./bank-additional/bank-full.csv", header = TRUE,sep = ";", quote = '"'),
           "Tennis" = read.csv("./Tennis/FrenchOpen-men-2013.csv", sep = ",", quote = ""))
  }, ignoreNULL = FALSE)
  
  title<-eventReactive(input$update, {
    switch(input$path,
           "Bank" ="bank-additional",
           "Tennis" ="Tournoi FrenchOpen catégorie homme 2013")
  }, ignoreNULL = FALSE)
  row1<-eventReactive(input$update,{
    switch(input$path,
           "Bank" =c("Aucune","Age","Job","Balance","Education"),
           "Tennis" ="Tournoi FrenchOpen catégorie homme 2013")
  }, ignoreNULL = FALSE)
  row2<-eventReactive(input$update,{
    switch(input$path,
           "Bank" =c("Balance en fonction des jobs", "Balance en fonction de l'age"),
           "Tennis" ="Tournoi FrenchOpen catégorie homme 2013")
  }, ignoreNULL = FALSE)
  output$view <- renderTable({
    head(datasetInput(), n = isolate(input$obs))
  })
  
  output$summary <- renderPrint({
    re <- datasetInput()
    summary(re)
  })
  
  output$taille<-renderPrint({
    re <- datasetInput()
    d<-dim(re)
    c<-c(noquote("Nombre colonnes"),d[2])
    names(c)<-c("Nombre lignes",d[1])
    c
  })
  
  output$title<-renderText({
    title()
  })
  output$datab<-renderPrint({
    datasetInput()
  })
  
  output$check = renderDataTable({
    re <- datasetInput()
    columns = names(re)
    if (!is.null(input$select)) {
      columns = input$select
    }
    re[,columns,drop=FALSE]
  })
  
}
