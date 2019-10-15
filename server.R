library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(gridExtra)
function(input, output) {
  bank <-read.csv("./bank-additional/bank-full.csv", sep = ";", quote = '"')
  tennis <- read.csv("./Tennis/FrenchOpen-men-2013.csv", sep = ",", quote = "")
  themetext <- theme( plot.title = element_text(color="red", size=14, face="bold.italic"), axis.title.x = element_text(color="blue", size=14, face="bold"),  axis.title.y = element_text(color="#993333", size=14, face="bold"))

  joueurtable<- eventReactive(input$player,{
    t1<-filter(tennis,Player1==input$player)
    t11<-select(t1,Round,FNL.1,FSP.1:ST5.1)
    t2<-filter(tennis,Player2==input$player)
    t22<-select(t2,Round,FNL.2,FSP.2:ST5.2)
    names(t22)=names(t11)
    t<-bind_rows(t11,t22)
    t[order(t[1]),]
  })

  
  
  output$BankPlot <- renderPlot({
    if (input$row =="Age")  {
      ggplot(datasetInput(),aes(x=age)) +geom_histogram(binwidth=1, colour="black", fill="white")+ ggtitle("Les ages des clients classés par effictifs") + ylab("Effictifs")+ themetext
    }
    else if (input$row =="Familial situation")  {
      ggplot(datasetInput(),aes(x=marital)) +geom_histogram(stat = "count",binwidth=1, colour="black", aes(fill=marital))+ ggtitle("Les situations familiales des clients classés par effictifs") + ylab("Effictifs")+ themetext
    }
    else if (input$row =="Loan")  {
      ggplot(datasetInput(),aes(x=loan)) +geom_histogram(stat = "count",binwidth=1, colour="black", aes(fill=loan))+ ggtitle("Les clients avec des crédits classés par effictifs") + ylab("Effictifs")+ themetext
    }
    else if(input$row =="Job")  {
      ggplot(datasetInput(),aes(x=job)) + geom_bar(stat = "count", colour="black", aes(fill=job),width = 0.5) + coord_flip() +  ggtitle("Les jobs des clients classés par effictifs")  + ylab("Effictifs") + themetext
    }
    else if(input$row =="Education")  {
      ggplot(datasetInput(),aes(x=education)) + geom_bar(stat = "count", colour="black", aes(fill=education),width = 0.5) + coord_flip() +  ggtitle("Les jobs des clients classés par effictifs")  + ylab("Effictifs")+ themetext
    }
    else if(input$combi == "Job")  {
      ggplot(datasetInput()%>% filter(balance <input$bal),aes(x=job ,y=balance ,color=balance)) + geom_point() + coord_flip() + ggtitle("Les salaires des clients classés par jobs") + themetext
    }
    else if(input$combi == "Niveau académique")  {
      ggplot(datasetInput()%>% filter(balance <input$bal),aes(x=education ,y=balance ,color=balance)) + geom_point() + coord_flip() + ggtitle("Les salaires des clients classés par jobs") + themetext
    }
    
    else if(input$combi == "Age")  {
      ggplot(datasetInput()%>% filter(balance <input$bal),aes(x=age ,y=balance, color=age)) + geom_smooth(model = lm) + ggtitle("Les salaires des clients classés par jobs") + themetext
    }

    else if(input$combi == "Situation familiale")  {
      ggplot(datasetInput()%>% filter(balance <input$bal),aes(x=marital ,y=balance,color=balance)) + geom_point() + coord_flip() + ggtitle("Les salaires des clients classés par situation familiale") + themetext
    }
    else if(input$combi == "Si oui ou non le client a pris un crédit")  {
      ggplot(datasetInput()%>% filter(balance <input$bal),aes(x=loan ,y=balance,color=balance)) + geom_point() + coord_flip() + ggtitle("Les salaires des clients classés par situation familiale") + themetext
    }
  })
  output$TennisPlot <- renderPlot(
    if(!is.null(input$aff)){
      ggplot(joueurtable() , aes_string(x="Round", y=parajoueur())   ) + geom_point() + geom_line() + themetext
    }
    )
  
  output$TennisPlot2 <- renderPlot(
    if(!is.null(input$aff2)){
      g1= ggplot(joueurtable2() , aes_string(x="Round", y=parajoueur()) ,size=3  ) + geom_point() + geom_line() + themetext
      g2=ggplot(joueurtable22() , aes_string(x="Round", y=parajoueur())   ) + geom_point() + geom_line() + themetext
      grid.arrange(g1, g2)
    }
  )

  datasetInput <- eventReactive(input$update, {
    switch(input$path,
           "Bank" =read.csv("./bank-additional/bank-full.csv",sep = ";", quote = '"'),
           "Tennis" = read.csv("./Tennis/FrenchOpen-men-2013.csv", sep = ",", quote = ""))
  }, ignoreNULL = FALSE)
  
  joueurtable<- eventReactive(input$player,{
    t1<-filter(tennis,Player1==input$player)
    t11<-select(t1,Round,FNL.1,FSP.1:ST5.1)
    t2<-filter(tennis,Player2==input$player)
    t22<-select(t2,Round,FNL.2,FSP.2:ST5.2)
    names(t22)=c("Round","FNL","FSP","FSW","SSP","SSW","ACE","DBF","WNR","UFE","BPC","BPW","NPA","NPW","TPW","ST1","ST2","ST3","ST4","ST5")
      names(t11)=c("Round","FNL","FSP","FSW","SSP","SSW","ACE","DBF","WNR","UFE","BPC","BPW","NPA","NPW","TPW","ST1","ST2","ST3","ST4","ST5")
    t<-bind_rows(t11,t22)
    t[order(t[1]),]
  })
  joueurtable2<- eventReactive(input$player2,{
    t1<-filter(tennis,Player1==input$player2)
    t11<-select(t1,Round,FNL.1,FSP.1:ST5.1)
    t2<-filter(tennis,Player2==input$player2)
    t22<-select(t2,Round,FNL.2,FSP.2:ST5.2)
    names(t22)=c("Round","FNL","FSP","FSW","SSP","SSW","ACE","DBF","WNR","UFE","BPC","BPW","NPA","NPW","TPW","ST1","ST2","ST3","ST4","ST5")
    names(t11)=c("Round","FNL","FSP","FSW","SSP","SSW","ACE","DBF","WNR","UFE","BPC","BPW","NPA","NPW","TPW","ST1","ST2","ST3","ST4","ST5")
    t<-bind_rows(t11,t22)
    t[order(t[1]),]
  })
  
  joueurtable22<- eventReactive(input$player22,{
    t1<-filter(tennis,Player1==input$player22)
    t11<-select(t1,Round,FNL.1,FSP.1:ST5.1)
    t2<-filter(tennis,Player2==input$player22)
    t22<-select(t2,Round,FNL.2,FSP.2:ST5.2)
    names(t22)=c("Round","FNL","FSP","FSW","SSP","SSW","ACE","DBF","WNR","UFE","BPC","BPW","NPA","NPW","TPW","ST1","ST2","ST3","ST4","ST5")
    names(t11)=c("Round","FNL","FSP","FSW","SSP","SSW","ACE","DBF","WNR","UFE","BPC","BPW","NPA","NPW","TPW","ST1","ST2","ST3","ST4","ST5")
    t<-bind_rows(t11,t22)
    t[order(t[1]),]
  })
  title<-eventReactive(input$update, {
    switch(input$path,
           "Bank" ="Information sur les clients de la bank",
           "Tennis" ="Tournoi FrenchOpen catégorie homme 2013")
  }, ignoreNULL = FALSE)

  parajoueur<-eventReactive(input$aff2,{
    switch (input$aff2,
      "First Serve Percentage"="FSP",
      "First Serve Won"="FSW",
      "Second Serve Percentage"="SSP",
      "Second Serve Won"="SSW",
      "Aces won"="ACE",
      "Double Faults committed"="DBF",
      "Winners earned"="WNR",
      "Unforced Errors committed"="UFE",
      "Break Points Created"="BPC",
      "Break Points Won"="BPW",
      "Net Points Attempted"="NPA",
      "Net Points Won"="NPW",
      "Total Points Won"="TPW",
      "Set 1 result"="ST1",
      "Set 2 Result"="ST2",
      "Set 3 Result"="ST3",
      "Set 4 Result"="ST4",
      "Set 5 Result"="ST5"
    )
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
    if (input$path=="Bank"){
      if (!is.null(input$select2)) {
        columns = input$select2
        }
    }
    if (input$path=="Tennis"){
      if (!is.null(input$select1)) {
        columns = input$select1
      }
    }
    re[,columns,drop=FALSE]
    
  })
  
  
  
}
