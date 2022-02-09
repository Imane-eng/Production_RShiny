library(shiny)
library(dplyr)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggstream)
library(reshape2)
data1 <-  read.csv("data1.csv", sep = ";", encoding = "UTF-8")
head(data1)
data1 <-rename(data1 , "Region" = "Région", "Filiere.de.production" = "Filière.de.production" ,"Courbe.Moyenne.n1..Wh." = "Courbe.Moyenne.n.1..Wh." ,   "Total.Energie.injecte..Wh."= "Total.énergie.injectée..Wh." )
data1$Horodate <- as.POSIXct(data1$Horodate, tz="UTC", "%Y-%m-%dT%H:%M:%OS")
data1 <- na.omit(data1)
data1$Total.Energie.injecte..Wh. <- data1$Total.Energie.injecte..Wh./1000000
CT_prod <-c("Total.Energie.injecte..Wh.","Nb.points.injection","Courbe.Moyenne.n1..Wh.")
data_11_verticale <- melt(data1, id.vars = c("Horodate", "Region", "Plage.de.puissance.injection", "Filiere.de.production"), measure.vars = CT_prod)

shinyApp(
  ui = dashboardPage(
    dashboardHeader(
      title = "Analyse des productions regionales au pas demi horaire",
      titleWidth = 700
    ),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Production", tabName = "vue", icon = icon("dashboard")),
        menuItem("Données", icon = icon("database"), href = "https://data.enedis.fr/explore/dataset/prod-region/table/")
      )
    ),
    dashboardBody( 
      tabItem("production",align="center",
              column(width = 6,align="center",
                     box(
                       title = "Filtres",
                       em("Deux axes d'analyse sont disponibles (par profil ou plage de puissance) sélectionnables par un bouton."),
                       status = "info",
                       solidHeader = TRUE,
                       width = 10,align="center",
                       
                       column(10,align="center",style=list("padding-right: 3px;"),
                              selectInput(inputId = "cat_prod", label = " ", choices= unique(data_11_verticale$variable))),
                       
                       column(10,align="center",style=list("padding-right: 3px;"),          
                              selectInput(inputId = "nat_reg_prod", label = "Sélectionner le national ou une région", choices= c(unique(data_11_verticale$Region), "National"), selected ='Auvergne-Rhône-Alpes' )),
                       
                       column(5,align="center",style=list("padding-right: 3px;"),           
                              selectInput(inputId = "filiere_prod", label = "Filiere", choices= c(unique(data_11_verticale$Filiere.de.production), "Toutes les filieres" ), selected = "Toutes les filieres" )),
                       
                       column(5,align="center",style=list("padding-right: 3px;"),
                              selectInput(inputId = "plage_puissance_prod", label = "Plage de puissance d'injection", choices= unique(data_11_verticale$Plage.de.puissance.injection))),
                       
                       column(10,align="center",style=list("padding-right: 3px;"),
                              dateRangeInput(
                                inputId = "dates_prod",
                                label = "Période",
                                start = "2021-12-23",
                                end = "2021-12-31")),
                       column(10,align="center",style=list("padding-right: 3px;"),   
                              radioButtons(inputId = "pas_prod", label = "Pas", choices = list("pas demi horaire", "pas quotidien"), selected = "pas demi horaire"))
                     )),
              column(width = 6,
                     
                     box(
                       title = "Evolution de la production",
                       status = "info",
                       solidHeader = TRUE,
                       width = 10,
                       plotOutput('prod')
                     ),  valueBox(value=textOutput("consprod"), subtitle = "La somme de la quantite produite "), downloadButton("Download2", "Telecharger")
              )     
              
              
      )
    ),
    title = "Texas Housing",
    skin = "blue"
  ),
  server = function(input, output) {
    
    construit_prod <- reactive({
      
      new_data2 <-  data_11_verticale  %>% filter(variable %in% input$cat_prod, Plage.de.puissance.injection == input$plage_puissance_prod,
                                                  Horodate <= as.POSIXct(input$dates_prod[2]) &
                                                    Horodate >= as.POSIXct(input$dates_prod[1]) ) 
      if (input$nat_reg_prod != "National")
      {
        new_data2 <- new_data2 %>% filter(Region == input$nat_reg_prod )
      }
      if (input$filiere_prod != "Toutes les filieres")
      {
        new_data2 <- new_data2 %>% filter(Filiere.de.production == input$filiere_prod )
      }
      if(input$pas_prod != 'pas demi horaire')
      {
        new_data2$Horodate  <- date(new_data2$Horodate)
      }
      
      ## agregation commune a tous
      new_data_grouped2 <- new_data2 %>% 
        group_by(Horodate, Filiere.de.production) %>%
        summarise(value = sum(value)) %>%
        ungroup()
      
      new_data_grouped2
    })
    plot_dt_prod <- reactive({
      ggplot(data= construit_prod ()) +
        aes(x=as.POSIXct(Horodate), y = value , fill = Filiere.de.production) +
        geom_stream(type = "ridge") +
        theme(plot.title = element_text(hjust = 0.5)) +
        ggtitle("Evolution de la quantité d'énergie soutirée moyenne (MWh)")+
        ylab(" ")+
        xlab("Temps ") + 
        theme(
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          axis.line.x = element_line(color = "grey"))
    })
    
    output$prod <- renderPlot({plot(plot_dt_prod())}) 
  }
)
