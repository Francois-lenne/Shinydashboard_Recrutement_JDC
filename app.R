#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# chargement des librairies utilisés

library(shinydashboard)
library(shiny)
library(readxl)
library(tidyverse)
library(highcharter)
library(sqldf)
library(plotly)

# chargement des BDD


## initial 

data_frame <- read_excel("JDC_2.xlsx")


## carte

data_fr <- download_map_data("countries/fr/fr-all-all") %>% 
    get_data_from_map()



tab_1 = sqldf("select Date,sum(Armée_air) as 'Armée_air', sum(Armée_terre) as 'Armée_terre', sum(Gendarmerie) as Gendarmerie, sum(Marine) as Marine, sum(Service_Civique) as 'Service_Civique' from data_frame group by Date")
graph1 <- tab_1 %>%
    dplyr::select(Date, Marine, Gendarmerie,Service_Civique,Armée_air,Armée_terre) %>%
    gather(key = "variable", value = "value", -Date)
# calcul des values box




somme_participant <- round(sum(data_frame$NB_présent)/1000000,2)

part_info <- round(sum(data_frame$Total_armée)/sum(data_frame$NB_présent),2)*100

Part_femme <- round(sqldf("select sum(NB_présent) from data_frame where Sexe = 'Féminin'")/sqldf("select sum(NB_présent) from data_frame"),2) * 100

# modif pour créer la carte 

data_fr$dep = data_fr$name




data_fr$name[data_fr$fips %in% "FG01"] <- "Guyane"

data_fr$name[data_fr$fips %in% "FRB1"] <- "Corrèze"

data_fr$name[data_fr$fips %in% "FRB7"] <- "Deux-Sèvres"

data_fr$name[data_fr$fips %in% "FRB3"] <- "Ariège"

data_fr$name[data_fr$fips %in% "FRA2"] <- "Finistère"

data_fr$name[data_fr$fips %in% "FRA1"] <- "Nièvre"

data_fr$name[data_fr$fips %in% "FRA9"] <- "Lozère"

data_fr$name[data_fr$longitude %in% "4.4999"] <- "Ardèche"

data_fr$name[data_fr$longitude %in% "5.4175"] <- "Isère"

df2 = sqldf("select Libelle_departement, Total_armée as NB_demande from data_frame group by Libelle_departement")


data_fr2 = data_fr %>% left_join(df2, by = c("name" = "Libelle_departement"))
armee <- sort(unique(graph1$variable))
departement <- c("Aisne","Nord","Oise","Pas-de-Calais","Somme")


# liste de département

dep <- unique(data_frame$Libelle_departement)


# code pour la barre à gauche

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Overview", tabName = "Overview",
                 icon = icon("door-open")),
        menuItem("Tableau de bord", tabName = "Dashboard",
                 icon = icon("dashboard"),
                 badgeLabel = "nouveau", badgeColor = "green")
    )
    
)

body <- dashboardBody(
    tabItems(
        tabItem(
            tabName = "Overview",
            column(width =10,
                   valueBox(value = somme_participant,
                            subtitle = "Millions de participants à la JDC",
                            icon("user", lib = "glyphicon"),
                            width = 4),
                   valueBox(value = paste(part_info,"%"),
                            subtitle = "Part d'appelées voulant rejoindre l'armée",
                            icon("file", lib = "font-awesome"),
                            width = 4,
                            color = "orange"),
                   valueBox(value = paste(Part_femme,"%"),
                            subtitle = "Part de femme parmis les appelées",
                            icon("venus", lib = "font-awesome"),
                            width = 4,
                            color = "purple")),
            column( 10, align="center"
                    , box(title = "Presentation de la Journée Défense et Citoyenneté (JDC)",
                          width = 12,
                          p("La JDC a été créée en 2011. Auparavant celle-ci se nommer JAPD et elle remplace le service militaire obligatoire qui fût supprimé en 1997. Cette journée permet aux jeunes de découvrir les différents metiers de l'armée française."))
            ),
            column(width = 10,
                   box(title = tagList(icon("globe"),"Carte des départements en fonction du nombre de demandes d'informations"),
                       status = "success",
                       width = 12,
                       height = 600,
                       solidHeader = TRUE,
                       highchartOutput( outputId = "carte")
                       
                   ) 
            )
            
        ),
        tabItem(
            tabName = "Dashboard",
            fluidRow(
                column(4,
                       selectInput(inputId = "Libelle_departement",
                                   label = "Choix du département",
                                   choices = dep,
                                   multiple = TRUE,
                                   selected = departement ))
                ,column(4,
                        sliderInput(inputId = "id",
                                    label = "Années",
                                    min = 2013, max = 2020,
                                    step = 1, sep = "",
                                    value = 2013,
                                    animate = TRUE))
                ,column(4,
                        selectizeInput(inputId = "armee",
                                       label = "Corps de métier",
                                       choices = armee,
                                       multiple = TRUE,
                                       selected = armee))),
            fluidRow(
                column(width = 6,
                       box(title = tagList(icon("chart-bar"),"Nombre de demandes d'informations par corps de métier"),
                           width = 12,
                           height = 500,
                           solidHeader = TRUE,
                           status = "info",
                           plotlyOutput( outputId = "geom_bar" ))),
                column(width = 6,
                       height = 500,
                       box(title = tagList(icon("chart-pie"),"Répartition des participants en fonction de leur sexe"),
                           width = 12,solidHeader = TRUE,
                           status = "warning",
                           plotlyOutput( outputId = "pie" )))
                
            ))
        
        
    )
    
)






ui <- dashboardPage( skin = 'red',
                     dashboardHeader( title = "La JDC"), sidebar, body
)

server <- function(input, output) {
    output$carte <- renderHighchart( {
        hcmap("countries/fr/fr-all-all",
              data = data_fr2,
              value = "NB_demande",
              joinBy = c("hc-a2"),
              name = "France",
              dataLabels = list(enabled = TRUE, format = '{point.name}'),
              borderColor = "#FAFAFA", borderWidth = 0.1) %>% 
            hc_mapNavigation(enabled = TRUE) %>%
            hc_title(
                text = "",
                margin = 20,
                align = "center",
                style = list(color = "#1E5FD9", useHTML = TRUE)
            )
        
    })
    
    data_filter_1 <- reactive ({
        data_frame %>% filter (id == input$id, Libelle_departement %in% input$Libelle_departement)%>%
            select(id,Sexe, any_of(input$armee)) %>%
            gather(key = "variable", value = "value",-id, -Sexe) %>%
            group_by(variable,Sexe) %>%
            summarise(somme = sum(value))
        
    })
    
    
    output$geom_bar <- renderPlotly( {
        g3 = ggplot(data_filter_1()) + aes(x= variable,y = somme, fill = Sexe) + 
            geom_bar(stat="identity") +  
            labs(x = "Corps de metier", y = "Nombre de demandes d'informations") +
            scale_fill_brewer(palette="Blues") +
            coord_flip() 
        
        ggplotly(g3)
        
    })
    
    data_filter_1 <- reactive ({
        data_frame %>% filter (id == input$id, Libelle_departement %in% input$Libelle_departement)%>%
            select(id,Sexe, any_of(input$armee)) %>%
            gather(key = "variable", value = "value",-id, -Sexe) %>%
            group_by(variable,Sexe) %>%
            summarise(somme = sum(value))
        
    })
    data_filter_2 <- reactive ({
        data_frame %>%
            filter(id == input$id, Libelle_departement %in% input$Libelle_departement) %>%
            select(Sexe,NB_présent) %>%
            group_by(Sexe) %>%
            summarise(somme = sum(NB_présent))
    })
    
    output$pie <- renderPlotly( {
        fig <- plot_ly(data_filter_2(), labels = ~ Sexe, values = ~somme, type = 'pie',
                       insidetextfont = list(color = '#FFFFFF'),
                       marker = list(colors = c("#C1D6FC","#819FD8")),
                       line = list(color = '#FFFFFF', width = 1))
        
        
        fig <- fig %>% layout(title = "",
                              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        
        
        fig
        
        
    })
}

shinyApp(ui = ui, server = server)