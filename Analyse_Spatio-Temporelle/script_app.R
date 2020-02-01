# . -------------------------------------------------------------------------- =============
# 0 - Chargement des librairies ---------------------------------------------- =============
# . -------------------------------------------------------------------------- =============

### manipulation et traitement de donnees
library(readxl)
library(dplyr) 
library(tidyr)
library(tidyverse)
library(lubridate) 
library(stringr)
library(geojsonR)
### manipulation application
library(shiny)



### visualisation des donnees 
library(ggplot2) # la visualisation graph stat
library(RColorBrewer) # des palettes
library(rmarkdown)
library(gganimate)
library(gapminder)
library(plotly)
library(maps)
#install.packages("mapdata")
library(mapdata)
library(maptools)

## analyse spatiale / carto
library(sp) # classes et methodes pour donnees spatiales pe dlasspar SF
library(tmap) # carto
library(ggmap)# carto +
library(leaflet) # carto web
library(rgdal) #gdal pour projection, crud et surtout export
library(sf) # nouveau package de classes et methodes spatiales doit "remplacer" rgdal et rgeos (et ofc sp) 
library(mapview)
library(raster)
library(rasterVis)
library(rgdal)
library(viridis)


library(rasterVis)
library(animation)
library(classInt)

# . -------------------------------------------------------------------------- =============
# 1 - Preparation des données ------------------------------------------------ =============
# . -------------------------------------------------------------------------- =============

#tableau de variable 
All_event_open_agenda <-  read.csv("All_event_open_agenda.csv", encoding="latin1", sep=";", stringsAsFactors=FALSE) 

# tableau trier de variable 
All_event_open_agenda_OK <- All_event_open_agenda %>%
  drop_na(Longitude, Latitude) %>%
  mutate(Date = dmy(Date)) %>% 
  mutate(categorie = paste(toupper(substr(categorie, 1,1)),substr(categorie, 2,nchar(categorie)), sep=""))

# les des catégories 
list_cat <- All_event_open_agenda_OK %>% distinct(categorie)

# liste des temporalité possible de choisir 
#{(Mensuel : janvier, fevrier, mars, ... ; Hebdomadaire : Lundi, mardi, mercredi, ...; Jounalier: 1h, 2h, 3h,)
list_tempo <- c('Mensuel','Hebdomadaire','Journalier');
#list_tempo

# Visualisation cartographique des arrondisements de Paris 
Limit_com <- (st_read("arrondissements/arrondissements_2154.shp"))
Limit_com$geometry <- st_set_crs(Limit_com$geometry,2154)
LIMIT_COM<- sf:::as_Spatial(Limit_com$geometry)
Limit_box<-as(LIMIT_COM,"SpatialPolygons")
plot(Limit_box)


# . -------------------------------------------------------------------------- =============
# 2 - Creation fonction ------------------------------------------------------ =============
# . -------------------------------------------------------------------------- =============


# Cette fonction a pour but de creer un GIF animé en raster à partir d'une taille de maille, d'un type de temporalité et un jeu de donnée  
GIF_RASTER <- function(rayon, temporalite,data){
# Rayon, variable numérique
# temporalité : variable charactère égale à Mensuel, Hebdomadaire ou journalier 
# data : data.Frame qui contiens les colonnes : date, longitude, latitude, categorie
  
  # transformation en table geom ====
  All_event_open_agenda_GEOM <- data %>% #All_event_open_agenda_OK
    st_as_sf(coords = c("Longitude", "Latitude"), crs = CRS("+init=epsg:4326")) %>%
    st_transform(crs = (2154))
  
  
  # creation BBox ====
  POINT_EVENT=st_cast(All_event_open_agenda_GEOM,"POINT")  # on passe de MULTIPOINTS à POINTS
  POINT_EVENT_sp <- sf:::as_Spatial(POINT_EVENT) #on transforme en SpatialPOintDataFrame
  #plot(POINT_EVENT_sp,pch=20,col="red",cex=0.4)
 
  LIMIT_COM<- sf:::as_Spatial(Limit_com$geometry)
  
  Limit_box<-as((LIMIT_COM),"SpatialPolygons") #on enregistre la taille max de la fenetre de point
  #crs(Limit_box)=crs(POINT_EVENT_sp) #on verifie
  #Comm_area<-area(Limit_box)
  
  Dx = (Limit_box@bbox[3] - Limit_box@bbox[1]) #on cherhe la taille en X
  Dy = Limit_box@bbox[4] - Limit_box@bbox[2] #on cherche la taille en Y
  
  Dx = round(Dx/rayon, 0) #définie la taille de pixel à obtenir en X
  Dy = round(Dy/rayon, 0) #définie la taille de pixel à obtenir en Y
  
  r = raster(Limit_box, ncol = Dx, nrow = Dy) #cree un raster template avec la taille de pixel adapté aux rayons indiqué
  
  # Creation table data des temporalite ====
  # création des tableaux adapté aux donnée sélectionnés 
  if (temporalite == "Journalier"){
    tempo <- All_event_open_agenda_GEOM %>%
      mutate(Temporalite = as.numeric(substr(Heure,0,2)))%>%
      group_by(Temporalite) %>% 
      summarise(nb_event = n())
  } 
  
  if(temporalite == "Hebdomadaire"){
    tempo <- All_event_open_agenda_GEOM %>%
      mutate(Temporalite = wday(Date, label = TRUE, abbr = FALSE))%>%
      group_by(Temporalite) %>% 
      summarise(nb_event = n())
  }
  
  if(temporalite == "Mensuel") {
    tempo <- All_event_open_agenda_GEOM %>%
      mutate(Temporalite = month(Date, label = TRUE, abbr = FALSE))%>%
      group_by(Temporalite) %>% 
      summarise(nb_event = n())
  }
  
  
  # Creation liste de raster selon chaque heure/jours/mois ====
  
  LISTE_DATA = c()
  for(i in c(1:nrow(tempo))){
    
    #affiche la temporalité du raster créer
    print(tempo$Temporalite[i])
    
    #selectionne uniquement la temporalité 
    POINT_EVENT_TEMPO_OK <- tempo %>%
      filter(Temporalite == tempo$Temporalite[i])
    
    # gestion des données géom
    POINT_EVENT=st_cast(POINT_EVENT_TEMPO_OK$geometry,"POINT")  # on passe de MULTIPOINTS à POINTS
    POINT_EVENT_sp<- sf:::as_Spatial(POINT_EVENT) #on transforme en SpatialPOintDataFrame
 
    
    rc = rasterize(POINT_EVENT_sp@coords, r, fun = "count") # créer un raster du jeux de données 
    rc@title <- as.character(as.vector(tempo$Temporalite[i])) # ajout du titre dans le raster à partir de la temporalité selectionner
    LISTE_DATA = c(LISTE_DATA, rc) # enregistrement du raster dans une liste 
    plot(rc, main = rc@title) # affichage du raster et son titre 
  }
  
  # 
  s <- stack(x=LISTE_DATA)
  
  
  # Classification et gestion des couleurs ====

  # classes <- classIntervals(values(rc), n=6, style="quantile", precision = 3)
  # brks <- classes$brks
  
  brks = c(0,1,5,10,25,50,100,1000,3000)

  
  
  # Save en GIF ====

  saveGIF({
    #plot(Limit_com$geometry)
    for(i in c(1:nlayers(s))){
      coul <- viridis(100)
      l <- levelplot(s[[i]], colorkey=list(at=brks, labels=c(as.character(brks))), main = paste(temporalite, s[[i]]@title), col.regions = coul, margin=FALSE, alpha = 0.1)+
        layer_(sp.polygons(Limit_box, fill='grey', alpha=0.9))+
        layer(sp.polygons(Limit_box))
      plot(l)
      
    }
  }, interval=1, movie.name= "animation1.gif")


  #sf::map2SpatialPolygons(Limit_com$geometry, IDs=Limit_com$c_ar,
                      #proj4string=CRS(projection(rs)))
  
  #boundaries <- map('LIMIT_COM', fill=TRUE,
  #                  xlim=ext[1:2], ylim=ext[3:4],
  #                  plot=FALSE)
  
}

#st_sfc(Limit_com$geometry)
GIF_RASTER(200,"Journalier",All_event_open_agenda_OK )


# . -------------------------------------------------------------------------- =============
# 2 - Analyses cartographique ------------------------------------------------ =============
# . -------------------------------------------------------------------------- =============

#visualisation
BG_timide <- mapview(s[[1]], layer.name = c(s[[1]]@title))

  
  colr <- colorRampPalette(rev(brewer.pal(11, 'RdBu')))
  levelplot(rc, 
            margin=FALSE,                       
            colorkey=list(
              space='bottom',                   
              labels=list(at=0,1000, font=4),
              axis.line=list(col='black')       
            ),    
            par.settings=list(
              axis.line=list(col='transparent') 
            ),
            scales=list(draw=FALSE),            
            col.regions=viridis,          
            at=seq(0,300, len=7))
  


# . -------------------------------------------------------------------------- =============
# 3 - Application Shiny  -------------------------------------- =============
# . -------------------------------------------------------------------------- =============

# 1 - Gestion des entrées/sorties ====
ui <- fluidPage(
  # Application titre
  titlePanel("Analyses spatio-temporelles des comportements événementiels"),
  
  conditionalPanel(condition="!$('html').hasClass('shiny-busy')",
                   HTML("<style>#dashboard_complete{cursor:default;}</style>")
  ),
  
  # DEF des entrées ====
  sidebarLayout(
    sidebarPanel(
      
      selectInput("choix_tempo","Choix de la temporalité", list_tempo, selected = 'Mensuel'),
      
      dateRangeInput("choix_date", "choix de la plage temporelle", start = '2018-01-01', end = Sys.Date(), min = NULL,
                     max = NULL, format = "dd/mm/yyyy", startview = "month", weekstart = 0,
                     language = "en", separator = " au ", width = NULL),
      
      checkboxGroupInput("choix_cat","Choix des catégories d'évènements",list_cat[[1]], selected = list_cat[[1]]),
      
      

      tableOutput("cat_select"),
      sliderInput("scale_dist","Rayon d'aggrégation, en Km", 0.05,5,0.1),
      actionButton("start_raster","Lancer la carto", icon("refresh"))
    ),
    
    # DEF des sorties ====
    mainPanel(wellPanel(
      navbarPage("Menu",
                 tabPanel("Tableau des données analysés",tableOutput("table_graph")),
                 tabPanel("description statistique",plotOutput("distPlot_histo")),
                 tabPanel("Carte de chaleur",imageOutput("Plot_GIF") )))
    )
  )
)

# 2 - Gestion du serveur ====
server <- function(input, output) {
  
  # gestion du bouton "lancer carto" ====
  observeEvent(input$start_raster, {
    dist <- (input$scale_dist)*1000
    GIF_RASTER(dist,input$choix_tempo ,react_input())
    Sys.sleep(1)
    
    # gestion de la visualisation carto ====
    output$Plot_GIF <- renderImage({
      # Return a list
      return(list(src = "animation1.gif",
                  contentType = "image/gif",
                  alt = "Ma carte a un petit probleme"))
    }, deleteFile = FALSE)
    
  })

  # gestion reactive des autres entrées ====
  react_input <- reactive({
    Sys.sleep(1)
    data_graph <- All_event_open_agenda_OK %>% filter(categorie %in% input$choix_cat) %>% 
      filter(Date >= input$choix_date[[1]] & Date <= input$choix_date[[2]])
    
    #c(class(All_event_open_agenda_OK$Date),All_event_open_agenda_OK$Date[1], class(dmy(as.character(input$choix_date[[1]]))), input$choix_date[[1]])  
    

  })
  
  # gestion de la visualisation des données ====
  output$table_graph <- renderTable({
    head(react_input(),10)
    })
  
  # gestion de la vialusation statistiques ====
  output$distPlot_histo <- renderPlot({
    
    if (input$choix_tempo == "Journalier"){
      
      data_graph <- react_input() %>%  
        mutate(Temporalite = as.numeric(substr(Heure,0,2)))%>%
        group_by(Temporalite, categorie) %>% 
        summarise(nb_event = n())
      
      graph_plot <- ggplot(data_graph, aes(x = Temporalite, y = nb_event, fill = categorie, color = categorie)) +
        geom_line(stat="identity", alpha = 0.7, size = 1.5 ) +
        labs(x = "heures", y = "Nombre d'événement") +
        ggtitle("Nombre d'événement par heures")+
        theme_minimal()
    } 
    
    if(input$choix_tempo == "Hebdomadaire"){
      data_graph <- react_input() %>%  
        mutate(Temporalite = wday(Date, label = TRUE, abbr = FALSE))%>%
        group_by(Temporalite, categorie) %>% 
        summarise(nb_event = n())
      
      graph_plot <- ggplot(data_graph, aes(x = Temporalite, y = nb_event, fill = categorie)) +
        geom_bar(stat="identity", alpha = 0.7, show.legend = TRUE, position = "dodge" ) +
        labs(x = "Jour de la semaine", y = "Nombre d'événement") +
        ggtitle("Nombre d'événement par jours de la semaines")+
        theme_minimal()
    } 
    
    if(input$choix_tempo == "Mensuel") {
      data_graph <- react_input() %>%  
        mutate(Temporalite = month(Date, label = TRUE, abbr = FALSE))%>%
        group_by(Temporalite, categorie) %>% 
        summarise(nb_event = n())
      
      graph_plot <- ggplot(data_graph, aes(x = Temporalite, y = nb_event, fill = categorie)) +
        geom_bar(stat="identity", alpha = 0.7, show.legend = TRUE, position = "dodge" ) +
        labs(x = "Mois de l'année", y = "Nombre d'événement") +
        ggtitle("Nombre d'événement par mois")+
        theme_minimal()
    }
    graph_plot

  })
  }
  # . -------------------------------------------------------------------------- =============
  # Lancer l'application ------------------------------------------------------- =============
  # . -------------------------------------------------------------------------- =============
shinyApp(ui = ui, server = server)



