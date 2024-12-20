# Application de visualisation des actes de Terrorisme sur le continent
# Africain par Edi Valère TRA _ 2024 #

# Installation des packages
#install.packages(c("shiny", "shinydashboard", "leaflet", "sf", "dplyr", "ggplot2", "plotly"))

# Chargement des bibliothèques nécessaires
library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(dplyr)
library(plotly)

# Chargement des données avec suppression des messages
terrorisme_data <- tryCatch({
  st_read("C:/Users/kaize/OneDrive/Bureau/Master2_GEODYM/Webmapping/Rendu/Terrorisme/Terrorisme.shp", quiet = TRUE) %>%
    na.omit()
}, error = function(e) {
  showNotification("Erreur lors du chargement du shapefile : vérifiez le chemin ou le format.", type = "error")
  NULL
})

# Harmonisation des colonnes
if (!is.null(terrorisme_data)) {
  # Supprimer les colonnes géométriques pour éviter les erreurs dans les calculs
  terrorisme_data <- terrorisme_data %>%
    st_drop_geometry() %>%
    mutate(
      Pays = trimws(Pays),
      Typ_violen = tolower(trimws(Typ_violen)),
      latitude = as.numeric(latitude),
      longitude = as.numeric(longitude),
      Annee = as.integer(Annee),
      Nbre_morts = as.numeric(Nbre_morts)
    )
}

# Interface utilisateur
ui <- dashboardPage( skin = "black",
                     dashboardHeader(title = "Analyse du Terrorisme en Afrique"),
                     dashboardSidebar(
                       # Sélectionnez un pays dans la liste déroulante
                       selectizeInput(
                         "recherche_pays", 
                         "Rechercher un pays", 
                         choices = if (!is.null(terrorisme_data)) unique(terrorisme_data$Pays) else NULL, 
                         multiple = FALSE,
                         options = list(placeholder = "Sélectionnez un pays", allowEmptyOption = TRUE)
                       ),
                       
                       # Faites le choix des éléments à afficher
                       tags$h4("Types de violences"),
                       checkboxInput("checkbox_etat", "Conflit étatique", value = TRUE),
                       checkboxInput("checkbox_non_etatique", "Conflit non étatique", value = TRUE),
                       checkboxInput("checkbox_unilaterale", "Violence unilatérale", value = TRUE)
                     ),
                     
                     # Onglets de l'application
                     dashboardBody(
                       tabsetPanel(
                         # Onglet 1 : Présentation et but de l'application
                         tabPanel(
                           "Description",
                           tags$div(
                             style = "padding: 20px; background-color: #f9f9f9;",
                             tags$h3("Projet de Webmapping avec RShiny | Université de Rouen Normandie | Master 2 GEODYN| 2024"),
                             tags$h3("Auteur: Edi Valère TRA"),
                             tags$h3("Problématique: Analyse du Terrorisme en Afrique"),
                             tags$p(
                               "La raison d'être de cette application est d'analyser et de visualiser des données sur le terrorisme en Afrique. Elle permet de filtrer et d'examiner des informations sur les événements terroristes en fonction de plusieurs critères, tels que le pays, le type de violence, et l'année."
                             ),
                             tags$ul(
                               tags$li("Explorer les données géospatiales : Carte interactive affichant les incidents selon leurs coordonnées géographiques."),
                               tags$li("Analyser l'évolution des événements terroristes : Diagrammes en camembert et courbes d'évolution."),
                               tags$li("Faire des comparaisons par type de violence : Conflit étatique, non étatique, et violence unilatérale."),
                               tags$li("Fournir un outil pédagogique et de recherche : Idéal pour l'analyse, la recherche académique et la sensibilisation.")
                             ),
                             tags$h3("Stop au Terrorisme"),
                             tags$footer("© 2024 Edi Valère TRA")
                           )
                         ),
                         
                         # Onglet 2 : Application interactive et visualisation des données
                         tabPanel(
                           "Visualisation interactive",
                           fluidRow(
                             column(6,
                                    plotlyOutput("courbe_morts")  # Graphique d'évolution des morts par année
                             ),
                             column(6,
                                    plotlyOutput("camembert")  # Camembert des types de violences
                             )
                           ),
                           fluidRow(
                             column(12,
                                    leafletOutput("carte")  # Carte interactive
                             )
                           )
                         ),
                         
                         # Onglet 3 : Sources de données et Flowchart
                         tabPanel(
                           "Sources de données et Flowchart",
                           tags$div(
                             style = "padding: 20px; background-color: #f9f9f9;",
                             tags$h3("Sources des données"),
                             tags$p(
                               "Les données utilisées dans cette application proviennent de diverses sources publiques, notamment des bases de données internationales sur le terrorisme et des rapports d'organisations de surveillance des conflits."
                             ),
                             # Infos sur les Sources de données
                             tags$ul(
                               tags$li(
                                 tags$a(
                                   href = "https://www.start.umd.edu/gtd-download", 
                                   target = "_blank", 
                                   "Global Terrorism Database (GTD)"
                                 )
                               ),
                               tags$li(
                                 tags$a(
                                   href = "https://acleddata.com/curated-data-files/#regional", 
                                   target = "_blank", 
                                   "ACLED (Armed Conflict Location & Event Data Project)"
                                 )
                               )
                             ),
                             # Organisation du code avec le flowchart
                             tags$h3("Flowchart de l'Organisation du Code et du Fonctionnement"),
                             tags$p(
                               "Voici un schéma représentant l'organisation du code et les étapes clés de l'analyse des données dans cette application."
                             ),
                             tags$img(src = 'shiny_app.png' ,width= 350, height= 500, style = "margin-left: 50px;"
                             )
                           )
                         )
                       )
                     )
)

# Organisation du serveur
server <- function(input, output, session) {
  
  # Filtrer les données en fonction du pays et des types de violences que vous vouklez
  filtered_data <- reactive({
    req(input$recherche_pays)  # En fonction du pays sélectionné
    
    selected_types <- c()
    if (input$checkbox_etat) selected_types <- c(selected_types, "conflit étatique")
    if (input$checkbox_non_etatique) selected_types <- c(selected_types, "conflit non étatique")
    if (input$checkbox_unilaterale) selected_types <- c(selected_types, "violence unilatérale")
    
    # Filtrage des données
    data <- terrorisme_data %>%
      filter(Pays == input$recherche_pays) %>%
      filter(Typ_violen %in% selected_types)
    
    return(data)
  })
  
  # Carte interactive en clusters
  output$carte <- renderLeaflet({
    req(filtered_data())
    data <- filtered_data()
    
    # Couleurs en fonction du type de violence
    colors <- c("conflit étatique" = "green", "conflit non étatique" = "gold", "violence unilatérale" = "darkorange")
    
    # Création de la carte
    leaflet(data) %>%
      addTiles(
        urlTemplate = "https://tile.jawg.io/jawg-matrix/{z}/{x}/{y}{r}.png?access-token=fbHr4ScybbgkRVg27GfanQo3W7Mf2r9QCoP9Df2uxIznXIHBmnHH1eAfSBY8xBUX"
      ) %>%
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        color = ~colors[Typ_violen],  # Couleur des marqueurs selon le type de violence
        radius = ~sqrt(Nbre_morts),  # Taille des marqueurs en fonction du nombre de morts
        label = ~paste("Nombre de morts:", Nbre_morts, "<br>Type de violence:", Typ_violen),
        clusterOptions = markerClusterOptions()  # Vue des données sous forme de clusters
      )
  })
  
  # Courbe d'évolution des morts par année et type de violence
  output$courbe_morts <- renderPlotly({
    req(filtered_data())
    
    data <- filtered_data() %>%
      filter(Annee >= 1989 & Annee <= 2017) %>%
      group_by(Annee, Typ_violen) %>%
      summarise(morts = sum(Nbre_morts, na.rm = TRUE), .groups = 'drop')
    
    # Tendance générale
    trend_data <- filtered_data() %>%
      filter(Annee >= 1989 & Annee <= 2017) %>%
      group_by(Annee) %>%
      summarise(morts = sum(Nbre_morts, na.rm = TRUE), .groups = 'drop')
    
    # Tracer les courbes
    plot_ly() %>%
      add_lines(data = trend_data, x = ~Annee, y = ~morts, line = list(color = 'blue'), name = "Tendance générale") %>%
      add_lines(data = data %>% filter(Typ_violen == "conflit étatique"), x = ~Annee, y = ~morts, line = list(color = 'green'), name = "Conflit étatique") %>%
      add_lines(data = data %>% filter(Typ_violen == "conflit non étatique"), x = ~Annee, y = ~morts, line = list(color = 'gold'), name = "Conflit non étatique") %>%
      add_lines(data = data %>% filter(Typ_violen == "violence unilatérale"), x = ~Annee, y = ~morts, line = list(color = 'darkorange'), name = "Violence unilatérale") %>%
      layout(
        title = "Évolution des morts par année (1989-2017)",
        xaxis = list(title = "Année", dtick = 2), # 2 est le pas d'espacement entre les années
        yaxis = list(title = "Nombre de morts")
      )
  })
  
  # Camembert dynamique pour les types de violences
  output$camembert <- renderPlotly({
    req(filtered_data())
    data <- filtered_data() %>%
      group_by(Typ_violen) %>%
      summarise(morts = sum(Nbre_morts), .groups = 'drop')
    
    colors <- c("conflit étatique" = "green", "conflit non étatique" = "gold", "violence unilatérale" = "darkorange")
    
    plot_ly(
      data, 
      labels = ~Typ_violen, 
      values = ~morts, 
      type = 'pie', 
      textinfo = 'label+percent',
      marker = list(colors = colors)
    ) %>%
      layout(
        title = "Répartition des morts par types de violence",
        titlefont = list(size = 16)
      )
  })
}

# Fin & lancement de l'application
shinyApp(ui = ui, server = server)