
library(shiny)
library(shinyjs)

# Modules
source("modules/projet_module.R")
source("modules/tache_module.R")
source("modules/dashboard_module.R")

ui <- navbarPage("Suivi de projets",
  tabPanel("Projets", projet_ui("projet")),
  tabPanel("Tâches", tache_ui("tache")),
  tabPanel("Tableau de bord", dashboard_ui("dashboard"))
)

server <- function(input, output, session) {
  projet_server("projet")
  tache_server("tache")
  dashboard_server("dashboard")
}

shinyApp(ui, server)

