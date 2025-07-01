# modules/dashboard_module.R

library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(tidyr)

dashboard_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    h3("Tableau de bord de l'avancement"),
    fluidRow(
      column(12,
             DT::dataTableOutput(ns("table_avancement"))
      )
    ),
    hr(),
    fluidRow(
      column(12,
             plotOutput(ns("plot_avancement"))
      )
    )
  )
}

dashboard_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    fichier_taches <- "data/taches.rds"
    fichier_projets <- "data/projets.rds"

    # Charger données
    projets <- reactive({
      if (file.exists(fichier_projets)) readRDS(fichier_projets) else tibble()
    })

    taches <- reactive({
      if (file.exists(fichier_taches)) readRDS(fichier_taches) else tibble()
    })

    # Tableau de synthèse
    resume_avancement <- reactive({
      req(nrow(projets()) > 0)
      df_t <- taches()
      df_p <- projets()

      resume <- df_t %>%
        group_by(id_projet) %>%
        summarise(
          nb_taches = n(),
          nb_completees = sum(statut_tache == "Complétée"),
          pourcentage = round(100 * nb_completees / nb_taches, 1),
          .groups = "drop"
        ) %>%
        right_join(df_p, by = "id_projet") %>%
        replace_na(list(nb_taches = 0, nb_completees = 0, pourcentage = 0)) %>%
        select(nom_projet, nb_taches, nb_completees, pourcentage)

      resume <- resume %>%
        mutate(
          progression = purrr::map_chr(pourcentage, ~{
            paste0(
              '<div style="background-color: #e0e0e0; border-radius: 5px; height: 20px; width: 100%;">',
              '<div style="background-color: #4CAF50; width:', .x, '%; height: 100%; border-radius: 5px;"></div>',
              '</div>'
            )
          })
        )
      resume
    })

    # Tableau
    output$table_avancement <- renderDataTable({
      df <- resume_avancement()
      datatable(
        df %>%
          select(Projet = nom_projet, `Tâches` = nb_taches, `Complétées` = nb_completees, `%` = pourcentage, Progression = progression),
        escape = c(1:4),  # ne pas échapper la progression (HTML)
        options = list(pageLength = 10),
        rownames = FALSE
      )
    })

    # Graphique
    output$plot_avancement <- renderPlot({
      df <- resume_avancement()
      ggplot(df, aes(x = reorder(nom_projet, -pourcentage), y = pourcentage)) +
        geom_col(fill = "#4CAF50") +
        geom_text(aes(label = paste0(pourcentage, "%")), vjust = -0.5) +
        ylim(0, 100) +
        labs(x = "Projet", y = "% de tâches complétées", title = "Avancement des projets") +
        theme_minimal()
    })
  })
}
