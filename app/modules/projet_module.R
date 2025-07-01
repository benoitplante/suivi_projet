# modules/projet_module.R

library(shiny)
library(DT)
library(dplyr)

projet_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    fluidRow(
      column(6,
             h4("Ajouter / Modifier un projet"),
             textInput(ns("nom_projet"), "Nom du projet"),
             textAreaInput(ns("description_projet"), "Description", height = "100px"),
             dateInput(ns("date_debut"), "Date de début"),
             dateInput(ns("date_echeance"), "Date d'échéance"),
             selectInput(ns("statut_projet"), "Statut",
                         choices = c("En cours", "Terminé", "En pause")),
             actionButton(ns("ajouter_projet"), "Ajouter projet", class = "btn-primary"),
             actionButton(ns("modifier_projet"), "Sauvegarder modifications", class = "btn-warning"),
             hidden(textInput(ns("id_modif"), "ID projet (interne)")) # utilisé pour modification
      ),
      column(6,
             h4("Projets enregistrés"),
             selectInput(ns("filtre_statut"), "Filtrer par statut", choices = c("Tous", "En cours", "Terminé", "En pause")),
             DT::dataTableOutput(ns("table_projets"))
      )
    )
  )
}

projet_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    chemin_fichier <- "data/projets.rds"

    # Données réactives
    projets <- reactiveVal({
      if (file.exists(chemin_fichier)) {
        readRDS(chemin_fichier)
      } else {
        tibble(
          id_projet = character(),
          nom_projet = character(),
          description_projet = character(),
          date_debut = as.Date(character()),
          date_echeance = as.Date(character()),
          statut_projet = character()
        )
      }
    })

    # Filtrage des projets
    projets_filtres <- reactive({
      if (input$filtre_statut == "Tous") {
        projets()
      } else {
        filter(projets(), statut_projet == input$filtre_statut)
      }
    })

    # Ajouter projet
    observeEvent(input$ajouter_projet, {
      if (input$nom_projet != "") {
        nouveau <- tibble(
          id_projet = paste0("P", as.integer(Sys.time())),
          nom_projet = input$nom_projet,
          description_projet = input$description_projet,
          date_debut = input$date_debut,
          date_echeance = input$date_echeance,
          statut_projet = input$statut_projet
        )
        maj <- bind_rows(projets(), nouveau)
        projets(maj)
        saveRDS(maj, chemin_fichier)
        update_inputs()
      }
    })

    # Modifier projet
    observeEvent(input$modifier_projet, {
      id <- input$id_modif
      if (id != "") {
        df <- projets()
        index <- which(df$id_projet == id)
        if (length(index) == 1) {
          df[index, ] <- tibble(
            id_projet = id,
            nom_projet = input$nom_projet,
            description_projet = input$description_projet,
            date_debut = input$date_debut,
            date_echeance = input$date_echeance,
            statut_projet = input$statut_projet
          )
          projets(df)
          saveRDS(df, chemin_fichier)
          update_inputs()
        }
      }
    })

    # Sélection pour modification
    observeEvent(input$table_projets_rows_selected, {
      sel <- input$table_projets_rows_selected
      if (!is.null(sel)) {
        projet <- projets_filtres()[sel, ]
        updateTextInput(session, "nom_projet", value = projet$nom_projet)
        updateTextAreaInput(session, "description_projet", value = projet$description_projet)
        updateDateInput(session, "date_debut", value = projet$date_debut)
        updateDateInput(session, "date_echeance", value = projet$date_echeance)
        updateSelectInput(session, "statut_projet", selected = projet$statut_projet)
        updateTextInput(session, "id_modif", value = projet$id_projet)
      }
    })

    # Supprimer projet via bouton dans le tableau
    observeEvent(input$table_projets_cell_clicked, {
      info <- input$table_projets_cell_clicked
      if (!is.null(info$value) && info$col == 6 && info$value == "🗑️") {
        id_ligne <- projets_filtres()[info$row, ]$id_projet
        df <- projets() |> filter(id_projet != id_ligne)
        projets(df)
        saveRDS(df, chemin_fichier)
      }
    })

    # Table avec suppression et indicateur coloré
    output$table_projets <- renderDataTable({
      df <- projets_filtres() |>
        mutate(
          statut_color = case_when(
            statut_projet == "En cours" ~ "<span style='color: green;'>● En cours</span>",
            statut_projet == "Terminé" ~ "<span style='color: blue;'>● Terminé</span>",
            statut_projet == "En pause" ~ "<span style='color: orange;'>● En pause</span>",
            TRUE ~ statut_projet
          ),
          supprimer = "🗑️"
        ) |>
        select(Nom = nom_projet, Description = description_projet,
               `Début` = date_debut, `Échéance` = date_echeance,
               Statut = statut_color, supprimer)

      datatable(df, escape = FALSE, selection = "single", rownames = FALSE,
                options = list(pageLength = 5, columnDefs = list(
                  list(targets = 5, orderable = FALSE)
                )))
    })

    # Réinitialiser les champs
    update_inputs <- function() {
      updateTextInput(session, "nom_projet", value = "")
      updateTextAreaInput(session, "description_projet", value = "")
      updateDateInput(session, "date_debut", value = Sys.Date())
      updateDateInput(session, "date_echeance", value = Sys.Date())
      updateSelectInput(session, "statut_projet", selected = "En cours")
      updateTextInput(session, "id_modif", value = "")
    }
  })
}
