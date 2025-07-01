# modules/tache_module.R

library(shiny)
library(DT)
library(dplyr)
library(shinyjs)

tache_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    fluidRow(
      column(6,
             h4("Ajouter / Modifier une tâche"),
             selectInput(ns("projet_associe"), "Projet associé", choices = NULL),
             textInput(ns("nom_tache"), "Nom de la tâche"),
             textAreaInput(ns("description_tache"), "Description", height = "100px"),
             dateInput(ns("date_echeance_tache"), "Date d'échéance", value = Sys.Date()),
             selectInput(ns("priorite"), "Priorité", choices = c("Basse", "Moyenne", "Haute")),
             numericInput(ns("duree_estimee"), "Durée estimée (h)", value = 1, min = 0.5, step = 0.5),
             selectInput(ns("statut_tache"), "Statut", choices = c("À faire", "En cours", "Complétée")),
             shinyjs::hidden(textInput(ns("id_tache_modif"), "ID tâche")),
             actionButton(ns("ajouter_tache"), "Ajouter tâche", class = "btn-primary"),
             actionButton(ns("modifier_tache"), "Modifier tâche", class = "btn-warning")
      ),
      column(6,
             h4("Liste des tâches"),
             selectInput(ns("filtre_projet"), "Filtrer par projet", choices = c("Tous")),
             DT::dataTableOutput(ns("table_taches"))
      )
    )
  )
}

tache_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    fichier_taches <- "data/taches.rds"
    fichier_projets <- "data/projets.rds"

    # Charger les projets pour les menus
    observe({
      if (file.exists(fichier_projets)) {
        projets <- readRDS(fichier_projets)
        choix <- setNames(projets$id_projet, projets$nom_projet)
        updateSelectInput(session, "projet_associe", choices = choix)
        updateSelectInput(session, "filtre_projet", choices = c("Tous", projets$nom_projet))
      }
    })

    # Chargement des tâches
    taches <- reactiveVal({
      if (file.exists(fichier_taches)) {
        tryCatch(readRDS(fichier_taches), error = function(e) tibble())
      } else {
        tibble()
      }
    })

    # Filtrage selon projet sélectionné
    taches_filtrees <- reactive({
      df <- taches()
      if (input$filtre_projet == "Tous" || is.null(input$filtre_projet)) return(df)
      projets <- readRDS(fichier_projets)
      id <- projets$id_projet[projets$nom_projet == input$filtre_projet]
      filter(df, id_projet == id)
    })

    # Ajouter tâche
    observeEvent(input$ajouter_tache, {
      if (input$nom_tache != "" && input$projet_associe != "") {
        nouvelle_tache <- tibble(
          id_tache = paste0("T", as.integer(Sys.time())),
          id_projet = input$projet_associe,
          nom_tache = input$nom_tache,
          description_tache = input$description_tache,
          date_echeance_tache = input$date_echeance_tache,
          priorite = input$priorite,
          duree_estimee = input$duree_estimee,
          statut_tache = input$statut_tache
        )
        maj <- bind_rows(taches(), nouvelle_tache)
        taches(maj)
        saveRDS(maj, fichier_taches)
        reset_form()
      }
    })

    # Préremplir formulaire lors sélection ligne
    observeEvent(input$table_taches_rows_selected, {
      sel <- input$table_taches_rows_selected
      df <- taches_filtrees()
      if (!is.null(sel) && nrow(df) >= sel) {
        ligne <- df[sel, ]
        updateSelectInput(session, "projet_associe", selected = ligne$id_projet)
        updateTextInput(session, "nom_tache", value = ligne$nom_tache)
        updateTextAreaInput(session, "description_tache", value = ligne$description_tache)
        updateDateInput(session, "date_echeance_tache", value = ligne$date_echeance_tache)
        updateSelectInput(session, "priorite", selected = ligne$priorite)
        updateNumericInput(session, "duree_estimee", value = ligne$duree_estimee)
        updateSelectInput(session, "statut_tache", selected = ligne$statut_tache)
        updateTextInput(session, "id_tache_modif", value = ligne$id_tache)
      }
    })

    # Modifier tâche
    observeEvent(input$modifier_tache, {
      id <- input$id_tache_modif
      df <- taches()
      if (id != "" && id %in% df$id_tache) {
        index <- which(df$id_tache == id)
        df[index, ] <- tibble(
          id_tache = id,
          id_projet = input$projet_associe,
          nom_tache = input$nom_tache,
          description_tache = input$description_tache,
          date_echeance_tache = input$date_echeance_tache,
          priorite = input$priorite,
          duree_estimee = input$duree_estimee,
          statut_tache = input$statut_tache
        )
        taches(df)
        saveRDS(df, fichier_taches)
        reset_form()
      }
    })

    # Affichage du tableau
    output$table_taches <- renderDataTable({
      df <- taches_filtrees()
      if (nrow(df) > 0 && file.exists(fichier_projets)) {
        projets <- readRDS(fichier_projets)
        df <- left_join(df, projets, by = c("id_projet" = "id_projet")) |>
          mutate(
            statut_colore = case_when(
              statut_tache == "À faire" ~ "<span style='color: gray;'>● À faire</span>",
              statut_tache == "En cours" ~ "<span style='color: orange;'>● En cours</span>",
              statut_tache == "Complétée" ~ "<span style='color: green;'>● Complétée</span>",
              TRUE ~ statut_tache
            )
          ) |>
          select(
            Projet = nom_projet,
            Tâche = nom_tache,
            Description = description_tache,
            `Échéance` = date_echeance_tache,
            Priorité = priorite,
            `Durée (h)` = duree_estimee,
            Statut = statut_colore
          )
        datatable(df, escape = FALSE, selection = "single", options = list(pageLength = 5))
      }
    })

    # Fonction pour vider le formulaire
    reset_form <- function() {
      updateTextInput(session, "nom_tache", value = "")
      updateTextAreaInput(session, "description_tache", value = "")
      updateDateInput(session, "date_echeance_tache", value = Sys.Date())
      updateSelectInput(session, "priorite", selected = "Moyenne")
      updateNumericInput(session, "duree_estimee", value = 1)
      updateSelectInput(session, "statut_tache", selected = "À faire")
      updateTextInput(session, "id_tache_modif", value = "")
    }
  })
}
