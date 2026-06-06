#====================================================
# PACKAGES
#====================================================

library(shiny)
library(shinydashboard)
library(shinymanager)

library(tidyverse)
library(plotly)
library(DT)
library(corrplot)

source("R/data_cleaning.R")
source("R/custom_geom.R")
source("R/grid_tools.R")
source("R/helpers.R")


#====================================================
# AUTHENTIFICATION
#====================================================
credentials <- data.frame(
  
  user = c(
    "admin",
    "user1",
    "user2"
  ),
  
  password = c(
    "admin123",
    "data2026",
    "manager123"
  ),
  stringsAsFactors = FALSE
  
)


#====================================================
# DONNEES
#====================================================

data <- load_and_clean_data()

#====================================================
# THEME GGPLOT GLOBAL
# Un thème unique appliqué à tous les graphiques
#====================================================

PALETTE_TROUBLES <- c(
  "Yes" = "#1da87a",
  "No"  = "#e2e8f0"
)

PALETTE_TROUBLES_MULTI <- c(
  "depression" = "#e11d48",
  "anxiety"    = "#d97706",
  "panic"      = "#7c3aed"
)

PALETTE_GENDER <- c(
  "Male"   = "#2563eb",
  "Female" = "#db2777"
)

theme_mental <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      # Police globale
      text = element_text(family = "sans", color = "#334155"),
      
      # Titres des axes
      axis.title = element_text(size = 11, color = "#64748b", face = "plain"),
      axis.text  = element_text(size = 10, color = "#64748b"),
      axis.ticks = element_blank(),
      axis.line  = element_blank(),
      
      # Grille
      panel.grid.major   = element_line(color = "#f1f5f9", linewidth = 0.6),
      panel.grid.minor   = element_blank(),
      panel.background   = element_rect(fill = "white", color = NA),
      plot.background    = element_rect(fill = "white", color = NA),
      
      # Légende
      legend.position    = "bottom",
      legend.title       = element_text(size = 10, face = "bold", color = "#334155"),
      legend.text        = element_text(size = 10, color = "#475569"),
      legend.key.size    = unit(0.5, "lines"),
      legend.background  = element_blank(),
      
      # Facettes
      strip.text         = element_text(size = 11, face = "bold", color = "#1e293b"),
      strip.background   = element_rect(fill = "#f8fafc", color = NA),
      
      # Titres du graphique
      plot.title         = element_text(size = 14, face = "bold", color = "#0f172a",
                                        margin = margin(b = 4)),
      plot.subtitle      = element_text(size = 11, color = "#64748b",
                                        margin = margin(b = 12)),
      plot.caption       = element_text(size = 9, color = "#94a3b8",
                                        hjust = 1),
      plot.margin        = margin(12, 16, 12, 16)
    )
}

# Appliquer le thème comme défaut global
theme_set(theme_mental())

#====================================================
# UI
#====================================================

ui <-  secure_app(
  
  dashboardPage(
  
  skin = "blue",
  
  dashboardHeader(
    title = tags$span(
      style = "font-family:'Nunito',sans-serif; font-weight:700; letter-spacing:-0.01em; color:#ffffff;",
      tags$span("MentalHealth"),
      tags$span(style = "color:#6ee7b7;", " Dashboard")   # menthe clair sur fond forêt
    )
  ),
  
  dashboardSidebar(
    
    sidebarMenu(
      id = "active_tab",
      
      # ---- Entrée Home (page accueil) ----
      menuItem(
        "Accueil",
        tabName = "home",
        icon = icon("house-user")
      ),
      
      menuItem(
        "Profil étudiant",
        tabName = "profil",
        icon = icon("user-graduate")
      ),
      
      menuItem(
        "Santé mentale",
        tabName = "mental",
        icon = icon("brain")
      ),
      
      menuItem(
        "Analyse croisée",
        tabName = "analyse",
        icon = icon("chart-bar")
      )
      
    ),
    
    hr(),
    
    # ---- FILTRES ----
    selectInput(
      "gender",
      "Genre",
      choices = c("Tous", sort(unique(data$gender)))
    ),
    
    selectInput(
      "year",
      "Année",
      choices = c("Toutes", sort(unique(data$year)))
    ),
    
    selectInput(
      "course",
      "Filière",
      choices = c("Toutes", sort(unique(data$course)))
    ),
    
    checkboxGroupInput(
      "troubles",
      "Troubles",
      choices  = c("depression", "anxiety", "panic"),
      selected = c("depression", "anxiety", "panic")
    )
    
  ),
  
  dashboardBody(
    
    # ---- CSS ----
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      # Icônes Font Awesome 6
      tags$link(
        rel = "stylesheet",
        href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.2/css/all.min.css"
      )
    ),
    
    tabItems(
      
      #====================================================
      # ONGLET 0 : PAGE D'ACCUEIL
      # Objectif : première impression professionnelle
      # => badge, titre fort, KPI snapshot, CTA, features
      #====================================================
      
      tabItem(
        tabName = "home",
        
        div(
          class = "home-page",
          
          # Logo
          div(
            class = "home-logo-circle",
            tags$i(class = "fa-solid fa-heart-pulse")
          ),
          
          # Badge
          div(
            class = "home-badge",
            tags$i(class = "fa-solid fa-circle-nodes", style = "font-size:10px;"),
            "Projet Data Visualization · INP-HB / IDSI"
          ),
          
          # Titre
          h1(
            class = "home-title",
            "Student",
            tags$span("Mental Health"),
            " Dashboard"
          ),
          
          # Sous-titre
          p(
            class = "home-subtitle",
            "Exploration des indicateurs de santé mentale — dépression, anxiété, panique —
             chez les étudiants universitaires. Analyses croisées avec le profil académique
             et démographique."
          ),
          
          # KPI Snapshot (statiques, issus de la data globale)
          div(
            class = "home-kpi-grid",
            
            div(class = "home-kpi-card",
                div(class = "home-kpi-icon", tags$i(class = "fa-solid fa-users")),
                div(class = "home-kpi-value", nrow(data)),
                div(class = "home-kpi-label", "Étudiants")
            ),
            
            div(class = "home-kpi-card",
                div(class = "home-kpi-icon", tags$i(class = "fa-solid fa-face-sad-tear")),
                div(
                  class = "home-kpi-value",
                  paste0(round(mean(data$depression == "Yes") * 100, 1), "%")
                ),
                div(class = "home-kpi-label", "Dépression")
            ),
            
            div(class = "home-kpi-card",
                div(class = "home-kpi-icon", tags$i(class = "fa-solid fa-heart-crack")),
                div(
                  class = "home-kpi-value",
                  paste0(round(mean(data$anxiety == "Yes") * 100, 1), "%")
                ),
                div(class = "home-kpi-label", "Anxiété")
            ),
            
            div(class = "home-kpi-card",
                div(class = "home-kpi-icon", tags$i(class = "fa-solid fa-user-doctor")),
                div(
                  class = "home-kpi-value",
                  paste0(round(mean(data$treatment == "Yes") * 100, 1), "%")
                ),
                div(class = "home-kpi-label", "Suivi spécialiste")
            )
          ),
          
          # Bouton CTA → active l'onglet Profil
          actionButton(
            "go_to_profil",
            label   = tagList(
              tags$span("Accéder au Dashboard"),
              tags$i(class = "fa-solid fa-arrow-right")
            ),
            class   = "btn-enter-dashboard",
            onclick = "Shiny.setInputValue('active_tab', 'profil');
                       $('[data-value=profil]').tab('show');
                       $('.sidebar-menu li a[href=#]').removeClass('active');
                       $('[data-value=profil]').closest('li').addClass('active');"
          ),
          
          # Feature cards
          div(
            class = "home-features",
            
            div(class = "home-feature-card",
                div(class = "home-feature-icon",
                    tags$i(class = "fa-solid fa-user-graduate")),
                div(class = "home-feature-title", "Profil étudiant"),
                div(class = "home-feature-desc",
                    "Âge, genre, filière, statut marital, niveau académique.")
            ),
            
            div(class = "home-feature-card",
                div(class = "home-feature-icon",
                    tags$i(class = "fa-solid fa-brain")),
                div(class = "home-feature-title", "Santé mentale"),
                div(class = "home-feature-desc",
                    "Prévalence des troubles, recours aux soins, facettes par groupe.")
            ),
            
            div(class = "home-feature-card",
                div(class = "home-feature-icon",
                    tags$i(class = "fa-solid fa-chart-bar")),
                div(class = "home-feature-title", "Analyse croisée"),
                div(class = "home-feature-desc",
                    "CGPA vs troubles mentaux, corrélations, triple trouble.")
            )
            
            
          ),
          
          div(
            class = "home-footer",
            " Projet M1 Data Science ·",
            tags$strong(" INP-HB / IDSI"), " 2025-2026"
          )
        )
      ),
      
      #====================================================
      # ONGLET 1 : PROFIL ETUDIANT
      #====================================================
      
      tabItem(
        tabName = "profil",
        
        fluidRow(
          valueBoxOutput("n_students", width = 6),
          valueBoxOutput("avg_age",    width = 6)
        ),
        
        fluidRow(
          
          box(
            width  = 6,
            title  = "Distribution des âges",
            status = "primary",
            solidHeader = FALSE,
            plotlyOutput("age_plot", height = "280px")
          ),
          
          box(
            width  = 6,
            title  = "Répartition par genre",
            status = "primary",
            solidHeader = FALSE,
            plotlyOutput("gender_plot", height = "280px")
          )
          
        ),
        
        fluidRow(
          
          box(
            width  = 6,
            title  = "Top 10 filières",
            status = "primary",
            solidHeader = FALSE,
            plotlyOutput("course_plot", height = "280px")
          ),
          
          box(
            width  = 6,
            title  = "Statut marital",
            status = "primary",
            solidHeader = FALSE,
            plotlyOutput("marital_plot", height = "280px")
          )
          
        ),
        
        fluidRow(
          
          box(
            width  = 12,
            title  = "Distribution du CGPA selon le genre",
            status = "primary",
            solidHeader = FALSE,
            plotlyOutput("cgpa_gender", height = "300px")
          )
          
        )
        
      ),
      
      #====================================================
      # ONGLET 2 : SANTE MENTALE
      #====================================================
      
      tabItem(
        tabName = "mental",
        
        fluidRow(
          valueBoxOutput("depression_rate", width = 3),
          valueBoxOutput("anxiety_rate",    width = 3),
          valueBoxOutput("panic_rate",      width = 3),
          valueBoxOutput("treatment_rate",  width = 3)
        ),
        
        fluidRow(
          
          box(
            width  = 6,
            title  = "Prévalence des troubles",
            status = "primary",
            solidHeader = FALSE,
            plotlyOutput("prevalence_plot", height = "280px")
          ),
          
          box(
            width  = 6,
            title  = "Recours à un spécialiste",
            status = "primary",
            solidHeader = FALSE,
            plotlyOutput("treatment_plot", height = "280px")
          )
          
        ),
        
        fluidRow(
          
          box(
            width  = 12,
            title  = "Recours spécialiste selon le trouble",
            status = "primary",
            solidHeader = FALSE,
            plotlyOutput("specialist_trouble", height = "300px")
          )
          
        ),
        
        fluidRow(
          
          box(
            width  = 12,
            title  = "Troubles selon le genre",
            status = "primary",
            solidHeader = FALSE,
            tags$div(
              style = "text-align:center; padding:4px 0 10px; font-size:12px; color:#475569;",
              tags$b("F"), " = Female",
              tags$span(style = "margin:0 14px; color:#cbd5e1;", "·"),
              tags$b("M"), " = Male"
            ),
            plotlyOutput("facet_gender", height = "300px")
          )
          
        ),
        
        fluidRow(
          
          box(
            width  = 12,
            title  = "Troubles selon l'année d'étude",
            status = "primary",
            solidHeader = FALSE,
            tags$div(
              style = "text-align:center; padding:4px 0 10px; font-size:12px; color:#475569;",
              tags$b("1"), " = Year 1",
              tags$span(style = "margin:0 10px; color:#cbd5e1;", "·"),
              tags$b("2"), " = Year 2",
              tags$span(style = "margin:0 10px; color:#cbd5e1;", "·"),
              tags$b("3"), " = Year 3",
              tags$span(style = "margin:0 10px; color:#cbd5e1;", "·"),
              tags$b("4"), " = Year 4"
            ),
            plotlyOutput("facet_year", height = "300px")
          )
          
        ),
        
        fluidRow(
          
          box(
            width  = 12,
            title  = "Dépression par filière — Top 10",
            status = "primary",
            solidHeader = FALSE,
            plotOutput("lollipop_plot", height = "360px")
          )
          
        )
        
      ),
      
      #====================================================
      # ONGLET 3 : ANALYSE CROISEE
      #====================================================
      
      tabItem(
        tabName = "analyse",
        
        fluidRow(
          
          box(
            width  = 6,
            title  = "CGPA × Dépression",
            status = "danger",
            solidHeader = FALSE,
            plotlyOutput("cgpa_dep", height = "280px")
          ),
          
          box(
            width  = 6,
            title  = "CGPA × Anxiété",
            status = "warning",
            solidHeader = FALSE,
            plotlyOutput("cgpa_anx", height = "280px")
          )
          
        ),
        
        fluidRow(
          
          box(
            width  = 6,
            title  = "CGPA × Panique",
            status = "primary",
            solidHeader = FALSE,
            plotlyOutput("cgpa_panic", height = "280px")
          ),
          
          box(
            width  = 6,
            title  = "Affecte par les 3 trouples",
            status = "primary",
            solidHeader = FALSE,
            plotlyOutput("three_plot", height = "280px")
          )
          
        ),
        
        fluidRow(
          
          box(
            width  = 12,
            title  = "Matrice de corrélations",
            status = "primary",
            solidHeader = FALSE,
            plotOutput("corr_plot", height = "360px")
          )
          
        )
        
      )
    )
    
  )
  
 )
)

#====================================================
# SERVER
#====================================================

server <- function(input, output, session) {
  
  auth <- secure_server(
    
    check_credentials(
      credentials
    )
    
  )
  
  # Navigation depuis le bouton Home → onglet Profil
  observeEvent(input$go_to_profil, {
    updateTabItems(session, "active_tab", "profil")
  })
  
  #====================================================
  # FILTRES
  #====================================================
  
  df <- reactive({
    d <- data
    
    if (input$gender != "Tous")
      d <- d %>% filter(gender == input$gender)
    
    if (input$year != "Toutes")
      d <- d %>% filter(year == input$year)
    
    if (input$course != "Toutes")
      d <- d %>% filter(course == input$course)
    
    d
  })
  
  mental_long <- reactive({
    df() %>%
      pivot_longer(depression:panic,
                   names_to  = "trouble",
                   values_to = "value") %>%
      filter(trouble %in% input$troubles)
  })
  
  #====================================================
  # KPI — PROFIL
  #====================================================
  
  output$n_students <- renderValueBox({
    valueBox(
      value    = nrow(df()),
      subtitle = "Étudiants ",
      icon     = icon("users"),
      color    = "blue"
    )
  })
  
  output$avg_age <- renderValueBox({
    valueBox(
      value    = round(mean(df()$age), 1),
      subtitle = "Âge moyen",
      icon     = icon("user"),
      color    = "green"
    )
  })
  
  #====================================================
  # KPI — SANTE MENTALE
  #====================================================
  
  output$depression_rate <- renderValueBox({
    taux <- mean(df()$depression == "Yes") * 100
    valueBox(
      value    = paste0(round(taux, 1), "%"),
      subtitle = "Dépression",
      icon     = icon("face-sad-tear"),
      color    = ifelse(taux > 30, "red", "yellow")
    )
  })
  
  output$anxiety_rate <- renderValueBox({
    taux <- mean(df()$anxiety == "Yes") * 100
    valueBox(
      value    = paste0(round(taux, 1), "%"),
      subtitle = "Anxiété",
      icon     = icon("heart-crack"),
      color    = ifelse(taux > 30, "red", "yellow")
    )
  })
  
  output$panic_rate <- renderValueBox({
    taux <- mean(df()$panic == "Yes") * 100
    valueBox(
      value    = paste0(round(taux, 1), "%"),
      subtitle = "Panique",
      icon     = icon("exclamation-triangle"),
      color    = ifelse(taux > 20, "red", "yellow")
    )
  })
  
  output$treatment_rate <- renderValueBox({
    taux <- mean(df()$treatment == "Yes") * 100
    valueBox(
      value    = paste0(round(taux, 1), "%"),
      subtitle = "Suivi spécialiste",
      icon     = icon("user-doctor"),
      color    = "green"
    )
  })
  
  #====================================================
  # PROFIL — GRAPHIQUES
  # Améliorations :
  # - fill = teal cohérent
  # - labels d'axes propres
  # - tooltips explicites
  #====================================================
  
  output$age_plot <- renderPlotly({
    
    p <- ggplot(df(), aes(age)) +
      geom_histogram(
        bins = 12,
        fill = "#1da87a",
        color = "white"
      ) +
      labs(
        x = "Âge",
        y = "Effectif"
      )
    
    ggplotly(p) %>%
      style(
        hovertemplate =
          "Âge : %{x}<br>Effectif : %{y}<extra></extra>"
      ) %>%
      layout(showlegend = FALSE)
    
  })
  
  output$gender_plot <- renderPlotly({
    d <- df() %>% count(gender) %>%
      mutate(pct = round(n / sum(n) * 100, 1))
    
    p <- ggplot(d, aes(gender, n, fill = gender,
                       text = paste0(gender, " : ", n, " (", pct, "%)"))) +
      geom_col(width = 0.6, show.legend = FALSE) +
      scale_fill_manual(values = PALETTE_GENDER) +
      labs(x = "Genre", y = "Effectif")
    
    ggplotly(p, tooltip = "text") %>%
      layout(showlegend = FALSE)
  })
  
  output$course_plot <- renderPlotly({
    top <- top_courses(df())
    
    p <- ggplot(top, aes(reorder(course, n), n,
                         text = paste0(course, " : ", n, " étudiants"))) +
      geom_col(fill = "#2563eb", width = 0.7) +
      coord_flip() +
      labs(x = NULL, y = "Effectif")
    
    ggplotly(p, tooltip = "text") %>%
      layout(showlegend = FALSE)
  })
  
  output$marital_plot <- renderPlotly({
    d <- df() %>% count(marital_status) %>%
      mutate(pct = round(n / sum(n) * 100, 1))
    
    p <- ggplot(d, aes(marital_status, n,
                       fill = marital_status,
                       text = paste0(marital_status, " : ", n, " (", pct, "%)"))) +
      geom_col(width = 0.6, show.legend = FALSE) +
      scale_fill_manual(values = c("#7c3aed", "#db2777", "#d97706", "#0891b2")) +
      labs(x = "Statut marital", y = "Effectif")
    
    ggplotly(p, tooltip = "text") %>%
      layout(showlegend = FALSE)
  })
  
  output$cgpa_gender <- renderPlotly({
    # Calcul de la proportion par genre + niveau CGPA pour le tooltip
    d <- df() %>%
      mutate(cgpa_level = factor(cgpa_level,
                                 levels = c("Excellent", "Bon", "Moyen", "Faible"))) %>%
      count(gender, cgpa_level) %>%
      group_by(gender) %>%
      mutate(prop = round(n / sum(n) * 100, 1)) %>%
      ungroup()
    
    p <- ggplot(
      d,
      aes(
        gender,
        y    = n,
        fill = cgpa_level,
        text = paste0(
          "Genre : ", gender,
          "<br>Niveau CGPA : ", cgpa_level,
          "<br>Effectif : ", n,
          "<br>Proportion : ", prop, "%"
        )
      )
    ) +
      geom_bar(position = "fill", stat = "identity", width = 0.65) +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(
        values = c(
          "Excellent" = "#1e3a8a",
          "Bon"       = "#2563eb",
          "Moyen"     = "#60a5fa",
          "Faible"    = "#bfdbfe"
        ),
        name = "Niveau CGPA"
      ) +
      labs(
        x        = "Genre",
        y        = "Proportion",
        subtitle = "Répartition du niveau de CGPA selon le genre"
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "v", x = 1.02, y = 1))
  })
  
  #====================================================
  # SANTE MENTALE — GRAPHIQUES
  #====================================================
  
  output$prevalence_plot <- renderPlotly({
    prev <- mental_prevalence(df())
    
    
    prev <- prev %>%
      mutate(Trouble_key = tolower(Trouble))
    
    couleurs <- c(
      "depression" = "#e11d48",
      "anxiety"    = "#d97706",
      "panic"      = "#7c3aed"
    )
    
    p <- ggplot(prev, aes(
      Trouble, Taux,
      fill = Trouble_key,
      text = paste0(Trouble, " : ", round(Taux, 1), "%")
    )) +
      geom_col(width = 0.6, show.legend = FALSE) +
      scale_fill_manual(values = couleurs) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      labs(x = NULL, y = "Prévalence (%)",
           subtitle = "% d'étudiants déclarant le trouble")
    
    ggplotly(p, tooltip = "text") %>%
      layout(showlegend = FALSE)
  })
  
  output$treatment_plot <- renderPlotly({
    d <- df() %>% count(treatment) %>%
      mutate(pct = round(n / sum(n) * 100, 1))
    
    p <- ggplot(d, aes(treatment, n, fill = treatment,
                       text = paste0(treatment, " : ", n, " (", pct, "%)"))) +
      geom_col(width = 0.5, show.legend = FALSE) +
      scale_fill_manual(values = c("Yes" = "#1da87a", "No" = "#e2e8f0")) +
      labs(x = "Suivi spécialiste", y = "Effectif")
    
    ggplotly(p, tooltip = "text") %>%
      layout(showlegend = FALSE)
  })
  
  output$specialist_trouble <- renderPlotly({
    long <- df() %>%
      pivot_longer(depression:panic, names_to = "trouble", values_to = "etat") %>%
      filter(etat == "Yes")
    
    p <- ggplot(long, aes(trouble, fill = treatment)) +
      geom_bar(position = "fill", width = 0.65) +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(
        values = c("Yes" = "#1da87a", "No" = "#f43f5e"),
        name   = "Suivi spécialiste",
        labels = c("Yes" = "Oui", "No" = "Non")
      ) +
      labs(x = "Trouble", y = "Proportion",
           subtitle = "Parmi les étudiants déclarant le trouble")
    
    ggplotly(p) %>%
      layout(legend = list(orientation = "h"))
  })
  
  output$facet_gender <- renderPlotly({
    d <- mental_long() %>%
      filter(value == "Yes") %>%
      count(gender, trouble) %>%
      mutate(
        gender_court = recode(gender, "Female" = "F", "Male" = "M"),
        trouble_fr = recode(trouble,
                            depression = "Dépression", anxiety = "Anxiété", panic = "Panique"),
        tooltip = paste0(gender, " — ", trouble_fr, "<br>Effectif : ", n)
      )
    
    p <- ggplot(d, aes(gender_court, n, fill = gender, text = tooltip)) +
      geom_col(width = 0.55, show.legend = FALSE) +
      scale_fill_manual(values = PALETTE_GENDER) +
      facet_wrap(~ trouble_fr) +
      labs(x = "Genre", y = "Effectif")
    
    ggplotly(p, tooltip = "text") %>%
      layout(showlegend = FALSE)
  })
  
  output$facet_year <- renderPlotly({
    d <- mental_long() %>%
      filter(value == "Yes") %>%
      count(year, trouble) %>%
      mutate(
        year_num   = as.integer(gsub("[^0-9]", "", as.character(year))),
        trouble_fr = recode(trouble,
                            depression = "Dépression", anxiety = "Anxiété", panic = "Panique"),
        tooltip = paste0(trouble_fr, " — Year ", year_num, "<br>Effectif : ", n)
      )
    
    p <- ggplot(d, aes(year_num, n, fill = trouble, text = tooltip)) +
      geom_col(width = 0.55, show.legend = FALSE) +
      scale_x_continuous(breaks = 1:4, labels = 1:4) +
      scale_fill_manual(values = PALETTE_TROUBLES_MULTI) +
      facet_wrap(~ trouble_fr) +
      labs(x = "Année", y = "Effectif")
    
    ggplotly(p, tooltip = "text") %>%
      layout(showlegend = FALSE)
  })
  
  #====================================================
  # ANALYSE CROISEE
  #====================================================
  
  output$cgpa_dep <- renderPlotly({
    p <- ggplot(df(), aes(
      cgpa_level, fill = depression,
      text = paste0("CGPA : ", cgpa_level, "<br>Dépression : ", depression)
    )) +
      geom_bar(position = "fill", width = 0.65) +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(
        values = c("Yes" = "#e11d48", "No" = "#e2e8f0"),
        name = "Dépression"
      ) +
      labs(x = "Niveau CGPA", y = "Proportion") +
      # Légende à droite au lieu du bas → libère l'axe X
      theme(legend.position = "right")
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        legend = list(
          orientation = "v",      # vertical = ne chevauche pas l'axe X
          x = 1.02, y = 0.5,
          title = list(text = "Dépression")
        )
      )
  })
  
  output$cgpa_anx <- renderPlotly({
    p <- ggplot(df(), aes(cgpa_level, fill = anxiety)) +
      geom_bar(position = "fill", width = 0.65) +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(
        values = c("Yes" = "#d97706", "No" = "#e2e8f0"),
        name = "Anxiété"
      ) +
      labs(x = "Niveau CGPA", y = "Proportion")
    
    ggplotly(p) %>%
      layout(
        legend = list(
          orientation = "h",
          x = 0.5, xanchor = "center",
          y = -0.25,           # sous le graphe, pas sur l'axe X
          title = list(text = "Anxiété : ")
        ),
        margin = list(b = 60)
      )
  })
  output$cgpa_panic <- renderPlotly({
    p <- ggplot(df(), aes(cgpa_level, fill = panic)) +
      geom_bar(position = "fill", width = 0.65) +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(
        values = c("Yes" = "#7c3aed", "No" = "#e2e8f0"),
        name = "Panique"
      ) +
      labs(x = "Niveau CGPA", y = "Proportion")
    
    ggplotly(p) %>%
      layout(legend = list(orientation = "h"))
  })
  
  output$three_plot <- renderPlotly({
    d <- df() %>%
      count(three_troubles) %>%
      mutate(
        pct   = round(n / sum(n) * 100, 1),
        key   = tolower(trimws(three_troubles)),
        label = paste0(three_troubles, "<br>Effectif : ", n, " (", pct, "%)")
      )
    
    couleurs <- c(
      "none"      = "#10b981",   # vert  — aucun trouble
      "one"       = "#60a5fa",   # bleu  — un trouble
      "two"       = "#f59e0b",   # amber — deux troubles
      "all three" = "#e11d48"    # rouge — trois troubles
    )
    
    p <- ggplot(d, aes(reorder(three_troubles, n), n,
                       fill = key,
                       text = label)) +
      geom_col(width = 0.6, show.legend = FALSE) +
      coord_flip() +
      scale_fill_manual(values = couleurs) +
      labs(x = NULL, y = "Effectif",
           subtitle = "Nombre de troubles cumulés")
    
    ggplotly(p, tooltip = "text") %>%
      layout(showlegend = FALSE)
  })
  
  output$corr_plot <- renderPlot({
    corr_data <- df() %>%
      transmute(
        Dépression = ifelse(depression == "Yes", 1, 0),
        Anxiété    = ifelse(anxiety    == "Yes", 1, 0),
        Panique    = ifelse(panic      == "Yes", 1, 0)
      )
    corrplot(
      cor(corr_data),
      method      = "color",
      type        = "upper",
      col         = colorRampPalette(c("#e11d48", "white", "#1da87a"))(200),
      tl.col      = "#334155",
      tl.srt      = 45,
      tl.cex      = 1.1,
      addCoef.col = "#334155",
      number.cex  = 1.1,
      bg          = "white"
    )
  })
  
  
  
  #====================================================
  # GEOM PERSONNALISE
  #====================================================
  
  output$lollipop_plot <- renderPlot({
    temp <- df() %>%
      group_by(course) %>%
      summarise(depression = mean(depression == "Yes") * 100) %>%
      arrange(desc(depression)) %>%
      slice(1:10)
    
    ggplot(temp, aes(reorder(course, depression), depression)) +
      geom_lollipop() +
      coord_flip() +
      labs(
        x        = NULL,
        y        = "Prévalence de la dépression (%)",
        title    = "Dépression par filière",
        subtitle = "Top 10 filières — étudiants déclarant une dépression"
      )
  })
  
}

#====================================================
# LANCEMENT
#====================================================

shinyApp(ui = ui, server = server)