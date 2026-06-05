#====================================================
# PACKAGES
#====================================================

library(shiny)
library(shinydashboard)

library(tidyverse)
library(plotly)
library(DT)
library(corrplot)

source("R/data_cleaning.R")
source("R/custom_geom.R")
source("R/grid_tools.R")
source("R/helpers.R")

#====================================================
# DONNEES
#====================================================

data <- load_and_clean_data()

#====================================================
# UI
#====================================================

ui <- dashboardPage(
  
  dashboardHeader(
    title = "Student Mental Health Dashboard"
  ),
  
  dashboardSidebar(
    
    sidebarMenu(
      
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
      ),
      
      menuItem(
        "Visualisation avancée",
        tabName = "avancee",
        icon = icon("project-diagram")
      )
      
    ),
    
    hr(),
    
    selectInput(
      "gender",
      "Genre",
      choices = c(
        "Tous",
        sort(unique(data$gender))
      )
    ),
    
    selectInput(
      "year",
      "Année",
      choices = c(
        "Toutes",
        sort(unique(data$year))
      )
    ),
    
    selectInput(
      "course",
      "Filière",
      choices = c(
        "Toutes",
        sort(unique(data$course))
      )
    ),
    
    checkboxGroupInput(
      "troubles",
      "Troubles",
      choices = c(
        "depression",
        "anxiety",
        "panic"
      ),
      selected = c(
        "depression",
        "anxiety",
        "panic"
      )
    )
    
  ),
  
  dashboardBody(
    
   
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    
    tabItems(
      
      #====================================================
      # ONGLET 1
      #====================================================
      
      tabItem(
        
        tabName = "profil",
        
        fluidRow(
          
          valueBoxOutput("n_students", width = 6),
          
          valueBoxOutput("avg_age", width = 6)
          
        ),
        
        fluidRow(
          
          box(
            width = 6,
            title = "Distribution des âges",
            plotlyOutput("age_plot")
          ),
          
          box(
            width = 6,
            title = "Répartition par genre",
            plotlyOutput("gender_plot")
          )
          
        ),
        
        fluidRow(
          
          box(
            width = 6,
            title = "Top 10 filières",
            plotlyOutput("course_plot")
          ),
          
          box(
            width = 6,
            title = "Statut marital",
            plotlyOutput("marital_plot")
          )
          
        ),
        
        fluidRow(
          
          box(
            width = 12,
            title = "CGPA selon le genre",
            plotlyOutput("cgpa_gender")
          )
          
        )
        
      ),
      
      #====================================================
      # ONGLET 2
      #====================================================
      
      tabItem(
        
        tabName = "mental",
        
        # LIGNE DES KPI
        fluidRow(
          valueBoxOutput("depression_rate", width = 3),
          valueBoxOutput("anxiety_rate", width = 3),
          valueBoxOutput("panic_rate", width = 3),
          valueBoxOutput("treatment_rate", width = 3)
        ),
        
        
        fluidRow(
          
          box(
            width = 6,
            title = "Prévalence des troubles",
            plotlyOutput("prevalence_plot")
          ),
          
          box(
            width = 6,
            title = "Recours à un spécialiste",
            plotlyOutput("treatment_plot")
          )
          
        ),
        
        fluidRow(
          
          box(
            width = 12,
            title = "Recours spécialiste selon le trouble",
            plotlyOutput("specialist_trouble")
          )
          
        ),
        
        fluidRow(
          
          box(
            width = 12,
            title = "Troubles selon le genre",
            plotlyOutput("facet_gender")
          )
          
        ),
        
        fluidRow(
          
          box(
            width = 12,
            title = "Troubles selon l'année d'étude",
            plotlyOutput("facet_year")
          )
          
        ),
       
        
      ),
      
      #====================================================
      # ONGLET 3
      #====================================================
      
      tabItem(
        
        tabName = "analyse",
        
        fluidRow(
          
          box(
            width = 6,
            title = "CGPA × Dépression",
            plotlyOutput("cgpa_dep")
          ),
          
          box(
            width = 6,
            title = "CGPA × Anxiété",
            plotlyOutput("cgpa_anx")
          )
          
        ),
        
        fluidRow(
          
          box(
            width = 6,
            title = "CGPA × Panique",
            plotlyOutput("cgpa_panic")
          ),
          
          box(
            width = 6,
            title = "Triple trouble",
            plotlyOutput("three_plot")
          )
          
        ),
        
        fluidRow(
          
          box(
            width = 12,
            title = "Heatmap corrélations",
            plotOutput("corr_plot")
          )
          
        )
        
      ),
      
      #====================================================
      # ONGLET 4
      #====================================================
      
      tabItem(
        
        tabName = "avancee",
        
        fluidRow(
          
          box(
            width = 6,
            title = "Grid Layout",
            plotOutput("grid_plot")
          ),
          
          box(
            width = 6,
            title = "Geom Lollipop",
            plotOutput("lollipop_plot")
          )
          
        ),
        fluidRow(
          box(
            width = 6,
            title = "Grob Plot (ggplotGrob + grid.draw)",
            plotOutput("grob_plot")
          ),
          box(
            width = 6,
            title = "Annotation avec grid.text",
            plotOutput("annotation_plot")
          )
        )
        
      )
      
    )
    
  )
  
)

#====================================================
# SERVER
#====================================================

server <- function(input, output) {
  
  #====================================================
  # FILTRES
  #====================================================
  
  
  df <- reactive({
    
    d <- data
    
    if(input$gender != "Tous")
      d <- d %>% filter(gender == input$gender)
    
    if(input$year != "Toutes")
      d <- d %>% filter(year == input$year)
    
    if(input$course != "Toutes")
      d <- d %>% filter(course == input$course)
    
    d
    
  })
  
  
  mental_long <- reactive({
    
    df() %>%
      
      pivot_longer(
        depression:panic,
        names_to = "trouble",
        values_to = "value"
      ) %>%
      
      filter(
        trouble %in% input$troubles
      )
    
  })
  #====================================================
  # KPI
  #====================================================
  
  output$n_students <- renderValueBox({
    
    valueBox(
      value = nrow(df()),
      subtitle = "Étudiants",
      icon = icon("users"),
      color = "blue"
    )
    
  })
  
  output$avg_age <- renderValueBox({
    
    valueBox(
      value = round(mean(df()$age),1),
      subtitle = "Âge moyen",
      icon = icon("user"),
      color = "green"
    )
    
  })
  
  
  #====================================================
  # KPI (suite)
  #====================================================
  
  output$depression_rate <- renderValueBox({
    taux <- mean(df()$depression == "Yes") * 100
    valueBox(
      value = paste0(round(taux, 1), "%"),
      subtitle = "Taux de dépression",
      icon = icon("frown"),
      color = ifelse(taux > 30, "red", "yellow")
    )
  })
  
  output$anxiety_rate <- renderValueBox({
    taux <- mean(df()$anxiety == "Yes") * 100
    valueBox(
      value = paste0(round(taux, 1), "%"),
      subtitle = "Taux d'anxiété",
      icon = icon("heartbeat"),
      color = ifelse(taux > 30, "red", "yellow")
    )
  })
  
  output$panic_rate <- renderValueBox({
    taux <- mean(df()$panic == "Yes") * 100
    valueBox(
      value = paste0(round(taux, 1), "%"),
      subtitle = "Taux de panique",
      icon = icon("exclamation-triangle"),
      color = ifelse(taux > 20, "red", "yellow")
    )
  })
  
  output$treatment_rate <- renderValueBox({
    taux <- mean(df()$treatment == "Yes") * 100
    valueBox(
      value = paste0(round(taux, 1), "%"),
      subtitle = "Recours spécialiste",
      icon = icon("user-md"),
      color = "green"
    )
  })
  #====================================================
  # PROFIL
  #====================================================
  
  output$age_plot <- renderPlotly({
    
    p <- ggplot(df(), aes(age)) +
      geom_histogram(bins = 10)
    
    ggplotly(p)
    
  })
  
  output$gender_plot <- renderPlotly({
    
    p <- ggplot(df(), aes(gender)) +
      geom_bar()
    
    ggplotly(p)
    
  })
  
  output$course_plot <- renderPlotly({
    
    top <- top_courses(df())
    
    p <- ggplot(
      top,
      aes(reorder(course,n), n)
    ) +
      geom_col() +
      coord_flip()
    
    ggplotly(p)
    
  })
  
  output$marital_plot <- renderPlotly({
    
    p <- ggplot(
      df(),
      aes(marital_status)
    ) +
      geom_bar()
    
    ggplotly(p)
    
  })
  
  output$cgpa_gender <- renderPlotly({
    
    p <- ggplot(
      df(),
      aes(
        gender,
        fill = cgpa_level
      )
    ) +
      geom_bar()
    
    ggplotly(p)
    
  })
  
  #====================================================
  # SANTE MENTALE
  #====================================================
  
  output$prevalence_plot <- renderPlotly({
    
    prev <- mental_prevalence(df())
    
    p <- ggplot(
      prev,
      aes(Trouble,Taux)
    ) +
      geom_col()
    
    ggplotly(p)
    
  })
  
  #specialist_trouble
  
  output$treatment_plot <- renderPlotly({
    
    p <- ggplot(
      df(),
      aes(treatment)
    ) +
      geom_bar()
    
    ggplotly(p)
    
  })
  
  output$specialist_trouble <- renderPlotly({
    
    long <- df() %>%
      
      pivot_longer(
        depression:panic,
        names_to = "trouble",
        values_to = "etat"
      ) %>%
      
      filter(etat == "Yes")
    
    p <- ggplot(
      
      long,
      
      aes(
        trouble,
        fill = treatment
      )
      
    ) +
      
      geom_bar(
        position = "fill"
      ) +
      
      scale_y_continuous(
        labels = scales::percent
      ) +
      
      labs(
        x = "Trouble",
        y = "Proportion"
      ) +
      
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  ### facet_gender
  
  output$facet_gender <- renderPlotly({
    
    p <- ggplot(
      
      mental_long() %>%
        filter(value == "Yes"),
      
      aes(gender)
      
    ) +
      
      geom_bar() +
      
      facet_wrap(
        ~ trouble
      ) +
      
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  
  output$facet_year <- renderPlotly({
    
    p <- ggplot(
      
      mental_long() %>%
        filter(value == "Yes"),
      
      aes(year)
      
    ) +
      
      geom_bar() +
      
      facet_wrap(
        ~ trouble
      ) +
      
      theme_minimal()
    
    ggplotly(p)
    
  })
  #====================================================
  # ANALYSE CROISEE
  #====================================================
  
  output$cgpa_dep <- renderPlotly({
    
    p <- ggplot(
      df(),
      aes(
        cgpa_level,
        fill = depression
      )
    ) +
      geom_bar(position = "fill")
    
    ggplotly(p)
    
  })
  
  output$cgpa_anx <- renderPlotly({
    
    p <- ggplot(
      df(),
      aes(
        cgpa_level,
        fill = anxiety
      )
    ) +
      geom_bar(position = "fill")
    
    ggplotly(p)
    
  })
  
  output$cgpa_panic <- renderPlotly({
    
    p <- ggplot(
      df(),
      aes(
        cgpa_level,
        fill = panic
      )
    ) +
      geom_bar(position = "fill")
    
    ggplotly(p)
    
  })
  
  output$three_plot <- renderPlotly({
    
    p <- ggplot(
      df(),
      aes(three_troubles)
    ) +
      geom_bar()
    
    ggplotly(p)
    
  })
  
  output$corr_plot <- renderPlot({
    corr_data <- df() %>%
      transmute(
        Dépression = ifelse(depression=="Yes",1,0),
        Anxiété = ifelse(anxiety=="Yes",1,0),
        Panique = ifelse(panic=="Yes",1,0)
      )
    corrplot(cor(corr_data), 
             method = "color",
             type = "upper",
             col = colorRampPalette(c("#E74C3C", "white", "#2ECC71"))(200),
             tl.col = "black",
             tl.srt = 45,
             addCoef.col = "black",
             number.cex = 0.8)
  })
  #====================================================
  # GRID
  #====================================================
  
  output$grid_plot <- renderPlot({
    
    create_grid_dashboard(df())
    
  })
  
  
  output$grob_plot <- renderPlot({
    create_grob_plot(df())
  })
  
  output$annotation_plot <- renderPlot({
    create_annotation_plot(df())
  })
  #====================================================
  # GEOM PERSONNALISE
  #====================================================
  
  output$lollipop_plot <- renderPlot({
    
    temp <- df() %>%
      group_by(course) %>%
      summarise(
        depression =
          mean(depression=="Yes")*100
      ) %>%
      arrange(desc(depression)) %>%
      slice(1:10)
    
    ggplot(
      temp,
      aes(
        reorder(course,depression),
        depression
      )
    ) +
      geom_lollipop() +
      coord_flip() +
      labs(
        x = "Filière",
        y = "Prévalence (%)"
      ) +
      theme_minimal()
    
  })
  
}

#====================================================
# LANCEMENT
#====================================================

shinyApp(
  ui = ui,
  server = server
)