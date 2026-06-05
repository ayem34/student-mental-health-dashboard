library(tidyverse)

mental_prevalence <- function(data){
  tibble(
    Trouble = c("Depression", "Anxiety", "Panic"),
    Taux = c(
      mean(data$depression == "Yes") * 100,
      mean(data$anxiety == "Yes") * 100,
      mean(data$panic == "Yes") * 100
    )
  )
}

top_courses <- function(data){
  data |>
    count(course) |>
    arrange(desc(n)) |>
    slice(1:10)
}

# Nouvelles fonctions pour les KPIs individuels
depression_rate <- function(data) {
  mean(data$depression == "Yes", na.rm = TRUE) * 100
}

anxiety_rate <- function(data) {
  mean(data$anxiety == "Yes", na.rm = TRUE) * 100
}

panic_rate <- function(data) {
  mean(data$panic == "Yes", na.rm = TRUE) * 100
}

treatment_rate <- function(data) {
  mean(data$treatment == "Yes", na.rm = TRUE) * 100
}

# Fonction générique retournant un tableau (utile pour debug ou export)
all_mental_rates <- function(data) {
  tibble(
    indicateur = c("Dépression", "Anxiété", "Panique", "Recours spécialiste"),
    taux = c(
      depression_rate(data),
      anxiety_rate(data),
      panic_rate(data),
      treatment_rate(data)
    )
  )
}