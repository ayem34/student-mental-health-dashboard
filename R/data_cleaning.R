library(tidyverse)

load_and_clean_data <- function() {
  #=====================================================
  # CHARGEMENT
  #=====================================================
  data <- read.csv(
    "data/Student_Mental_health.csv",
    stringsAsFactors = FALSE
  )
  
  
  
  names(data) <- c(
    "timestamp",
    "gender",
    "age",
    "course",
    "year",
    "cgpa",
    "marital_status",
    "depression",
    "anxiety",
    "panic",
    "treatment"
  )
  
  #=====================================================
  # NETTOYAGE
  #=====================================================
  
  # Harmonisation des années
  
  data$year <- case_when(
    str_detect(tolower(data$year), "1") ~ "Year 1",
    str_detect(tolower(data$year), "2") ~ "Year 2",
    str_detect(tolower(data$year), "3") ~ "Year 3",
    str_detect(tolower(data$year), "4") ~ "Year 4"
  )
  #=====================================================
  # IMPUTATION DE L'AGE MANQUANT
  #=====================================================
  data$age[is.na(data$age)] <-
    median(data$age, na.rm = TRUE)
  
  #=====================================================
  # CATEGORISATION DU CGPA
  #=====================================================
  data$cgpa_level <- case_when(
    data$cgpa %in%
      c("0 - 1.99","2.00 - 2.49") ~ "Faible",
    
    data$cgpa == "2.50 - 2.99" ~ "Moyen",
    
    data$cgpa == "3.00 - 3.49" ~ "Bon",
    
    TRUE ~ "Excellent"
  )
  
  #=====================================================
  # VARIABLES BINAIRES
  #=====================================================
  vars <- c(
    "marital_status",
    "depression",
    "anxiety",
    "panic",
    "treatment"
  )
  
  data[vars] <- lapply(
    data[vars],
    factor,
    levels = c("No","Yes"),
    ordered = TRUE
  )
  
  data$three_troubles <- ifelse(
    data$depression == "Yes" &
      data$anxiety == "Yes" &
      data$panic == "Yes",
    "Oui",
    "Non"
  )
  
  return(data)
}