# =====================================================
# PLOT FUNCTIONS — plot_functions.R
# Toutes les fonctions graphiques du dashboard
# Appelées depuis app.R via renderPlot()
# =====================================================

library(ggplot2)
library(tidyverse)

# =====================================================
# THEME COMMUN
# =====================================================

theme_dash <- function() {
  theme_minimal(base_family = "sans") +
    theme(
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "#F1F5F9", linewidth = 0.5),
      panel.grid.minor = element_blank(),
      axis.text        = element_text(size = 10, color = "#475569"),
      axis.title       = element_text(size = 11, color = "#1E293B", face = "bold"),
      plot.title       = element_text(size = 13, face = "bold",
                                      color = "#1E293B", margin = margin(b = 6)),
      plot.subtitle    = element_text(size = 11, color = "#94A3B8",
                                      margin = margin(b = 12)),
      legend.position  = "bottom",
      legend.title     = element_text(size = 10, face = "bold", color = "#475569"),
      legend.text      = element_text(size = 10, color = "#475569"),
      legend.key.size  = unit(0.55, "cm"),
      strip.text       = element_text(size = 10, face = "bold", color = "#1E293B"),
      strip.background = element_rect(fill = "#F1F5F9", color = NA),
      plot.caption     = element_text(size = 9, color = "#94A3B8",
                                      hjust = 1, margin = margin(t = 8)),
      plot.margin      = margin(12, 16, 12, 16)
    )
}

# =====================================================
# PALETTES
# =====================================================

pal_dep <- c("Yes" = "#E11D48", "No" = "#94A3B8")
pal_anx <- c("Yes" = "#D97706", "No" = "#94A3B8")
pal_gen <- c("Male" = "#2563EB", "Female" = "#0D9488")
pal_risk <- c(
  "Normal"        = "#94A3B8",
  "Moderate Risk" = "#D97706",
  "High Risk"     = "#E11D48"
)

# =====================================================
# ONGLET PROFIL ETUDIANT
# =====================================================

plot_gender <- function(df) {
  df %>%
    count(gender) %>%
    ggplot(aes(gender, n, fill = gender)) +
    geom_col(width = 0.5, show.legend = FALSE) +
    geom_text(aes(label = n), vjust = -0.5,
              size = 3.5, fontface = "bold", color = "#1E293B") +
    scale_fill_manual(values = pal_gen) +
    labs(
      title    = "Répartition par genre",
      subtitle = paste0("n = ", nrow(df)),
      x = NULL, y = "Effectif"
    ) +
    theme_dash()
}

plot_year <- function(df) {
  df %>%
    count(year) %>%
    ggplot(aes(year, n, fill = n)) +
    geom_col(width = 0.6, show.legend = FALSE) +
    geom_text(aes(label = n), vjust = -0.5,
              size = 3.5, fontface = "bold", color = "#1E293B") +
    scale_fill_gradient(low = "#BFDBFE", high = "#2563EB") +
    labs(
      title = "Répartition par année d'études",
      x = "Année", y = "Effectif"
    ) +
    theme_dash()
}

plot_age <- function(df) {
  ggplot(df, aes(age)) +
    geom_histogram(binwidth = 1, fill = "#0D9488",
                   color = "white", alpha = 0.85) +
    labs(
      title    = "Distribution de l'âge",
      subtitle = "Histogramme (binwidth = 1 an)",
      x = "Âge", y = "Effectif"
    ) +
    theme_dash()
}

plot_course <- function(df) {
  df %>%
    count(course, sort = TRUE) %>%
    slice_head(n = 10) %>%
    ggplot(aes(reorder(course, n), n)) +
    geom_col(fill = "#D97706", width = 0.65, alpha = 0.9) +
    geom_text(aes(label = n), hjust = -0.2,
              size = 3.2, color = "#1E293B") +
    coord_flip() +
    labs(
      title = "Top 10 filières",
      x = NULL, y = "Effectif"
    ) +
    theme_dash()
}

# =====================================================
# ONGLET PERFORMANCE ACADEMIQUE
# =====================================================

plot_hist_cgpa <- function(df) {
  ggplot(df, aes(cgpa_num, fill = depression)) +
    geom_histogram(bins = 12, alpha = 0.8,
                   position = "identity", color = "white") +
    scale_fill_manual(
      values = pal_dep,
      labels = c("Yes" = "Dépression", "No" = "Aucune"),
      name   = NULL
    ) +
    labs(
      title    = "Distribution du CGPA",
      subtitle = "Colorié selon la présence de dépression",
      x = "CGPA", y = "Effectif"
    ) +
    theme_dash()
}

plot_density_cgpa <- function(df) {
  ggplot(df, aes(cgpa_num, colour = anxiety, fill = anxiety)) +
    geom_density(linewidth = 1, alpha = 0.12) +
    scale_colour_manual(
      values = pal_anx,
      labels = c("Yes" = "Anxiété", "No" = "Aucune"),
      name   = NULL
    ) +
    scale_fill_manual(
      values = pal_anx,
      labels = c("Yes" = "Anxiété", "No" = "Aucune"),
      name   = NULL
    ) +
    labs(
      title    = "Densité du CGPA",
      subtitle = "Selon la présence d'anxiété",
      x = "CGPA", y = "Densité"
    ) +
    theme_dash()
}

plot_boxplot_cgpa <- function(df) {
  ggplot(df, aes(depression, cgpa_num, fill = gender)) +
    geom_boxplot(
      alpha           = 0.8,
      outlier.colour  = "#E11D48",
      outlier.size    = 1.8
    ) +
    scale_fill_manual(
      values = pal_gen,
      labels = c("Male" = "Homme", "Female" = "Femme"),
      name   = "Genre"
    ) +
    scale_x_discrete(
      labels = c("No" = "Pas de dépression", "Yes" = "Dépression")
    ) +
    labs(
      title    = "CGPA selon dépression et genre",
      subtitle = "Boxplot comparatif",
      x = NULL, y = "CGPA"
    ) +
    theme_dash()
}

# =====================================================
# ONGLET BIEN-ETRE
# =====================================================

plot_panic <- function(df) {
  df %>%
    count(panic) %>%
    ggplot(aes(panic, n, fill = panic)) +
    geom_col(width = 0.45, show.legend = FALSE) +
    geom_text(aes(label = n), vjust = -0.5,
              size = 3.5, fontface = "bold", color = "#1E293B") +
    scale_fill_manual(
      values = c("Yes" = "#E11D48", "No" = "#94A3B8")
    ) +
    scale_x_discrete(
      labels = c("Yes" = "Oui", "No" = "Non")
    ) +
    labs(
      title    = "Crises de panique",
      subtitle = "Présence déclarée",
      x = NULL, y = "Effectif"
    ) +
    theme_dash()
}

plot_treatment <- function(df) {
  df %>%
    count(treatment) %>%
    ggplot(aes(treatment, n, fill = treatment)) +
    geom_col(width = 0.45, show.legend = FALSE) +
    geom_text(aes(label = n), vjust = -0.5,
              size = 3.5, fontface = "bold", color = "#1E293B") +
    scale_fill_manual(
      values = c("Yes" = "#0D9488", "No" = "#94A3B8")
    ) +
    scale_x_discrete(
      labels = c("Yes" = "Oui", "No" = "Non")
    ) +
    labs(
      title    = "Suivi d'un traitement",
      subtitle = "Étudiant sous traitement médical",
      x = NULL, y = "Effectif"
    ) +
    theme_dash()
}

plot_risk_profile <- function(df) {
  df %>%
    count(risk_profile) %>%
    ggplot(aes(risk_profile, n, fill = risk_profile)) +
    geom_col(width = 0.55, show.legend = FALSE) +
    geom_text(aes(label = n), vjust = -0.5,
              size = 3.5, fontface = "bold", color = "#1E293B") +
    scale_fill_manual(values = pal_risk) +
    labs(
      title    = "Profils de risque",
      subtitle = "Basé sur dépression + CGPA",
      x = NULL, y = "Effectif"
    ) +
    theme_dash()
}

# =====================================================
# ONGLET ANALYSE MULTIVARIEE
# =====================================================

plot_facet <- function(df) {
  ggplot(df, aes(age, cgpa_num, colour = anxiety)) +
    geom_point(size = 2.2, alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
    scale_colour_manual(
      values = pal_anx,
      labels = c("Yes" = "Anxiété", "No" = "Aucune"),
      name   = NULL
    ) +
    facet_wrap(~year, nrow = 2) +
    labs(
      title    = "CGPA vs Âge par année d'études",
      subtitle = "Colorié selon l'anxiété — tendance linéaire",
      x = "Âge", y = "CGPA"
    ) +
    theme_dash()
}

plot_scatter <- function(df) {
  ggplot(df, aes(age, cgpa_num,
                 color = depression,
                 shape = panic)) +
    geom_point(size = 2.5, alpha = 0.75) +
    scale_colour_manual(
      values = pal_dep,
      labels = c("Yes" = "Dépression", "No" = "Aucune"),
      name   = "Dépression"
    ) +
    scale_shape_manual(
      values = c("Yes" = 17, "No" = 16),
      labels = c("Yes" = "Oui", "No" = "Non"),
      name   = "Panique"
    ) +
    labs(
      title    = "CGPA vs Âge",
      subtitle = "Couleur = dépression  |  Forme = panique",
      x = "Âge", y = "CGPA"
    ) +
    theme_dash()
}

plot_heatmap <- function(df) {
  df %>%
    count(depression, year) %>%
    group_by(year) %>%
    mutate(pct = n / sum(n) * 100) %>%
    filter(depression == "Yes") %>%
    ggplot(aes(year, 1, fill = pct)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = paste0(round(pct, 1), "%")),
              size = 4, fontface = "bold", color = "white") +
    scale_fill_gradient(
      low  = "#FECDD3",
      high = "#E11D48",
      name = "% dépression"
    ) +
    labs(
      title    = "Taux de dépression par année d'études",
      subtitle = "% d'étudiants déclarant une dépression",
      x = "Année d'études", y = NULL
    ) +
    theme_dash() +
    theme(
      axis.text.y  = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid   = element_blank()
    )
}

# =====================================================
# ONGLET PROFILS A RISQUE
# =====================================================

plot_risk <- function(df) {

  if (nrow(df) == 0) {
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Aucun profil à risque dans la sélection",
                 size = 5, color = "#94A3B8", fontface = "italic") +
        theme_void() +
        theme(plot.background = element_rect(fill = "white", color = NA))
    )
  }

  ggplot(df, aes(age, cgpa_num, color = gender, size = cgpa_num)) +
    geom_point(alpha = 0.8) +
    scale_color_manual(
      values = pal_gen,
      labels = c("Male" = "Homme", "Female" = "Femme"),
      name   = "Genre"
    ) +
    scale_size_continuous(range = c(3, 8), guide = "none") +
    geom_hline(
      yintercept = 2.5,
      linetype   = "dashed",
      color      = "#E11D48",
      linewidth  = 0.8
    ) +
    annotate(
      "text",
      x     = min(df$age, na.rm = TRUE) + 0.3,
      y     = 2.58,
      label = "Seuil critique = 2.5",
      size  = 3,
      color = "#E11D48",
      hjust = 0
    ) +
    labs(
      title   = "Profils étudiants à risque",
      subtitle = "Dépression + CGPA < 2.5  |  Taille = CGPA",
      x       = "Âge",
      y       = "CGPA",
      caption = paste0(nrow(df), " étudiant(s) identifié(s)")
    ) +
    theme_dash()
}

# =====================================================
# ONGLET GEOM GGPROTO — utilise geom_mentalhealth()
# défini dans custom_geom.R
# =====================================================

plot_custom_geom <- function(df) {
  df$flag <- (df$depression == "Yes" | df$anxiety == "Yes")

  ggplot(df, aes(age, cgpa_num, flag = flag)) +
    geom_mentalhealth() +
    labs(
      title    = "Visualisation avec geom_mentalhealth()",
      subtitle = "Géom ggproto personnalisé — croix rouge = dépression OU anxiété",
      x = "Âge", y = "CGPA"
    ) +
    theme_dash()
}
