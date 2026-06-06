# 🧠 Tableau de Bord Interactif sur la Santé Mentale des Étudiants

## 📖 Présentation

Ce projet consiste en la réalisation d'une application interactive développée avec **R Shiny** permettant d'explorer et d'analyser les données relatives à la santé mentale des étudiants.

L'objectif est de mettre en évidence les facteurs associés à la dépression, à l'anxiété et aux attaques de panique, tout en étudiant leurs liens avec les caractéristiques démographiques et les performances académiques des étudiants.

---

## 🎯 Objectifs du projet

- Analyser le profil des étudiants.
- Mesurer la prévalence des troubles de santé mentale.
- Étudier les relations entre la santé mentale et le niveau académique (CGPA).
- Identifier les groupes d'étudiants les plus exposés aux troubles psychologiques.
- Fournir un outil interactif d'aide à la compréhension et à la visualisation des données.

---

## 🚀 Fonctionnalités

### 👨‍🎓 Profil des étudiants
- Distribution des âges
- Répartition par genre
- Répartition par filière
- Indicateurs clés (KPI)

### 🧠 Santé mentale
- Prévalence de la dépression
- Prévalence de l'anxiété
- Prévalence des attaques de panique
- Analyse des traitements suivis
- Analyse par année d'étude

### 📊 Analyses croisées
- Relation entre CGPA et dépression
- Relation entre CGPA et anxiété
- Relation entre CGPA et attaques de panique
- Matrice de corrélation
- Analyse des étudiants cumulant plusieurs troubles

### 📈 Visualisations avancées
- Graphiques interactifs avec Plotly
- Heatmap de corrélation
- Diagramme Lollipop personnalisé développé avec ggproto
- Tableau interactif des données

---

## 🛠️ Technologies utilisées

- R
- Shiny
- shinydashboard
- ggplot2
- Plotly
- DT
- corrplot
- Grid
- ggproto
- CSS

---

## 📂 Structure du projet

```text
StudentMentalHealthDashboard
│
├── app.R
├── README.md
│
├── data
│   └── Student_Mental_health.csv
│
├── R
│   ├── data_cleaning.R
│   ├── helpers.R
│   ├── custom_geom.R
│   └── grid_tools.R
│
├── www
│   ├── style.css
│   └── img
│
└── outputs
    └── screenshots
```

## 📊 Jeu de données

Le projet s'appuie sur le jeu de données **Student Mental Health Dataset**, contenant des informations relatives :

- à l'âge ;
- au genre ;
- à la filière d'étude ;
- au niveau académique (CGPA) ;
- à la dépression ;
- à l'anxiété ;
- aux attaques de panique ;
- aux traitements psychologiques suivis.

---

## 📸 Aperçu de l'application

### Tableau de bord principal

![Accueil](outputs/screenshots/dashboard_home.png)

### Analyse de la santé mentale

![Santé mentale](outputs/screenshots/mental_health.png)

### Analyses croisées

![Analyse croisée](outputs/screenshots/cross_analysis.png)

---

## 💡 Résultats principaux

L'analyse met en évidence :

- une prévalence importante de l'anxiété chez les étudiants ;
- des différences selon les filières et les années d'étude ;
- des relations entre certains troubles psychologiques et les performances académiques ;
- l'existence d'étudiants cumulant plusieurs troubles de santé mentale.

---

## 👨‍💻 Auteur

Projet réalisé dans le cadre du cours de **Visualisation de Données** en Master 1 Data Science.

---