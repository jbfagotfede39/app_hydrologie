# Application de suivi des débits en temps réel
# Jean-Baptiste Fagot

library(aquatools)
library(dplyr)
library(glue)
library(gt)
library(lubridate)
library(shiny)
library(shinydashboard)
library(sf)
library(stringr)
library(tibble)
library(tidyverse)

#### Éléments à afficher ####
url_mail_perso <- a("Jean-Baptiste Fagot", href="mailto:jean-baptiste.fagot@peche-jura.com")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(
    title = "Débits en temps réel"
  ),
  
  # setup a sidebar menu to be rendered server-side
  dashboardSidebar(
    collapsed = TRUE, 
    sidebarMenuOutput("sidebar")
  ),
  
  dashboardBody(
    tabItems(
      tabItem("tab_debits", 
              titlePanel("Débits en temps réel - Représentations graphiques"),
              uiOutput("plots")
      ), # Fin de tabItem
      tabItem("tab_shinyanki_sub_apropos", 
              titlePanel("Débits en temps réel - À propos"),
              includeMarkdown("NEWS.md")
      ) # Fin de tabItem
    ), # fin de tabItems
    
    # Bas de page de tous les tabItems
    fluidRow( # à rétablir mais fait planter l'application sans que je ne sache pourquoi
      column(
        width = 12,
        h5(tagList("Version 0.0.6 de l'application, déployée le 27/06/2025 par ", url_mail_perso))

      ) # Fermeture de column
    ) # Fermeture de fluidRow
  ) # fin de dashboardBody
) # fin de dashboardPage
