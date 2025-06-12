# Application de suivi des débits en temps réel
# Jean-Baptiste Fagot

library(aquatools)
library(DBI)
library(dplyr)
# library(ggplot2)
library(glue)
library(fs)
# library(gt)
# library(gtsummary)
# library(hms)
library(lubridate)
library(magrittr)
library(markdown)
library(purrr)
# library(readr)
# library(readxl)
# library(scales)
# library(sf)
library(shiny)
library(stringr)
# library(tibble)

function(input, output, session) {

  #### Francisation ####
  # if(Sys.info()['sysname'] %>% str_replace("sysname", "") == "Darwin") Sys.setlocale("LC_TIME", "French") # OSX
  # if(Sys.info()['sysname'] %>% str_replace("sysname", "") == "Linux") Sys.setlocale("LC_TIME", "fr_FR.UTF-8") # Serveur linux
  Sys.setlocale(locale="fr_FR.UTF-8")
  
  #### Menus ####
  # logo <- "FD39blanc.png"
  
  output$sidebar <- renderMenu({
    sidebarMenu(
      # ),
      id = "tabs",
      menuItem("Débits", tabName = "tab_debits",
               icon = icon("arrows-rotate")),
      menuItem('À propos',
               tabName = 'tab_shinyanki_sub_apropos',
               icon = icon('info-circle'))
    ) # Fin de sidebarMenu
  })

  #### Données initiales ####
  #### Paramètres ####
  profondeur_jours <- 7
  departement_insee <- 39
  # buffer_valeurs_reference <- 0.5
  buffer_valeurs_reference <- 2
  
  #### Données de référence ####
  ##### Débits de référence #####
  debits_references <-
    "https://raw.githubusercontent.com/jbfagotfede39/app_hydrologie/refs/heads/main/data/debits_references_39.csv" %>% 
    read_csv2() %>% 
    mutate(across(chsta_module:chsta_q300, as.numeric))
  
  ##### Palette des débits de référence #####
  palette_debits_ref <-
    c("QMNA5" = "#007db8",
      "Module" = "#acd079",
      "Q2" = "#458B00", 
      "Q5" = "#FFFF00",
      "Q10" = "#FFB90F", 
      "Q20" = "#FF8C00", 
      "Q30" = "#FF3030", 
      "Q50" = "#B23AEE",
      "Q100" = "#8B4513",
      "Q300" = "#BABABA"
    )
  
  ##### Stations #####
  stations <-
    hydrologie.hubeau.stations(departement_insee) %>% 
    filter(en_service == TRUE) %>% 
    filter(!grepl("EDF|Valouson", libelle_site)) # Sites EDF indisponibles en ligne
  # view()
  
  stations_avec_debits_large <-
    stations %>% 
    select(code_site, code_station, libelle_site) %>% 
    mutate(chsta_coderhj = glue("{unique(libelle_site)} - {unique(code_station)}")) %>% 
    left_join(debits_references, join_by(code_site == chsta_codemo)) %>% 
    relocate(geometry, .after = "chsta_q300")
  
  stations_avec_debits_long <-
    stations_avec_debits_large %>% 
    st_drop_geometry() %>% 
    pivot_longer(cols = chsta_module:chsta_q300, names_to = "Seuil", values_to = "Valeur") %>% 
    filter(!is.na(Valeur)) %>% 
    mutate(Seuil = case_when(Seuil == "chsta_module" ~ "Module",
                             Seuil == "chsta_qmna5" ~ "QMNA5",
                             Seuil == "chsta_q2" ~ "Q2",
                             Seuil == "chsta_q5" ~ "Q5",
                             Seuil == "chsta_q10" ~ "Q10",
                             Seuil == "chsta_q20" ~ "Q20",
                             Seuil == "chsta_q30" ~ "Q30",
                             Seuil == "chsta_q50" ~ "Q50",
                             Seuil == "chsta_q100" ~ "Q100",
                             Seuil == "chsta_q300" ~ "Q300",
    ))
  
  #### Mesures ####
  ##### Collecte #####
  data_to_view <-
    stations %>% 
    # head(2) %>%
    group_split(code_station) %>% 
    map(~ hydrologie.hubeau("tr", .$code_station, today() - days(profondeur_jours))) %>% 
    list_rbind()
  
  ##### Remise en forme #####
  data_to_view_v2 <-
    data_to_view %>% 
    filter(grandeur_hydro == "Q") %>% 
    # mutate(chmes_coderhj = glue("{libelle_site} - {code_station}")) %>% 
    mutate(chmes_coderhj = code_station) %>% 
    left_join(stations_avec_debits_large %>% 
                select(code_station, chsta_coderhj), 
              join_by(chmes_coderhj == code_station)) %>% 
    mutate(chmes_coderhj = chsta_coderhj, .keep = "unused") %>% 
    mutate(chmes_valeur = resultat_obs/1000) %>% 
    mutate(chmes_typemesure = "Hydrologie") %>% 
    mutate(chmes_unite = "m3/s") %>% 
    # mutate(chmes_mo = "DREAL BFC") %>% # Ne fonctionne pas en l'état avec le contexte
    mutate(time = date_obs) %>% 
    formatage.date.heure() %>% 
    formatage.time()
  
  #### Représentation graphique ####
  figure <- function(data_to_view_v2){
    contexte <- data_to_view_v2 %>% chronique.contexte()
    
    stations_avec_debits_long_station <-
      stations_avec_debits_long %>% 
      filter(chsta_coderhj == contexte$station) %>% 
      filter(Valeur >= contexte$valeur_min/(1+buffer_valeurs_reference)) %>% 
      filter(Valeur <= contexte$valeur_max*(1+buffer_valeurs_reference))
    
    gg <- ggplot(data_to_view_v2, aes(time))
    gg <- gg + geom_line(aes(y = chmes_valeur), colour = "black")
    # gg <- gg + scale_x_date(date_labels = "%a %d/%m")
    gg <- gg + scale_x_datetime(date_labels = "%a %d/%m")
    gg <- gg + geom_hline(data = stations_avec_debits_long_station, aes(yintercept = Valeur, colour = Seuil))
    gg <- gg + geom_text(data = stations_avec_debits_long_station, aes(x = now() - days(8), y = 1.1*Valeur , label = Seuil, colour = Seuil), size = 4, fontface="bold")
    gg <- gg + theme_minimal()
    gg <- gg + scale_colour_manual(values = palette_debits_ref)
    # gg <- gg + theme(legend.position="none") # On supprime la légende
    gg <- gg + labs(title = contexte$station)
    gg <- gg + labs(x = NULL)
    gg <- gg + labs(y = expression(paste("Débit m"^"3","/s")))
    # gg <- gg + annotate()
    return(gg)
  }
  
  # Liste des stations
  stations_liste <- data_to_view_v2 %>% distinct(code_station) %>% pull(code_station)
  
  # Génération dynamique des graphiques
  output$plots <- renderUI({
    # Utilisation de imap pour itérer sur les stations
    plot_ui_list <- 
      stations_liste %>% 
      map(~ {
        plotname <- paste0("plot_", .x)
        plotOutput(plotname)
      })
    
    # Combinaison des éléments de la liste en un seul objet tagList
    tagList(plot_ui_list)
  })
  
  # Création des graphiques individuels
  data_to_view_v2 %>%
    group_split(code_station) %>%
    walk(~ {
      plotname <- paste0("plot_", unique(.x$code_station))
      output[[plotname]] <- renderPlot({
        print(figure(.x))
      })
    })
  
}