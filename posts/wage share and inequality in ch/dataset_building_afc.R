rm(list=ls())
library(tidyverse)
library(readxl)


# les donées sur la répartition des revenus (gini, revenu moyen et médian...) viennent de l'administration fédérale des contribution (AFC), 
# les données ont été trouvée sur cette page web https://www.estv.admin.ch/estv/fr/accueil/afc/statistiques-fiscales/statistiques-fiscales-general/statistiques-impot-federal-direct.html#-1783813562
# l'idée est de compiler ces données en un seul fichier sur R

# cliquer sur le lien ci dessus
# aller sous revenu -> Chiffres-clés statistiques des personnes physiques -> avec et sans versement d'un impot federal direct

# importer tout les fichier dans un seul fichier et tous les combiner en un seul jeux de données

# faire cela de façon systématique

# communes
files <- list.files(path = "data_AFC/", pattern = "*.xlsx", full.names = TRUE)


 process_file <- function(file, year) {
  read_excel(file, sheet = 3) %>%
    filter(Einheit == "Total") %>%
    mutate(year = year,
           ktnr = as.character(ktnr))
}

years = c(1997:2020)

files
years

data_list_communes <- map2(files, years, process_file)
AFC_data_communes <- bind_rows(data_list_communes)



# pour les canton (juste remplacer le numero de feuille dans excel)

process_file <- function(file, year) {
  read_excel(file, sheet = 4) %>% # changer feuille 3 pour feuille 4
    filter(Einheit == "Total") %>%
    mutate(year = year,
           ktnr = as.character(ktnr))
}

data_list_cantons <- map2(files, years, process_file)
AFC_data_cantons <- bind_rows(data_list_cantons)

# et enfin, pour la suisse en général


process_file <- function(file, year) {
  read_excel(file, sheet = "Schweiz - Suisse") %>% # changer feuille 3 pour feuille 4
    filter(Einheit == "Total") %>%
    mutate(year = year)
}

data_list_CH <- map2(files, years, process_file)
AFC_data_CH <- bind_rows(data_list_CH)

# sauvegarder les fichiers
write.csv(AFC_data_cantons,"~/GitHub/celalguney/posts/wage share and inequality in ch/AFC_data_canton.csv")
write.csv(AFC_data_communes, "~/GitHub/celalguney/posts/wage share and inequality in ch/AFC_data_communes.csv")
write.csv(AFC_data_CH, "~/GitHub/celalguney/posts/wage share and inequality in ch/AFC_data_CH.csv")


AFC_data_CH_contr <- read_csv("data_contribuable_only/AFC_data_CH_contr.csv")

AFC_data_CH %>% 
  ggplot()+
  aes(x = year, y = gini_reinka)+
  geom_line()+
  geom_line(data = AFC_data_CH_contr, aes(x = year, y = gini_reinka), color = "red")
  



