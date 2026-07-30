# recodage vox 2024 avec l'initiative sur l'allègement des primes


# Je reprends une fonction de j'ai crééé afin de recoder une variable likert en 5 catégories en trois catégories tout en gardant les labels format sav-compatible
recode_likert <- function(df, labels = c("D'accord" = 1, "Ne sais pas / NA" = 2, "Pas d'accord" = 3)) {
  
  if (length(labels) != 3) {
    stop("Please provide exactly three labels.")
  }
  
  df %>%
    mutate(across(everything(), ~ case_when(
      . %in% c(1, 2) ~ 1,   # Code "voll und ganz einverstanden" and "eher einverstanden" as 1
      is.na(.) | . == 8 ~ 2,           # Code 8 [weiss nicht / keine Angabe] as 8
      . %in% c(3, 4) ~ 3,   # Code "überhaupt nicht einverstanden" and "überhaupt nicht einverstanden" as 3
      TRUE ~ as.numeric(.)  # Retain original values for non-Likert values
    ))) %>%
    # Adding labels
    mutate(across(everything(), ~ labelled(., labels = labels))) |> 
    rename_with(~ paste0("recoded_", .))
}

library(tidyverse)
library(sjlabelled)
library(haven)

datavox2024 <- read_sav("datavox2024.sav")

# les variables actives d'intérêt pour l'acl sont les variables zusatz1_* comprenant 8 questions

zusatz_cols <- paste0("ZUSATZ1_", 1:8)

zusatz_recoded_df <- 
datavox2024 |> 
  select(paste0("ZUSATZ1_", 1:8)) |> 
  recode_likert()

datavox2024_2 <- cbind(datavox2024, zusatz_recoded_df)


datavox2024_2 |> 
  count(as_label(ZUSATZ1_1)) |> 
  mutate(prop = n/sum(n))

write_sav(datavox2024_2, "recoded_datavox.sav")




datavox2024_3steps7clusters <- read_sav("datavox2024_3steps7clusters.sav")

library(tidyverse)
library(labelled)

# 1. Colonnes recodées et traductions des labels de variables (allemand -> français)
recoded_cols <- names(datavox2024_3steps7clusters)[startsWith(names(datavox2024_3steps7clusters), "recoded")]

labs_fr <- c(
  "ZUSATZ1_1" = "Augmentation de la TVA",
  "ZUSATZ1_2" = "Augmentation de l'impôt fédéral direct",
  "ZUSATZ1_3" = "Indexer les primes d'assurance-maladie sur le revenu",
  "ZUSATZ1_4" = "Créer une caisse unique",
  "ZUSATZ1_5" = "Supprimer l'assurance-maladie obligatoire",
  "ZUSATZ1_6" = "Renoncement personnel à des prestations médicales actuelles",
  "ZUSATZ1_7" = "Réduire les prestations prises en charge par l'assurance obligatoire",
  "ZUSATZ1_8" = "Restreindre l'accès aux prestations médicales (p. ex. limite d'âge)"
)

# 2. Réponses recodées en format long
id_col <- tibble(id = seq_len(nrow(datavox2024_3steps7clusters)))

resp_long <- datavox2024_3steps7clusters |>
  select(all_of(recoded_cols)) |>
  mutate(across(everything(), to_factor)) |>
  bind_cols(id_col) |>
  pivot_longer(all_of(recoded_cols), names_to = "variable", values_to = "response") |>
  mutate(variable = labs_fr[sub("^recoded_", "", variable)])

# 3. Probabilités postérieures d'appartenance aux clusters, en format long
weights_long <- datavox2024_3steps7clusters |>
  select(`clu#1`:`clu#7`) |>
  bind_cols(id_col) |>
  pivot_longer(-id, names_to = "cluster", values_to = "weight") |>
  mutate(cluster = paste("Cluster", parse_number(cluster)))

# 4. Crosstab pondéré par cluster (reproduit les paramètres du modèle LatentGold)
df_weighted <- resp_long |>
  left_join(weights_long, by = "id", relationship = "many-to-many") |>
  group_by(variable, cluster, response) |>
  summarise(w = sum(weight), .groups = "drop_last") |>
  mutate(pct = w / sum(w) * 100) |>
  ungroup()

# 5. Ligne "Overall" pondérée, ajoutée comme colonne "Total"
df_weighted_total <- df_weighted |>
  group_by(variable, response) |>
  summarise(w = sum(w), .groups = "drop_last") |>
  mutate(pct = w / sum(w) * 100, cluster = "Total") |>
  ungroup()

df_heat_final <- bind_rows(df_weighted, df_weighted_total) |>
  mutate(
    cluster = factor(cluster, levels = c(paste("Cluster", 1:7), "Total")),
    variable_wrap = str_wrap(variable, 22)
  )

# 6. Tailles de cluster pondérées (pour les libellés en haut du graphique)
cluster_sizes_post <- datavox2024_3steps7clusters |>
  summarise(across(`clu#1`:`clu#7`, sum)) |>
  pivot_longer(everything(), names_to = "cluster", values_to = "w") |>
  mutate(cluster_num = parse_number(cluster),
         pct_n = round(w / sum(w) * 100, 1),
         label = paste0("C", cluster_num, "\n(", pct_n, "%)"))

cluster_labels_post <- setNames(cluster_sizes_post$label, paste("Cluster", cluster_sizes_post$cluster_num))
cluster_labels_post["Total"] <- "Total\n(100%)"

# 7. Heatmap final
profile_plot <- 
ggplot(df_heat_final, aes(x = cluster, y = response, fill = pct)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(round(pct, 0), "%")), size = 2.9,
            color = ifelse(df_heat_final$pct > 55, "white", "black")) +
  facet_grid(rows = vars(variable_wrap), scales = "free_y", space = "free_y",
             switch = "y") +
  scale_x_discrete(position = "top", labels = cluster_labels_post) +
  scale_fill_gradient(low = "ghostwhite", high = "red4", name = "%") +
  labs(x = NULL, y = NULL,
       title = "",
       subtitle = "") +
  theme_minimal() +
  theme(
    strip.placement = "outside",
    strip.text.y.left = element_text(face = "bold", angle = 0, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7),
    axis.text.x.top = element_text(face = "bold", size = 7.5),
    panel.spacing.y = unit(0.5, "lines"),
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 0.4),
    legend.position = "none",
    legend.key.width = unit(1.2, "cm"),
    plot.title = element_text(size = 12)
  )



profile_plot

ggsave("profile_plot_7clusters.png", profile_plot)

# 8. Revenu (méthode du point médian) et décile de revenu
datavox2024_3steps7clusters <- datavox2024_3steps7clusters |>
  mutate(
    income = case_when(
      INCOME == 1  ~ (0 + 2000) / 2,
      INCOME == 2  ~ (2001 + 3000) / 2,
      INCOME == 3  ~ (3001 + 4000) / 2,
      INCOME == 4  ~ (4001 + 5000) / 2,
      INCOME == 5  ~ (5001 + 6000) / 2,
      INCOME == 6  ~ (6001 + 7000) / 2,
      INCOME == 7  ~ (7001 + 8000) / 2,
      INCOME == 8  ~ (8001 + 9000) / 2,
      INCOME == 9  ~ (9001 + 10000) / 2,
      INCOME == 10 ~ (10001 + 11000) / 2,
      INCOME == 11 ~ (11001 + 12000) / 2,
      INCOME == 12 ~ (12001 + 13000) / 2,
      INCOME == 13 ~ (13001 + 14000) / 2,
      INCOME == 14 ~ (14001 + 15000) / 2,
      # catégorie ouverte ("plus de 15'000 CHF") : borne inférieure faute de borne supérieure
      INCOME == 15 ~ 15001,
      INCOME == 98 ~ NA_real_
    ),
    decile = ntile(income, n = 10)
  )

# 9. Décile de revenu par cluster (pondéré par les probabilités postérieures)
decile_long <- datavox2024_3steps7clusters |>
  select(decile) |>
  bind_cols(id_col) |>
  filter(!is.na(decile))

decile_weighted <- decile_long |>
  left_join(weights_long, by = "id", relationship = "many-to-many") |>
  group_by(cluster, decile) |>
  summarise(w = sum(weight), .groups = "drop_last") |>
  mutate(pct = w / sum(w) * 100) |>
  ungroup()

decile_summary <- decile_long |>
  left_join(weights_long, by = "id", relationship = "many-to-many") |>
  group_by(cluster) |>
  summarise(decile_moyen = weighted.mean(decile, weight), .groups = "drop") |>
  arrange(decile_moyen)

ggplot(decile_weighted,
       aes(x = fct_reorder(cluster, as.numeric(sub("Cluster ", "", cluster))),
           y = factor(decile, levels = 10:1), fill = pct)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(pct, 0)), size = 3,
            color = ifelse(decile_weighted$pct > 15, "white", "black")) +
  scale_fill_gradient(low = "ghostwhite", high = "red4", name = "%") +
  labs(x = NULL, y = "Décile de revenu",
       title = "Distribution du décile de revenu par cluster (pondérée)") +
  theme_minimal() +
  theme(legend.position = "none")



