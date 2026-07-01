#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| echo: false
#| warning: false
#| message: false

library(rdbnomics)
library(tidyverse)
library(patchwork)

# --- Import données ---
hicp_ch_ea <- rdb("Eurostat", "PRC_HICP_MIDX",
                  dimensions = list(coicop = "CP00", unit = "I05", geo = c("CH", "EA")))

rates_raw <- rdb("BIS", "WS_CBPOL", dimensions = list(REF_AREA = c("CH", "XM")))

# --- Mise en forme taux directeurs (mensuel) ---
rates <- rates_raw |>
  filter(str_detect(series_name, "Monthly")) |>
  select(country = REF_AREA, period, value) |>
  mutate(
    country = case_when(
      country == "CH" ~ "BNS (Suisse)",
      country == "XM" ~ "BCE (Zone euro)"
    ),
    period = as.Date(period)
  ) |>
  filter(period >= as.Date("2005-01-01"))

# --- Inflation glissement annuel ---
hicp_yoy <- hicp_ch_ea |>
  select(geo, period, value) |>
  mutate(
    country = if_else(geo == "CH", "Suisse", "Zone euro"),
    yoy     = (value / lag(value, 12) - 1) * 100,
    .by = geo
  ) |>
  filter(!is.na(yoy), period >= as.Date("2005-01-01"))

# --- Zone grisée 2008-2015 ---
shade <- data.frame(
  xmin = as.Date("2008-09-01"), xmax = as.Date("2015-01-01"),
  ymin = -Inf, ymax = Inf
)

# --- Graphique 1 : inflation ---
p1 <- ggplot(hicp_yoy, aes(x = period, y = yoy, colour = country)) +
  geom_rect(data = shade, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            inherit.aes = FALSE, fill = "grey85", alpha = 0.5) +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50") +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = c("Suisse" = "#c0392b", "Zone euro" = "#2980b9")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "Inflation (HICP, glissement annuel)", y = "%", x = NULL, colour = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top")

# --- Graphique 2 : taux directeurs ---
p2 <- ggplot(rates, aes(x = period, y = value, colour = country)) +
  geom_rect(data = shade, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            inherit.aes = FALSE, fill = "grey85", alpha = 0.5) +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50") +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = c("BNS (Suisse)" = "#c0392b", "BCE (Zone euro)" = "#2980b9")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "Taux directeurs (BNS vs BCE)", y = "%", x = NULL, colour = NULL,
       caption = "Zone grisée : 2008–2015   |   Source : Eurostat, BIS via DBnomics") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top")

p2_inset <- p2 + labs(caption = NULL, title = "Taux directeurs") +
  theme(legend.key.size = unit(0.4, "cm"),
        legend.text = element_text(size = 7),
        plot.title = element_text(size = 9),
        axis.text = element_text(size = 7))

p1 + inset_element(p2_inset, left = 0, bottom = 0.52, right = 0.44, top = 1)
#
#
#
#
#
#| echo: false
#| warning: false
#| message: false

library(tidyverse)
library(rdbnomics)

codes_variables <- c("BEL.3.0.0.0.ZCPIN", "CHE.3.0.0.0.ZCPIN", "DEU.3.0.0.0.ZCPIN", "DNK.3.0.0.0.ZCPIN", "FRA.3.0.0.0.ZCPIN", "GBR.3.0.0.0.ZCPIN", "ITA.3.0.0.0.ZCPIN", "LUX.3.0.0.0.ZCPIN", "USA.3.0.0.0.ZCPIN", "JPN.3.0.0.0.ZCPIN", "FIN.3.0.0.0.ZCPIN", "AUT.3.0.0.0.ZCPIN", "NOR.3.0.0.0.ZCPIN", "SWE.3.0.0.0.ZCPIN", "DNK.3.0.0.0.ZCPIN", "NLD.3.0.0.0.ZCPIN", "KOR.3.0.0.0.ZCPIN", "ESP.3.0.0.0.ZCPIN", "ISL.3.0.0.0.ZCPIN", "EA21.3.0.0.0.ZCPIN")


data_cpi_ameco <- 
codes_variables %>% 
  map_dfr(~rdb(paste("AMECO/ZCPIN", .x, sep = "/"))) %>% 
  select(Country, original_period, value) %>% 
  drop_na()

data_cpi_ameco$original_period = as.numeric(data_cpi_ameco$original_period)

data_cpi_ameco <- 
  data_cpi_ameco %>% 
  group_by(Country) %>% 
  mutate(
    inflation_rate = c(NA, diff(log(value)))
  ) %>% 
  ungroup()

library(gghighlight)

data_cpi_ameco %>% 
  filter(original_period %in% c(1961:2025)) %>% 
  ggplot(aes(x = original_period, y = inflation_rate, color = Country))+
  geom_line(size = 1)+
  geom_point()+
  gghighlight(Country %in% c("Switzerland"))



#
#
#
#
#
#
#
#
#| echo: false
#| warning: false
#| message: false

data_cpi_ameco |>
  filter(original_period %in% c(2000:2025)) %>% 
  group_by(Country) |>
  summarise(avg_inflation = mean(inflation_rate, na.rm = TRUE), .groups = "drop") |>
  arrange(avg_inflation)

#
#
#
#
#
#| echo: false
#| warning: false
#| message: false

library(rdbnomics)
library(tidyverse)

codes_variables = c("CHE.1.0.0.0.ZCPIH", "DEU.1.0.0.0.ZCPIH", "EU27.1.0.0.0.ZCPIH", "FRA.1.0.0.0.ZCPIH", "AUT.1.0.0.0.ZCPIH")

data_hcpi_ameco <- 
codes_variables %>% 
  map_dfr(~rdb(paste("AMECO/ZCPIH", .x, sep = "/"))) %>% 
  select(Country, original_period, value) %>% 
  drop_na()

data_hcpi_ameco$original_period = as.numeric(data_hcpi_ameco$original_period)


data_hcpi_ameco %>% 
  filter(original_period %in% c(2005:2025)) %>% 
  ggplot(aes(x = original_period, y = value, color = Country, group = Country))+
  geom_line()+
  geom_point()+
  theme_minimal()


#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| echo: false
#| warning: false
#| message: false

library(tidyverse)

df_lc <- read.csv("low-carbon-share-energy.csv")

df_lc |>
  mutate(
    Entity = fct_reorder(Entity, Low.carbon.energy),
    groupe = case_when(
      Entity == "Switzerland" ~ "Suisse",
      Entity == "Europe"      ~ "Moyenne Europe",
      TRUE                    ~ "Autres pays"
    )
  ) |>
  ggplot(aes(y = Entity, x = Low.carbon.energy, fill = groupe)) +
  geom_col(width = 0.6) +
  geom_vline(
    xintercept = df_lc$Low.carbon.energy[df_lc$Entity == "Europe"],
    linetype = "dashed", colour = "grey40", linewidth = 0.6
  ) +
  geom_text(aes(label = paste0(round(Low.carbon.energy, 1), "%")),
            hjust = -0.2, size = 3.2) +
  scale_x_continuous(limits = c(0, 92), labels = scales::label_percent(scale = 1)) +
  scale_fill_manual(values = c(
    "Suisse"         = "#c0392b",
    "Moyenne Europe" = "#7f8c8d",
    "Autres pays"    = "#2980b9"
  )) +
  labs(
    title   = "Part des énergies bas carbone dans la consommation primaire (2024)",
    x       = "% de l'énergie primaire",
    y       = NULL,
    fill    = NULL,
    caption = "Source : Our World in Data / Energy Institute"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top", panel.grid.major.y = element_blank())
#
#
#
#| echo: false
#| warning: false
#| message: false

library(jsonlite)

# Fetch the data
df <- read.csv("https://ourworldindata.org/grapher/share-energy-source-sub.csv?v=1&csvType=full&useColumnShortNames=true")

# Fetch the metadata
metadata <- fromJSON("https://ourworldindata.org/grapher/share-energy-source-sub.metadata.json?v=1&csvType=full&useColumnShortNames=true")


energy_colors <- c(
  "Oil"              = "#4e3b2e",
  "Coal"             = "#6b6b6b",
  "Gas"              = "#e07b39",
  "Nuclear"          = "#8e44ad",
  "Hydro"            = "#2980b9",
  "Wind"             = "#1abc9c",
  "Solar"            = "#f1c40f",
  "Other Renewables" = "#27ae60"
)

df %>%
  filter(entity == "Switzerland") %>%
  pivot_longer(
    cols = ends_with("_pct_equivalent_primary_energy"),
    names_to = "source",
    values_to = "share"
  ) %>%
  mutate(source = source %>%
           str_remove("__pct_equivalent_primary_energy") %>%
           str_replace_all("_", " ") %>%
           str_to_title()) %>%
  ggplot(aes(x = year, y = share, color = source)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = energy_colors) +
  labs(
    title = "Switzerland: Energy Mix",
    subtitle = "Share of primary energy by source",
    x = NULL, y = "% of primary energy",
    color = NULL,
    caption = "Source: Our World in Data, Energy Institute"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position   = "right",
    plot.title        = element_text(face = "bold"),
    panel.grid.minor  = element_blank()
  )

#
#
#
#| echo: false
#| warning: false
#| message: false

data2 <- 
data_hcpi_ameco %>% 
  group_by(Country) %>% 
  mutate(
    inflation_rate = c(NA,diff(log(value)))
  ) %>% 
  ungroup()

data2 %>% 
  filter(original_period %in% c(2005:2025)) %>% 
  ggplot(aes(x = original_period, y = inflation_rate, color = Country, group = Country))+
  geom_line()+
  geom_point()

#
#
#
#
#
#| echo: false
#| warning: false
#| message: false


library(tidyverse)
hicp_admin_prices <- read_csv("hicp_admin_prices.csv") 

hicp_admin_prices %>% 
  filter(geo %in% c("Switzerland", "European Union - 27 countries (from 2020)") & coicop18 %in% c("Administered prices", "Fully administered prices", "Mainly administered prices", "Administered prices - energy and food") & TIME_PERIOD %in% c(2015:2025)) %>% 
  ggplot(aes(x = TIME_PERIOD, y = OBS_VALUE/10, color = geo, shape = coicop18))+
  geom_line()+
  geom_point()+
  facet_wrap(~geo)
#
#
#
#
#
#
