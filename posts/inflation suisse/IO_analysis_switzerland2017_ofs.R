library(tidyverse)
library(readxl)
library(ggrepel)

# ============================================================
# Data import
# ============================================================
# Sheet "siot" = symmetric input-output table at basic prices
# Rows 6-56 (in the raw tibble) = 51 industry sectors
# Cols 3-53  = inter-industry Z matrix
# Col  67    = total household final consumption (HFCE)
# Row  61    = total output by sector (x)

siot_raw <- read_excel("IO_table_ofs_2017.xlsx", sheet = "siot", col_names = TRUE)

n <- 51   # number of sectors

sector_codes <- siot_raw[[1]][6:56]   # NACE codes
sector_names <- siot_raw[[2]][6:56]   # full sector names

# Inter-industry flow matrix Z (51 × 51)
Z <- siot_raw[6:56, 3:53] |>
  mutate(across(everything(), as.numeric)) |>
  as.matrix()
rownames(Z) <- sector_codes
colnames(Z) <- sector_codes

# Total output vector x
x <- siot_raw[61, 3:53] |> unlist() |> as.numeric()

# Household final consumption by sector (HFCE weights)
hfce <- siot_raw[6:56, 67] |> unlist() |> as.numeric()


# ============================================================
# Leontief quantity model
# ============================================================
# Technical coefficient matrix A = Z * x_hat^{-1}
A <- Z %*% diag(1 / x, ncol = n)

# Leontief inverse L = (I - A)^{-1}
L <- solve(diag(n) - A)
rownames(L) <- sector_codes
colnames(L) <- sector_codes

# Forward and backward linkages
forward_linkages  <- rowSums(L)
backward_linkages <- colSums(L)

linkages <- tibble(
  sector      = sector_codes,
  sector_name = sector_names,
  forward     = forward_linkages,
  backward    = backward_linkages
)


# ============================================================
# Leontief price model
# ============================================================
# Price model: p = (I - A')^{-1} v = t(L) v
# Column j of t(L) = price changes in all sectors from a 100%
# shock originating in sector j.

L_price <- t(L)
rownames(L_price) <- sector_codes
colnames(L_price) <- sector_codes


# ============================================================
# Synthetic CPI impact
# ============================================================
# Weights = HFCE shares; CPI_j = w' %*% L_price[, j]

w          <- hfce / sum(hfce)
cpi_impact <- as.numeric(t(w) %*% L_price)

cpi_results <- tibble(
  sector      = sector_codes,
  sector_name = sector_names,
  cpi_impact  = cpi_impact,
  hfce_share  = w
) |>
  arrange(desc(cpi_impact))

cpi_results


# ============================================================
# Chart 1: CPI impact by sector (bar chart)
# ============================================================
cpi_plot <- cpi_results |>
  filter(cpi_impact >= 0.01) |>
  ggplot(aes(x = cpi_impact,
             y = reorder(sector_name, cpi_impact))) +
  geom_col() +
  labs(
    title    = "CPI impact of a 100% price increase by sector",
    subtitle = "Switzerland 2017 — weighted by HFCE shares",
    x        = "Synthetic CPI change",
    y        = NULL
  ) +
  theme_minimal(base_size = 11)

cpi_plot

ggsave("IO_cpi_impact_ofs2017.png", cpi_plot,
       width = 10, height = 8, dpi = 300)


# ============================================================
# Chart 2: Normalised forward/backward linkage quadrant
# ============================================================
linkages_data <- linkages |>
  mutate(
    bl_norm = backward / mean(backward),
    fl_norm = forward  / mean(forward)
  ) |>
  left_join(select(cpi_results, sector, cpi_impact, hfce_share), by = "sector")

# Sectors to label: top 10 by CPI impact + retail (47) + real estate (68)
top_sectors <- cpi_results |> slice_max(cpi_impact, n = 10) |> pull(sector)
label_sectors <- union(top_sectors, c("47", "68"))

linkages_plot <- linkages_data |>
  mutate(label = if_else(sector %in% label_sectors, sector_name, NA_character_)) |>
  ggplot(aes(x = bl_norm, y = fl_norm,
             size = hfce_share, label = label)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey60") +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey60") +
  geom_point(alpha = 0.6) +
  geom_text_repel(size = 3, max.overlaps = 20, na.rm = TRUE) +
  scale_size_continuous(range = c(1, 8), name = "CPI weight (HFCE share)") +
  labs(
    title    = "",
    subtitle = "",
    x        = "Backward linkage (normalised)",
    y        = "Forward linkage (normalised)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

linkages_plot

ggsave("IO_linkages_ofs2017.png", linkages_plot,
       width = 10, height = 7, dpi = 300)



library(readr)
hicp_switzerland <- read_csv("hicp_switzerland.csv")


# ============================================================
# Price volatility from HICP
# ============================================================
# Pivot to long form and extract COICOP codes
hicp_long <- hicp_switzerland |>
  pivot_longer(-period, names_to = "label", values_to = "index") |>
  mutate(
    coicop     = str_extract(label, "M\\.I15\\.([A-Z0-9]+)\\.CH", group = 1),
    description = str_extract(label, "– (.+?) – Switzerland", group = 1),
    date       = as.Date(paste0(period, "-01"))
  )

# Volatility = sd of 12-month YoY % changes, at 2-digit COICOP level (CP01–CP12)
coicop_vol <- hicp_long |>
  filter(str_detect(coicop, "^CP\\d{2}$")) |>
  group_by(coicop, description) |>
  arrange(date) |>
  mutate(yoy = (index / lag(index, 12) - 1) * 100) |>
  summarise(vol_pct = sd(yoy, na.rm = TRUE), .groups = "drop")

# ============================================================
# NACE → COICOP mapping
# ============================================================
# Each IO sector is matched to the COICOP division whose price
# dynamics best reflect that sector's output price behaviour.
nace_coicop_map <- tribble(
  ~sector_code,   ~coicop,
  "01",           "CP01",   # Agriculture → Food
  "02",           "CP05",   # Forestry → Furnishings (wood)
  "03",           "CP01",   # Fishing → Food
  "05 - 09",      "CP04",   # Mining → Housing/energy
  "10 - 12",      "CP01",   # Food & tobacco manuf → Food
  "13 - 15",      "CP03",   # Textiles → Clothing
  "16",           "CP05",   # Wood products → Furnishings
  "17",           "CP09",   # Paper → Recreation/culture
  "18",           "CP09",   # Printing → Recreation/culture
  "19 - 20",      "CP04",   # Chemicals/coke → Housing/energy
  "21",           "CP06",   # Pharma → Health
  "22",           "CP05",   # Rubber/plastic → Furnishings
  "23",           "CP04",   # Non-metallic minerals → Housing
  "24",           "CP07",   # Basic metals → Transport
  "25",           "CP05",   # Fabricated metals → Furnishings
  "26",           "CP09",   # Electronics → Recreation/culture
  "27",           "CP05",   # Electrical equip → Furnishings
  "28",           "CP05",   # Machinery → Furnishings
  "29",           "CP07",   # Motor vehicles → Transport
  "30",           "CP07",   # Other transport equip → Transport
  "31",           "CP05",   # Furniture → Furnishings
  "32",           "CP12",   # Other manufacturing → Misc
  "33",           "CP07",   # Repair machinery → Transport
  "35",           "CP04",   # Electricity/gas → Housing/energy
  "36 - 39",      "CP04",   # Water/waste → Housing
  "41 - 43",      "CP04",   # Construction → Housing
  "45",           "CP07",   # Motor vehicle trade → Transport
  "46",           "CP00",   # Wholesale → All-items (proxy)
  "47",           "CP00",   # Retail → All-items (proxy)
  "49 - 51",      "CP07",   # Transport services → Transport
  "52",           "CP07",   # Warehousing → Transport
  "53",           "CP08",   # Postal → Communications
  "55",           "CP11",   # Accommodation → Restaurants/hotels
  "56",           "CP11",   # Food & beverage services → Restaurants/hotels
  "58 - 60",      "CP09",   # Publishing/media → Recreation/culture
  "61",           "CP08",   # Telecom → Communications
  "62 - 63",      "CP08",   # IT services → Communications
  "64",           "CP12",   # Financial services → Misc
  "65",           "CP12",   # Insurance → Misc
  "68",           "CP04",   # Real estate → Housing
  "69 - 71",      "CP12",   # Professional services → Misc
  "72",           "CP12",   # R&D → Misc
  "73 - 75",      "CP12",   # Other professional → Misc
  "77 - 82",      "CP12",   # Admin support → Misc
  "84",           "CP00",   # Public admin → All-items (proxy)
  "85",           "CP10",   # Education → Education
  "86",           "CP06",   # Health → Health
  "87 - 88",      "CP06",   # Residential care → Health
  "90 - 93",      "CP09",   # Arts/entertainment → Recreation/culture
  "94 - 96",      "CP12",   # Other services → Misc
  "97 - 98",      "CP00"    # Households → All-items (proxy)
)

# ============================================================
# Leontief price model — volatility shocks
# ============================================================
shock_df <- tibble(sector_code = sector_codes) |>
  left_join(nace_coicop_map, by = "sector_code") |>
  left_join(select(coicop_vol, coicop, vol_pct), by = "coicop") |>
  mutate(shock = vol_pct / 100)   # convert % → fraction

shock_vec <- shock_df$shock

# Per-sector: CPI impact (pp) = (w' L_price[,j]) * shock_j * 100
# Interpretation: by how many pp would the synthetic CPI change
# if sector j experiences its historical price volatility?
cpi_unit <- as.numeric(t(w) %*% L_price)
cpi_vol  <- cpi_unit * shock_vec * 100   # in percentage points

cpi_vol_results <- tibble(
  sector          = sector_codes,
  sector_name     = sector_names,
  coicop          = shock_df$coicop,
  vol_pct         = shock_df$vol_pct,
  cpi_sensitivity = cpi_unit,
  cpi_vol_impact  = cpi_vol        # pp impact given actual volatility
) |>
  arrange(desc(cpi_vol_impact))

cpi_vol_results


# ============================================================
# Chart 3: CPI impact — volatility-weighted, decomposed
# ============================================================
# Direct effect:   w_j × σ_j          (household buys directly from sector j)
# Indirect effect: (w'L[:,j] − w_j) × σ_j  (supply-chain propagation)

direct_unit   <- w
indirect_unit <- cpi_unit - w     # always >= 0 since L diagonal >= 1

cpi_decomp <- tibble(
  sector      = sector_codes,
  sector_name = sector_names
) |>
  left_join(select(shock_df, sector_code, coicop, vol_pct),
            by = c("sector" = "sector_code")) |>
  mutate(
    shock           = vol_pct / 100,
    direct_impact   = direct_unit   * shock * 100,
    indirect_impact = indirect_unit * shock * 100,
    total_impact    = direct_impact + indirect_impact
  )

# Long format for stacked bars; filter to visible sectors
plot_data <- cpi_decomp |>
  filter(total_impact >= 0.05) |>
  mutate(sector_label = reorder(sector_name, total_impact))

# Scale factor to overlay vol_pct dots on the bar axis
scale_f <- max(plot_data$total_impact) / max(plot_data$vol_pct)

plot_long <- plot_data |>
  select(sector_label, vol_pct, direct_impact, indirect_impact) |>
  pivot_longer(c(direct_impact, indirect_impact),
               names_to = "effect", values_to = "impact") |>
  mutate(effect = recode(effect,
    direct_impact   = "Direct",
    indirect_impact = "Indirect"
  ))

cpi_vol_plot <- ggplot(plot_long,
                       aes(x = impact, y = sector_label, fill = effect)) +
  geom_col() +
  geom_point(
    data        = distinct(plot_long, sector_label, vol_pct),
    aes(x = vol_pct * scale_f, y = sector_label),
    colour      = "forestgreen", size = 3,
    inherit.aes = FALSE
  ) +
  scale_x_continuous(
    name     = "CPI impact",
    sec.axis = sec_axis(~ . / scale_f,
                        name = "Price volatility")
  ) +
  scale_fill_manual(
    values = c("Direct" = "gold",
               "Indirect" = "darkblue"),
    name = NULL
  ) +
  labs(
    title    = "",
    subtitle = "",
    y        = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

cpi_vol_plot

ggsave("IO_cpi_vol_impact_ofs2017.png", cpi_vol_plot,
       width = 10, height = 8, dpi = 300)


# ============================================================
# Sub-period comparison: Until 2019 / 2020–2021 / 2021–2022
# ============================================================

# Helper: sd of YoY changes for a given range of calendar years
vol_for_period <- function(start_yr, end_yr, label) {
  hicp_long |>
    filter(str_detect(coicop, "^CP\\d{2}$")) |>
    arrange(coicop, date) |>
    group_by(coicop) |>
    mutate(yoy = (index / lag(index, 12) - 1) * 100) |>
    ungroup() |>
    filter(year(date) >= start_yr, year(date) <= end_yr) |>
    group_by(coicop) |>
    summarise(vol_pct = sd(yoy, na.rm = TRUE), .groups = "drop") |>
    mutate(period = label)
}

period_vol3 <- bind_rows(
  vol_for_period(2005, 2019, "Until 2019"),
  vol_for_period(2020, 2021, "2020\u20132021"),
  vol_for_period(2021, 2022, "2021\u20132022")
)

# Helper: run Leontief price simulation for one period label
run_simulation <- function(period_label) {
  vol <- period_vol3 |>
    filter(period == period_label) |>
    select(coicop, vol_pct)

  tibble(sector_code = sector_codes, sector_name = sector_names) |>
    left_join(nace_coicop_map, by = "sector_code") |>
    left_join(vol, by = "coicop") |>
    mutate(
      shock           = vol_pct / 100,
      direct_impact   = direct_unit   * shock * 100,
      indirect_impact = indirect_unit * shock * 100,
      total_impact    = direct_impact + indirect_impact,
      period          = period_label
    )
}

all_periods <- bind_rows(
  run_simulation("Until 2019"),
  run_simulation("2020\u20132021"),
  run_simulation("2021\u20132022")
) |>
  mutate(period = factor(period,
                         levels = c("Until 2019", "2020\u20132021", "2021\u20132022")))

top10 <- all_periods |>
  group_by(period) |>
  slice_max(total_impact, n = 10) |>
  ungroup()

# ── Shared helpers ─────────────────────────────────────────
shorten <- function(x) x |>
  str_replace("Manufacture of ", "Mfr. of ") |>
  str_replace("and air-conditioning supply", "& AC") |>
  str_replace(", except of motor vehicles and motorcycles", "") |>
  str_replace(" and transport via pipelines", "") |>
  str_replace(", trailers and semi-trailers", "") |>
  str_replace(", architecture, engineering activities", "...") |>
  str_trunc(48)

make_panel <- function(period_label, show_legend = FALSE) {
  d <- top10 |>
    filter(period == period_label) |>
    mutate(sector_short = shorten(sector_name)) |>
    pivot_longer(c(direct_impact, indirect_impact),
                 names_to = "effect", values_to = "impact") |>
    mutate(
      effect = factor(recode(effect,
        direct_impact   = "Direct (HFCE weight)",
        indirect_impact = "Indirect (supply-chain)"
      ), levels = c("Indirect (supply-chain)", "Direct (HFCE weight)")),
      sector_short = reorder(sector_short, total_impact)
    )

  ggplot(d, aes(x = impact, y = sector_short, fill = effect)) +
    geom_col() +
    scale_fill_manual(
      values = c("Direct (HFCE weight)"    = "#2c3e50",
                 "Indirect (supply-chain)" = "#95a5a6"),
      name = NULL
    ) +
    labs(title = period_label,
         x = "Synthetic CPI change (pp)", y = NULL) +
    theme_minimal(base_size = 10) +
    theme(plot.title      = element_text(face = "bold", size = 11),
          legend.position = if (show_legend) "bottom" else "none")
}

# ============================================================
# Chart 4: Top-10 per period — stacked panels
# ============================================================
library(patchwork)

p1 <- make_panel("Until 2019")
p2 <- make_panel("2020\u20132021")
p3 <- make_panel("2021\u20132022", show_legend = TRUE)

period_chart <- (p1 / p2 / p3) +
  plot_annotation(
    title    = "Top-10 sectors by synthetic CPI impact \u2014 three volatility periods",
    subtitle = "Switzerland 2017 IO table | shock = YoY price volatility (sd) per COICOP proxy",
    theme    = theme(plot.title    = element_text(size = 12, face = "bold"),
                     plot.subtitle = element_text(size = 9, colour = "grey40"))
  )

period_chart

ggsave("IO_cpi_periods_ofs2017.png", period_chart,
       width = 11, height = 13, dpi = 300)


hicp_long |> 
  ggplot(aes(x = date, y = index, color = description))+
  geom_line()



















